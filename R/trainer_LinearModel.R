# R/trainer_LinearModel.R

#' Trainer: Interpret FactoMineR::LinearModel with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to interpret a
#' FactoMineR::LinearModel result. Handles selection = "aic", "bic",
#' or "none". Never invents numbers: passes verbatim output and instructs
#' interpretation only.
#'
#' @param lm_obj An object returned by FactoMineR::LinearModel(...).
#' @param introduction Optional character string giving the study context.
#'   Defaults to a generic description.
#' @param alpha Numeric significance level used as instruction for the LLM
#'   (no computation is performed). Default 0.05.
#' @param t_test Optional character vector to filter the T-test section by
#'   factor names and/or interactions expressed as "A:B". If NULL, all rows
#'   are shown.
#' @param audience One of c("beginner","applied","advanced").
#' @param summary_only Logical; if TRUE, return a 3-bullet executive summary
#'   regardless of audience depth.
#' @param llm_model Character model name for the generator (e.g., "llama3").
#' @param generate Logical; if TRUE, call trainer_core_generate_or_return()
#'   and return a list with prompt, response, and model. If FALSE, return
#'   the prompt string.
#'
#' @return Character prompt (if generate = FALSE) or a list with
#'   prompt, response, and model.
#'
#' @examples
#' # --- Example 1: multiple regression with selection (ham) -------------------
#' data(ham)
#' if (requireNamespace("FactoMineR", quietly = TRUE)) {
#'   intro_ham <- "A sensory analysis institute wants to know if it's possible to predict
#'   the overall liking of a ham from its sensory description.
#'   A trained panel used the following attributes to describe 21 hams:
#'   Juiciness, Crispy, Tenderness, Pasty, Fibrous, Salty, Sweet, Meaty,
#'   Seasoned, Metallic, Ammoniated, Fatty, Braised, Lactic.
#'   Afterward, an Overall Liking score was assigned to each of the hams."
#'   # collapse whitespace safely without extra packages
#'   intro_ham <- gsub("\n", " ", intro_ham)
#'   intro_ham <- gsub("\\s+", " ", intro_ham)
#'
#'   res <- FactoMineR::LinearModel(`Overall liking` ~ ., data = ham, selection = "bic")
#'   pr  <- trainer_LinearModel(res, introduction = intro_ham, audience = "advanced",
#'                              generate = FALSE)
#'   cat(pr)
#' }
#'
#' # --- Example 2: interaction with a categorical factor (deforestation) ------
#' data(deforestation)
#' if (requireNamespace("FactoMineR", quietly = TRUE)) {
#'   intro_flume <- "The study's goal is to determine how river deforestation affects
#'   the relationship between water and air temperature.
#'   The dataset contains maximum air and water temperatures measured over
#'   28 ten-day periods before deforestation and 28 periods after deforestation.
#'   The main objective is to understand if and how the link between air and
#'   water temperature changes after deforestation."
#'   intro_flume <- gsub("\n", " ", intro_flume)
#'   intro_flume <- gsub("\\s+", " ", intro_flume)
#'
#'   res <- FactoMineR::LinearModel(Temp_water ~ Temp_air * Deforestation,
#'                                  data = deforestation, selection = "none")
#'   pr  <- trainer_LinearModel(res, introduction = intro_flume, audience = "advanced",
#'                              generate = FALSE)
#'   cat(pr)
#' }
#'
#' @export
trainer_LinearModel <- function(lm_obj,
                                introduction = NULL,
                                alpha = 0.05,
                                t_test = NULL,
                                audience = c("beginner","applied","advanced"),
                                summary_only = FALSE,
                                llm_model = "llama3",
                                generate = FALSE) {
  audience <- match.arg(audience)

  # ---- Local helpers ---------------------------------------------------------
  split_lines <- function(txt) unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)

  extract_section <- function(lines, start_regex) {
    i <- which(grepl(start_regex, lines, perl = TRUE))
    if (!length(i)) return(character(0))
    k <- i[1] + 1L
    if (k <= length(lines) && grepl("^=+$", lines[k])) k <- k + 1L
    out <- character(0)
    while (k <= length(lines)) {
      ln <- lines[k]
      if (grepl("^Results for the complete model:", ln, perl = TRUE)) break
      if (grepl("^Results for the model selected by .* criterion:", ln, perl = TRUE)) break
      if (grepl("^\\s*Ftest\\s*$", ln, perl = TRUE)) break
      if (grepl("^\\s*Ttest\\s*$", ln, perl = TRUE)) break
      out <- c(out, ln); k <- k + 1L
    }
    while (length(out) && grepl("^\\s*$", out[length(out)])) out <- out[-length(out)]
    out
  }

  extract_ftest <- function(lines) trainer_core_extract_block_after(paste(lines, collapse = "\n"), "Ftest")
  extract_ttest <- function(lines) trainer_core_extract_block_after(paste(lines, collapse = "\n"), "Ttest")

  parse_ftest_terms <- function(ft_lines) {
    if (!length(ft_lines)) return(character(0))
    dat_rows <- ft_lines[ grepl("^[^\\s].*\\s\\d", ft_lines) | grepl("^Residuals\\b", ft_lines) ]
    if (!length(dat_rows)) return(character(0))
    terms <- sub("\\s+.*$", "", dat_rows)
    res_pos <- which(terms == "Residuals")
    if (length(res_pos)) terms <- terms[seq_len(res_pos[1])]
    terms[terms != "Residuals"]
  }

  parse_rhs_terms <- function(formula_str) {
    rhs  <- sub(".*~", "", formula_str)
    toks <- unlist(strsplit(rhs, "+", fixed = TRUE), use.names = FALSE)
    toks <- trimws(toks); toks[toks != ""]
  }
  extract_formula_from_call <- function(block_lines) {
    if (!length(block_lines)) return(NA_character_)
    call_idx <- which(grepl("^\\s*LinearModel\\(", block_lines))
    if (!length(call_idx)) return(NA_character_)
    call_line <- block_lines[call_idx[1]]
    k <- call_idx[1] + 1L
    while (k <= length(block_lines) && !grepl("\\)\\s*$", call_line)) {
      call_line <- paste0(call_line, " ", block_lines[k]); k <- k + 1L
    }
    m <- regexpr("formula\\s*=\\s*([^,]+)", call_line, perl = TRUE)
    if (m[1] == -1) return(NA_character_)
    frag <- regmatches(call_line, m)[1]
    sub("^formula\\s*=\\s*", "", frag)
  }

  parse_summary_metrics <- function(block_lines) {
    if (!length(block_lines)) return(list())
    txt <- paste(block_lines, collapse = "\n")
    num <- function(x) ifelse(length(x) && nzchar(x), as.numeric(x), NA_real_)
    rse_m <- regexec("Residual standard error:\\s*([0-9\\.eE+-]+)\\s*on\\s*(\\d+)\\s*degrees", txt, perl = TRUE)
    rse_a <- regmatches(txt, rse_m)[[1]]
    rse   <- if (length(rse_a)) num(rse_a[2]) else NA_real_
    dfres <- if (length(rse_a)) as.integer(rse_a[3]) else NA_integer_
    r2_m  <- regexec("Multiple R-squared:\\s*([0-9\\.eE+-]+)", txt, perl = TRUE)
    r2_a  <- regmatches(txt, r2_m)[[1]]
    r2    <- if (length(r2_a)) num(r2_a[2]) else NA_real_
    adj_m <- regexec("Adjusted R-squared:\\s*([0-9\\.eE+-]+)", txt, perl = TRUE)
    adj_a <- regmatches(txt, adj_m)[[1]]
    r2adj <- if (length(adj_a)) num(adj_a[2]) else NA_real_
    f_m <- regexec("F-statistic:\\s*([0-9\\.eE+-]+)\\s*on\\s*(\\d+)\\s*and\\s*(\\d+)\\s*DF,\\s*p-value:\\s*([0-9\\.eE+<\\- ]+)", txt, perl = TRUE)
    f_a <- regmatches(txt, f_m)[[1]]
    fval <- if (length(f_a)) num(gsub("[^0-9\\.eE+-]", "", f_a[2])) else NA_real_
    dfn  <- if (length(f_a)) as.integer(f_a[3]) else NA_integer_
    dfd  <- if (length(f_a)) as.integer(f_a[4]) else NA_integer_
    pval <- if (length(f_a)) gsub("^\\s+|\\s+$", "", f_a[5]) else NA_character_
    ab_m <- regexec("AIC\\s*=\\s*([\\-0-9\\.eE+]+)\\s+\\s*BIC\\s*=\\s*([\\-0-9\\.eE+]+)", txt, perl = TRUE)
    ab_a <- regmatches(txt, ab_m)[[1]]
    aic  <- if (length(ab_a)) num(ab_a[2]) else NA_real_
    bic  <- if (length(ab_a)) num(ab_a[3]) else NA_real_
    list(rse = rse, dfres = dfres, r2 = r2, r2adj = r2adj,
         f = fval, dfn = dfn, dfd = dfd, p = pval, aic = aic, bic = bic)
  }

  fmt_num   <- function(x, digits = 3) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
  fmt_delta <- function(x) ifelse(any(is.na(x)), "NA", sprintf("%+.3f", x))

  # ---- Capture & parse -------------------------------------------------------
  lm_txt <- trainer_core_capture(lm_obj)
  lines  <- split_lines(lm_txt)

  has_complete <- any(grepl("^Results for the complete model:", lines))
  has_selected <- any(grepl("^Results for the model selected by .* criterion:", lines))

  complete_block <- if (has_complete) extract_section(lines, "^Results for the complete model:") else character(0)
  selected_block <- if (has_selected) extract_section(lines, "^Results for the model selected by .* criterion:") else extract_section(lines, "^Call:")

  complete_met <- parse_summary_metrics(complete_block)
  selected_met <- parse_summary_metrics(selected_block)

  ftest_lines <- extract_ftest(lines)
  ttest_lines <- extract_ttest(lines)

  ftest_terms       <- parse_ftest_terms(ftest_lines)
  interaction_terms <- unique(ftest_terms[grepl(":", ftest_terms)])

  # ---- T-test filtering ------------------------------------------------------
  detected_main <- trainer_core_detect_main_factors(ttest_lines)
  if (is.null(t_test) || length(t_test) == 0L || all(!nzchar(t_test))) {
    req_main <- NULL; req_inter <- NULL
  } else {
    t_req     <- as.character(t_test)
    req_main  <- intersect(t_req[!grepl(":", t_req)], detected_main)
    req_inter <- t_req[grepl(":", t_req)]
  }
  requested <- c(req_main, req_inter)

  ttest_filtered <- trainer_core_filter_ttest_by_factors(
    ttest_lines,
    keep_factors   = if (length(requested)) requested else NULL,
    keep_intercept = TRUE
  )

  # If filtered, ensure the column header is present
  if (length(requested)) {
    hdr_idx <- grep("\\bEstimate\\b\\s+\\bStd\\. Error\\b\\s+\\bt value\\b\\s+\\bPr\\(>\\|t\\|\\)\\b", ttest_lines)
    if (length(hdr_idx)) {
      header_line <- ttest_lines[hdr_idx[1]]
      has_header  <- length(ttest_filtered) && grepl("\\bEstimate\\b", ttest_filtered[1])
      if (!has_header) ttest_filtered <- c(header_line, ttest_filtered)
    }
  }

  actually_shown  <- trainer_core_actually_shown(req_main, req_inter, ttest_filtered)
  ttest_scope_msg <- trainer_core_ttest_scope_msg(t_test, requested, actually_shown)

  # ---- Selection & context messages -----------------------------------------
  complete_formula <- extract_formula_from_call(complete_block)
  selected_formula <- extract_formula_from_call(selected_block)

  kept_dropped_msg <- ""
  if (!is.na(complete_formula) && !is.na(selected_formula)) {
    complete_terms <- parse_rhs_terms(complete_formula)
    selected_terms <- parse_rhs_terms(selected_formula)
    dropped_terms  <- setdiff(complete_terms, selected_terms)
    kept_terms     <- selected_terms
    parts <- c(
      if (length(complete_terms)) paste0("- Complete model RHS: ", paste(complete_terms, collapse = " + ")) else NULL,
      if (length(selected_terms)) paste0("- Selected model RHS: ", paste(selected_terms, collapse = " + ")) else NULL,
      if (length(dropped_terms))  paste0("- Dropped by selection: ", paste(dropped_terms, collapse = ", ")) else NULL,
      if (length(kept_terms))     paste0("- Kept: ", paste(kept_terms, collapse = ", ")) else NULL
    )
    kept_dropped_msg <- paste(parts, collapse = "\n")
  }

  selection_msg <- if (has_selected) {
    crit <- if (any(grepl("^Results for the model selected by AIC criterion:", lines))) "AIC"
    else if (any(grepl("^Results for the model selected by BIC criterion:", lines))) "BIC"
    else "AIC/BIC"

    cmp_lines <- character(0)
    if (length(complete_met) && length(selected_met)) {
      dAIC <- fmt_delta(selected_met$aic  - complete_met$aic)
      dBIC <- fmt_delta(selected_met$bic  - complete_met$bic)
      dRSE <- fmt_delta(selected_met$rse  - complete_met$rse)
      dAdj <- if (!is.na(selected_met$r2adj) && !is.na(complete_met$r2adj)) fmt_delta(selected_met$r2adj - complete_met$r2adj) else "NA"
      dDFR <- if (!is.na(selected_met$dfres) && !is.na(complete_met$dfres)) sprintf("%+d", selected_met$dfres - complete_met$dfres) else "NA"

      cmp_lines <- c(
        "- Evidence of improvement with selection:",
        paste0("  - Delta AIC (Selected - Complete) = ", dAIC, "  (~2 modest, 4-7 substantial, >=10 strong)"),
        paste0("  - Delta BIC (Selected - Complete) = ", dBIC, "  (similar thresholds)"),
        paste0("  - Delta RSE = ", dRSE),
        paste0("  - Delta Adjusted R^2 = ", dAdj),
        paste0("  - Delta Residual df = ", dDFR),
        "  - Post-selection caution: p-values in the selected model can be optimistic."
      )
    }

    paste0(
      "A model selection step (", crit, ") was requested in the complete-model call. ",
      "The F-test and T-test below describe the selected model.\n",
      if (nzchar(kept_dropped_msg)) paste0(kept_dropped_msg, "\n") else "",
      if (length(cmp_lines)) paste(cmp_lines, collapse = "\n") else ""
    )
  } else {
    "No selection requested (selection = \"none\"). The F-test and T-test below describe this single model."
  }

  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We interpret a FactoMineR::LinearModel with possible AIC/BIC selection and sum-to-zero contrasts."
  }

  current_met <- if (has_selected) selected_met else complete_met
  current_fit_line <- if (length(current_met)) {
    paste0(
      "- Current model fit: RSE ~ ", fmt_num(current_met$rse), " (units)",
      ", R^2 = ", fmt_num(current_met$r2),
      if (!is.na(current_met$r2adj)) paste0(", Adjusted R^2 = ", fmt_num(current_met$r2adj)) else "",
      if (!is.na(current_met$f) && !is.na(current_met$dfn) && !is.na(current_met$dfd))
        paste0(", F(", current_met$dfn, ", ", current_met$dfd, ") = ", fmt_num(current_met$f), ", p = ", ifelse(is.na(current_met$p), "NA", current_met$p)) else "",
      if (!is.na(current_met$aic)) paste0(", AIC = ", fmt_num(current_met$aic)) else "",
      if (!is.na(current_met$bic)) paste0(", BIC = ", fmt_num(current_met$bic)) else ""
    )
  } else ""

  all_na_fit <- all(is.na(c(current_met$rse, current_met$r2, current_met$r2adj,
                            current_met$f, current_met$dfn, current_met$dfd,
                            current_met$aic, current_met$bic)))

  # ---- Audience profile & header --------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # ---- “How to read (LinearModel)” block ------------------------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (LinearModel)",
      "- Global model quality (top summary): overall F tests whether all predictors together improve the fit vs a flat line.",
      "- Per-term tests (F/T below) are partial: H0 means the term does not add value once the other predictors are in.",
      "- Quantitative predictors: F and T p-values are equivalent for one coefficient.",
      "- Categorical predictors (sum-to-zero): the Intercept is the grand mean; level coefficients are deviations that sum to 0.",
      "- Interactions: interpret main effects conditionally; slopes/effects change by the interacting factor.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (LinearModel)",
      "- Global fit tells whether the model is useful overall (F, p, RSE, R^2).",
      "- Per-term tests are incremental contributions given the other predictors already in the model.",
      "- Quantitative: F and T match for a single coefficient; report the practical change per unit if units are clear.",
      "- Categorical (sum-to-zero): Intercept = grand mean; levels are deviations (sum to 0).",
      "- Interactions: describe how the slope/mean changes across the other factor; interpret main effects conditionally.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (LinearModel)",
      "- Global F assesses the joint contribution of all predictors; report RSE, R^2/Adj.R^2, F(df1,df2), p, AIC/BIC if printed.",
      "- Per-term tests are partial (added last). F and T coincide for a single numeric coefficient.",
      "- Apparent discrepancy (global sig. but partial not): can arise from shared variance/collinearity/small df; do not assert diagnostics.",
      "- Categorical (deviation/sum-to-zero): Intercept = grand mean; interpret grand mean + deviations without fabricating reconstructed means.",
      "- Interactions: use simple-effects reading based only on printed coefficients.",
      sep = "\n"
    )
  )

  # ---- Output requirements ---------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(50, 3, "LinearModel")
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- Overall fit: explain the global F-test and residual standard error in plain English; include R^2/AIC/BIC if printed.\n",
        "- Per-term tests: for each effect, report F (or T), p-value, and decision at alpha. Say 'adds value once others are in the model'.\n",
        "- Meaning: for significant effects, state direction/magnitude using printed coefficients (no new calculations).\n",
        "- Categorical factors: coefficients are deviations from the grand mean (sum-to-zero). Name which level appears higher/lower.\n",
        "- Interactions (if present): interpret main effects conditionally (simple effects).\n",
        "- Keep sentences short; no invented numbers."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- Fit: report RSE, R^2 (Adj if available), global F and p, AIC/BIC; one practical sentence about adequacy.\n",
        "- Per-term (partial) tests: F(df1, df2) or T, p, alpha decision; phrase as 'incremental contribution'.\n",
        "- Coefficients: translate key coefficients to practical change per unit (if units are clear) using printed values; separate magnitude from significance.\n",
        "- Categorical (sum-to-zero): Intercept = grand mean; level coefficients sum to 0; interpret deviations.\n",
        "- Interactions: describe how the slope/mean changes across the other factor (no new calculations).\n",
        "- Takeaway: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- Fit: RSE (units), R^2/Adj.R^2, global F(df1, df2), p; AIC/BIC if printed; short adequacy comment.\n",
        "- Partial tests: emphasize 'added last' meaning; avoid inventing diagnostics, but you may flag collinearity as a plausible reason for global/partial discrepancy when observed.\n",
        "- Coefficients: interpret on the original scale; for interactions, give simple-effect reading using printed coefficients only.\n",
        "- Categorical coding: deviation (sum-to-zero) contrasts; interpret grand mean + deviations (do not fabricate reconstructed means).\n",
        "- Multiplicity caution when many terms; no invented adjustments. Takeaway <=", profile$max_bullets, " bullets."
      )
    )
  }

  # ---- Compose SETUP (embed verbatim tables) ---------------------------------
  is_filtered <- length(requested) > 0
  ttest_title <- if (is_filtered) "### T-test (filtered)" else "### T-test"
  if (!is_filtered) ttest_scope_msg <- "T-test shows all coefficients."

  # Show both verbatims if selection was used; else show the single model
  fit_block <- if (has_selected) {
    paste(
      "### Complete model (verbatim summary)",
      paste0("```", "\n", trainer_core_collapse(complete_block), "\n", "```"),
      "",
      "### Selected model (verbatim summary)",
      paste0("```", "\n", trainer_core_collapse(selected_block), "\n", "```"),
      sep = "\n"
    )
  } else {
    paste(
      "### Model fit (verbatim summary)",
      if (length(complete_block)) {
        paste0("```", "\n", trainer_core_collapse(complete_block), "\n", "```")
      } else if (length(selected_block)) {
        paste0("```", "\n", trainer_core_collapse(selected_block), "\n", "```")
      } else {
        "(not printed)"
      },
      sep = "\n"
    )
  }

  setup <- paste(
    "- Assume sum-to-zero contrasts: coefficients are deviations from the grand mean; (Intercept) is the grand mean.",
    "",
    fit_block,
    "",
    "### Which model do F-test / T-test describe?",
    selection_msg,
    "",
    howto_block,
    "",
    "### F-test (current model; full table)",
    if (length(interaction_terms)) {
      "Interaction term(s) detected in F-test: interpret main effects conditionally (simple effects) if interactions are significant."
    } else {
      "No interaction term detected in F-test."
    },
    paste0("```", "\n", trainer_core_collapse(ftest_lines), "\n", "```"),
    "",
    ttest_title,
    ttest_scope_msg,
    paste0("```", "\n", trainer_core_collapse(ttest_filtered), "\n", "```"),
    if (!all_na_fit) paste("\n### Current model fit overview", current_fit_line, sep = "\n") else NULL,
    sep = "\n"
  )

  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction %||% "We interpret a FactoMineR::LinearModel with possible AIC/BIC selection and sum-to-zero contrasts.",
    setup               = setup,
    verbatim            = "",
    output_requirements = output_reqs,
    show_verbatim       = FALSE
  )

  trainer_core_generate_or_return(prompt, llm_model, generate)
}
