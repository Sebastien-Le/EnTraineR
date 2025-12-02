# R/trainer_LinearModel.R

#' Trainer: Interpret FactoMineR::LinearModel with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to interpret a
#' FactoMineR::LinearModel result. Handles model selection (AIC/BIC) and
#' instructs how to interpret deviation contrasts (sum-to-zero) for factors.
#' Works for ANOVA, ANCOVA, and multiple regression.
#'
#' @param lm_obj An object returned by FactoMineR::LinearModel(...).
#' @param introduction Optional character string giving the study context.
#' @param alpha Numeric significance level (default 0.05).
#' @param t_test Optional character vector to filter the T-test section by
#'   factor names and/or interactions (e.g. "FactorA" or "FactorA:FactorB").
#' @param audience One of c("beginner","applied","advanced").
#' @param summary_only Logical; if TRUE, return a 3-bullet executive summary.
#' @param llm_model Character model name for the generator (e.g., "llama3").
#' @param generate Logical; if TRUE, call the generator.
#'
#' @return Character prompt or list.
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
  if (is.null(lm_obj)) stop("lm_obj cannot be NULL.")

  # --- Capture output --------------------------------------------------------
  lm_txt <- if (is.character(lm_obj)) paste(lm_obj, collapse = "\n") else trainer_core_capture(lm_obj)
  lines  <- unlist(strsplit(lm_txt, "\n", fixed = TRUE), use.names = FALSE)

  # --- Detect selection context ----------------------------------------------
  pat_complete <- "^\\s*Results for the complete model:"
  pat_selected <- "^\\s*Results for the model selected by .* criterion:"
  has_complete <- any(grepl(pat_complete, lines, perl = TRUE))
  has_selected <- any(grepl(pat_selected, lines, perl = TRUE))

  # Extract blocks by headers (robust to order)
  extract_block <- function(lines, start_pat) {
    idx <- grep(start_pat, lines, perl = TRUE)
    if (!length(idx)) return(character(0))
    start <- idx[1]
    nxt <- grep("^\\s*Results for the (complete model|model selected by .* criterion):", lines, perl = TRUE)
    nxt <- nxt[nxt > start]
    end <- if (length(nxt)) nxt[1] - 1L else length(lines)
    lines[start:end]
  }

  complete_block <- if (has_complete) extract_block(lines, pat_complete) else character(0)
  selected_block <- if (has_selected) extract_block(lines, pat_selected) else lines

  # Use selected-block text for F/T tables when available
  scope_txt <- paste(selected_block, collapse = "\n")

  # --- Extract Ftest and Ttest (Standard + Heuristic Fallback) ---------------
  ftest_lines <- trainer_core_extract_block_after(scope_txt, "Ftest")
  ttest_lines <- trainer_core_extract_block_after(scope_txt, "Ttest")

  if (!length(ftest_lines) && !length(ttest_lines)) {
    blocks <- trainer_core_extract_tables_heuristic(scope_txt)
    ftest_lines <- trimws(blocks$ftest_lines, which = "both")
    ttest_lines <- trimws(blocks$ttest_lines, which = "both")
    if (!length(ftest_lines) && !length(ttest_lines) && length(selected_block) > 0) {
      ftest_lines <- selected_block
    }
  }

  # Trim trailing "Signif. codes" in F-test
  if (length(ftest_lines)) {
    sig_idx <- grep("Signif\\. codes:", ftest_lines, perl = TRUE)
    if (length(sig_idx)) ftest_lines <- ftest_lines[seq_len(sig_idx[1] - 1L)]
  }

  # Detect T-test header for potential re-attachment
  ttest_header <- character(0)
  if (length(ttest_lines)) {
    idx_header <- which(
      grepl("\\bEstimate\\b", ttest_lines, perl = TRUE) |
        grepl("Std\\.?\\s*Error", ttest_lines, perl = TRUE) |
        grepl("Pr\\(>\\|?t\\|?\\)", ttest_lines, perl = TRUE)
    )
    if (length(idx_header)) ttest_header <- ttest_lines[min(idx_header)]
  }

  # --- T-test filtering (space-safe; mains + interactions) -------------------
  t_req <- if (!is.null(t_test) && length(t_test) && any(nzchar(t_test))) trimws(as.character(t_test)) else character(0)
  req_main  <- t_req[!grepl(":", t_req, fixed = TRUE)]
  req_inter <- t_req[ grepl(":", t_req,  fixed = TRUE)]
  requested <- if (length(t_req)) c(req_main, req_inter) else NULL

  ttest_filtered <- trainer_core_filter_ttest_by_factors(
    tt_lines       = ttest_lines,
    keep_factors   = requested,      # NULL -> keep all
    keep_intercept = TRUE
  )

  is_filtered   <- !is.null(requested)
  ttest_to_show <- ttest_filtered

  # Re-attach header if missing after filtering
  if (is_filtered && length(ttest_header) && length(ttest_to_show)) {
    first_has_header <- grepl("\\bEstimate\\b", ttest_to_show[1], perl = TRUE) ||
      grepl("Std\\.?\\s*Error", ttest_to_show[1], perl = TRUE) ||
      grepl("Pr\\(>\\|?t\\|?\\)", ttest_to_show[1], perl = TRUE)
    if (!first_has_header) ttest_to_show <- c(ttest_header, ttest_to_show)
  }

  # Which requested items are actually present
  actually_shown <- trainer_core_actually_shown(req_main, req_inter, ttest_filtered)

  # --- Audience profile & header ---------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # Default introduction: generic across ANOVA, ANCOVA, multiple regression
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We interpret a linear model (ANOVA, ANCOVA, or multiple regression). Factors use sum-to-zero contrasts; the Intercept is the global mean for factors. Numeric predictors enter as slopes."
  }

  # --- Output requirements (audience-specific or summary-only) ---------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(50, 3, "Linear Model")
  } else {
    a <- trainer_core_fmt_alpha(alpha)
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "1) Model fit: R-squared and global F (plain language).\n",
        "2) Terms that matter: which terms are significant from the F-tests.\n",
        "3) Coefficients: for significant factors, indicate which levels are above or below the global mean (sign of coefficients; Intercept = global mean). For numeric predictors, describe direction of the slope.\n",
        "4) Conclusion: one sentence.\n",
        "5) Use only printed values; do not compute new statistics or effect sizes."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) Fit: R2, RSE, global significance; adequacy for decisions.\n",
        "2) Drivers (F-test): significant terms and practical implications.\n",
        "3) Profiling (coefficients): factors are deviations around the global mean (Intercept); numeric predictors are slopes per unit change.\n",
        "4) Selection: note if a Selected Model exists (AIC/BIC) and refer to printed metrics only.\n",
        "5) Actionable synthesis, concise.\n",
        "6) Use only printed values; do not introduce unprinted statistics or diagnostics."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) Diagnostics: R2/Adj-R2, RSE, F-statistic; selection gain (Delta AIC/Delta BIC) if present.\n",
        "2) Significance: partial (added-last) F-tests; hierarchy with interactions.\n",
        "3) Coefficients: for factors, sum-to-zero contrasts; interpret as mu_i - mu (Intercept = global mean). For numeric predictors, interpret slopes with SE and p-values. Respect printed values only.\n",
        "4) Strictly use printed values; do not derive unprinted statistics."
      )
    )
  }

  # --- How-to-read (generic for ANOVA, ANCOVA, regression) -------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (LinearModel)",
      "- Global fit: R-squared indicates how much variation is explained.",
      "- F-tests: show whether each term (factor or numeric predictor) affects the response.",
      "- Coefficients:",
      "  - For factors: Intercept is the global mean; coefficients are deviations around this mean (positive = above, negative = below).",
      "  - For numeric predictors: coefficients are slopes (change in the response per 1-unit increase).",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (LinearModel)",
      "- Fit: check R2 and residual error (RSE).",
      "- Partial F-tests: contribution of each term at alpha.",
      "- Coefficients (deviation coding for factors, slopes for numeric predictors): level - global mean for factors; slope per unit change for numeric.",
      "- Selection: if present, reflects AIC/BIC optimization; compare printed metrics only.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (LinearModel)",
      "- Factors use contr.sum (sum-to-zero). Coefficients test H0: mu_i - mu = 0; Intercept equals the global mean.",
      "- Numeric predictors enter linearly as slopes; interpret with SE and p-values as printed.",
      "- Compare complete vs selected models when both are printed; observe hierarchy (Type II/III logic) when interactions are present, including factor:covariate interactions.",
      sep = "\n"
    )
  )

  # --- Selection note ---------------------------------------------------------
  selection_msg <- if (has_selected) {
    paste(
      "**Note:** A variable selection procedure (AIC/BIC) was applied.",
      "The results below refer to the Selected Model.",
      if (has_complete) "If both Complete and Selected models are printed, compare their fit metrics to assess any improvement."
    )
  } else {
    "No variable selection was applied (standard model)."
  }

  # --- De-dup: strip Ftest/Ttest from Model Metrics blocks -------------------
  strip_tables <- function(xlines) {
    if (!length(xlines)) return(xlines)
    cut <- grep("^\\s*(Ftest|Ttest)\\b", xlines, perl = TRUE)
    if (length(cut)) {
      idx <- min(cut)
      if (idx > 1L) return(xlines[seq_len(idx - 1L)])
      return(character(0))
    }
    xlines
  }

  complete_metrics_only <- strip_tables(complete_block)
  selected_metrics_only <- strip_tables(selected_block)

  # Fallback: if stripping yields empty, keep original (rare)
  if (!length(complete_metrics_only) && length(complete_block)) complete_metrics_only <- complete_block
  if (!length(selected_metrics_only) && length(selected_block)) selected_metrics_only <- selected_block

  # --- Compose setup (use core wrappers; no duplicated F/T) ------------------
  block_metrics_complete <- if (has_complete) trainer_core_wrap_block("#### Complete Model", complete_metrics_only) else NULL
  block_metrics_selected <- trainer_core_wrap_block(if (has_selected) "#### Selected Model" else "", selected_metrics_only)
  block_ftest            <- trainer_core_wrap_block("", ftest_lines)
  block_ttest            <- trainer_core_wrap_block("", if (length(ttest_to_show)) ttest_to_show else "(no T-test section detected)")

  ttest_title     <- if (is_filtered) "### T-test (filtered)" else "### T-test"
  ttest_scope_msg <- trainer_core_ttest_scope_msg(t_req, requested, actually_shown)

  setup <- paste(
    paste0("- Significance threshold: p <= ", format(alpha), "."),
    "- Model type: FactoMineR::LinearModel (ANOVA, ANCOVA, or multiple regression).",
    "- Coding: factors use sum-to-zero contrasts (Intercept = global mean); numeric predictors are slopes.",
    selection_msg,
    "",
    howto_block,
    "",
    if (has_selected) "### Model Metrics (Comparison)" else "### Model Metrics",
    trainer_core_collapse(c(block_metrics_complete, block_metrics_selected)),
    "",
    "### F-test (ANOVA table)",
    block_ftest,
    "",
    ttest_title,
    ttest_scope_msg,
    block_ttest,
    sep = "\n"
  )

  # --- Build final prompt -----------------------------------------------------
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = "",
    output_requirements = output_reqs,
    show_verbatim       = FALSE
  )

  trainer_core_generate_or_return(prompt, llm_model, generate)
}
