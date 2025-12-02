#' Interpret a chi-squared test (chisq.test) with an audience-aware LLM prompt
#'
#' @description
#' Builds a clear, audience-tailored prompt to interpret base R stats::chisq.test()
#' results, handling both goodness-of-fit and contingency-table tests.
#' Aligned with other TraineR trainers: no invented numbers; audience-specific guidance.
#'
#' @param csq_obj An htest object returned by stats::chisq.test().
#' @param introduction Optional character string giving the study context.
#' @param alpha Numeric significance level (default 0.05).
#' @param audience One of c("beginner","applied","advanced").
#' @param summary_only Logical; if TRUE, return a 3-bullet executive summary
#'   regardless of audience depth (uses trainer_core_summary_only_block()).
#' @param llm_model Character; model name for the generator (default "llama3").
#' @param generate Logical; if TRUE, call the generator and return prompt + response.
#'
#' @return If generate = FALSE, a prompt string. If TRUE, a list with
#'   prompt, response, and model.
#' @examples
#' # GOF
#' set.seed(1); x <- c(18, 22, 20, 25, 15)
#' csq1 <- chisq.test(x, p = rep(1/5, 5))
#' cat(trainer_chisq_test(csq1, audience = "beginner"))
#'
#' # Contingency (independence)
#' tbl <- matrix(c(12,5,7,9), nrow=2)
#' csq2 <- chisq.test(tbl)  # Yates for 2x2 by default
#' cat(trainer_chisq_test(csq2, audience = "applied"))
#' @export
trainer_chisq_test <- function(csq_obj,
                               introduction = NULL,
                               alpha = 0.05,
                               audience = c("beginner","applied","advanced"),
                               summary_only = FALSE,
                               llm_model = "llama3",
                               generate = FALSE) {

  audience <- match.arg(audience)
  if (is.null(csq_obj) || !inherits(csq_obj, "htest"))
    stop("csq_obj must be an 'htest' from base R chisq.test().")

  # --- Audience profile & header ---------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # --- Verbatim output --------------------------------------------------------
  csq_txt <- trainer_core_capture(csq_obj)

  # --- Fields & branch detection ---------------------------------------------
  method    <- csq_obj$method %||% ""
  data_name <- csq_obj$data.name %||% ""
  observed  <- csq_obj$observed %||% NULL

  branch <- if (!is.null(observed)) {
    if (is.matrix(observed) && nrow(observed) > 1L && ncol(observed) > 1L) "contingency" else "gof"
  } else if (grepl("given probabilities", tolower(method))) {
    "gof"
  } else {
    "contingency"
  }

  has_yates <- grepl("continuity correction", tolower(method))
  has_mc    <- grepl("simulated p-value", tolower(method))

  # Monte Carlo replicates, if printed
  B_str <- NA_character_
  if (has_mc) {
    mB <- regexec("based on\\s*([0-9]+)\\s*replicates", tolower(method))
    aB <- regmatches(tolower(method), mB)[[1]]
    if (length(aB) >= 2) B_str <- aB[2]
  }

  # --- Default introduction ---------------------------------------------------
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- if (identical(branch, "gof")) {
      "We test whether the observed category counts fit a specified probability model."
    } else {
      "We test whether two categorical variables are independent (no association)."
    }
  }

  # --- How-to-read block ------------------------------------------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (Chi-squared test)",
      if (identical(branch, "gof"))
        "- **Goodness-of-fit**: does the observed distribution match the target probabilities?"
      else
        "- **Independence**: are the row/column variables unrelated in the population?",
      "- The **p-value** comes from a chi-squared reference (or simulation if stated).",
      "- **Residuals** show which cells differ most from the null (look for large positive/negative values if printed).",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (Chi-squared test)",
      if (identical(branch, "gof"))
        "- GOF: compare observed counts to expected under the specified probabilities."
      else
        "- Independence: compare observed vs expected under no association.",
      "- For **2x2 tables**, Yates' continuity correction may be applied (as printed).",
      if (has_mc) "- With **Monte Carlo** p-values, df may be NA; p is based on simulated tables."
      else "- Asymptotic chi-squared p-value by default.",
      "- Inspect **standardized residuals** (if printed) to see which cells drive the result.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (Chi-squared test)",
      "- Asymptotics assume adequate expected counts (rule of thumb: many cells around >= 5).",
      "- 2x2: Yates' correction can be conservative; Fisher's exact is an alternative for sparse data (do not compute here).",
      "- Monte Carlo (if used) simulates tables with fixed margins; df may be NA; report replicates only if printed.",
      "- Use printed (standardized) residuals for cell-wise diagnostics; do not fabricate effect sizes.",
      sep = "\n"
    )
  )

  # --- Setup (orientation) ----------------------------------------------------
  setup_lines <- c(
    paste0("- Significance threshold: p <= ", format(alpha), "."),
    paste0("- Test type: ", if (nzchar(method)) method else "Pearson's Chi-squared Test for Count Data", "."),
    paste0("- Data: ", if (nzchar(data_name)) data_name else "as named in the output"),
    if (identical(branch, "contingency"))
      "- Null: row and column variables are independent."
    else
      "- Null: probabilities equal the specified (or equal) target.",
    if (has_yates) "- Note: continuity correction applied (2x2)." else NULL,
    if (has_mc) paste0("- Note: Monte Carlo p-value", if (!is.na(B_str)) paste0(" (", B_str, " replicates)") else "") else NULL,
    "",
    howto_block
  )
  setup <- paste(setup_lines, collapse = "\n")

  # --- Output requirements ----------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    label <- if (identical(branch, "gof")) "Chi-squared (GOF)" else "Chi-squared (independence)"
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = label
    )
  } else {
    a <- trainer_core_fmt_alpha(alpha)

    common_items <- paste0(
      "- **Report**: X^2", if (isTRUE(profile$include_df)) "(df)" else "",
      ", p-value, and decision at alpha = ", a, ".\n",
      "- **Drivers**: if residuals/standardized residuals are printed, name the cells with the largest departures (no new calculations)."
    )

    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- **What was tested**: ",
        if (identical(branch, "gof")) "does the observed distribution match the target probabilities?\n"
        else "are the two variables independent?\n",
        common_items, "\n",
        "- **Plain meaning**: say whether counts look different from expectation and where (using printed residuals).\n",
        "- **Style**: short sentences; do not compute new statistics.\n\n",
        "**Return only a short explanation (5-8 short sentences).**"
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- **Test**: Pearson chi-squared (",
        if (identical(branch, "gof")) "goodness-of-fit" else "independence", "). ",
        if (has_yates) "Yates' correction noted for 2x2. " else "",
        if (has_mc) "Monte Carlo p-value used (df may be NA). " else "",
        "\n",
        common_items, "\n",
        "- **Practical reading**: describe the pattern of over-/under-represented cells from printed (standardized) residuals.\n",
        "- **Assumptions**: adequate expected counts; consider exact/Monte Carlo methods when sparse (do not compute here).\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- **Test**: Pearson chi-squared (",
        if (identical(branch, "gof")) "goodness-of-fit" else "independence", "); ",
        "include X^2", if (isTRUE(profile$include_df)) "(df)" else "", ", p, alpha = ", a, ". ",
        if (has_mc) "Monte Carlo p-value: note replicates only if printed. " else "",
        "\n",
        "- **Diagnostics**: discuss cell-wise departures using printed standardized residuals; avoid fabricating effect sizes (e.g., Cramer's V) unless printed.\n",
        "- **Small-sample caution**: Yates/Fisher (2x2) or simulation for sparse tables; do not assert diagnostics.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      )
    )
  }

  # --- Build final prompt -----------------------------------------------------
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = csq_txt,
    output_requirements = output_reqs,
    show_verbatim       = isTRUE(profile$show_verbatim),
    verbatim_title      = "Verbatim output"
  )

  # --- Return or generate -----------------------------------------------------
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate)
}
