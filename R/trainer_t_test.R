#' Interpret a Student's t-test (stats::t.test) with an audience-aware LLM prompt
#'
#' @description
#' Builds a clear, audience-tailored prompt to interpret a base R stats::t.test() result.
#' Uses TraineR core helpers to standardize phrasing and generation.
#'
#' @param tt_obj An htest object returned by stats::t.test().
#' @param introduction Optional character string giving the study context in plain English.
#' @param alpha Numeric significance level used for interpretation (default 0.05).
#' @param audience One of c("beginner","applied","advanced").
#' @param summary_only Logical; if TRUE, return a 3-bullet executive summary
#'   regardless of audience depth (uses trainer_core_summary_only_block()).
#' @param llm_model Character; model name passed to your generator (default "llama3").
#' @param generate Logical; if TRUE, call trainer_core_generate_or_return() and return prompt + response.
#'
#' @return If generate = FALSE, returns the prompt string. If TRUE, returns a list with
#'   prompt, response, and model.
#' @export
#'
#' @examples
#' set.seed(1)
#' tt1 <- t.test(rnorm(20, 0.1), mu = 0)              # one-sample
#' cat(trainer_t_test(tt1, audience = "beginner"))
#'
#' x <- rnorm(18, 0); y <- rnorm(20, 0.3)
#' tt2 <- t.test(x, y, var.equal = FALSE)             # two-sample Welch
#' cat(trainer_t_test(tt2, audience = "applied", summary_only = TRUE))
trainer_t_test <- function(tt_obj,
                           introduction = NULL,
                           alpha = 0.05,
                           audience = c("beginner","applied","advanced"),
                           summary_only = FALSE,
                           llm_model = "llama3",
                           generate = FALSE) {

  audience <- match.arg(audience)
  if (is.null(tt_obj) || !inherits(tt_obj, "htest"))
    stop("tt_obj must be an 'htest' from base R t.test().")

  # Core profile and header
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # Verbatim output
  tt_txt <- trainer_core_capture(tt_obj)

  # Extract fields
  method       <- tt_obj$method %||% ""
  alternative  <- tt_obj$alternative %||% ""
  mu_null      <- tt_obj$null.value %||% NA_real_
  conf_level   <- tt_obj$conf.level %||% NA_real_

  # Classify flavor
  flavor <- if (grepl("paired", tolower(method))) {
    "Paired t-test"
  } else if (length(tt_obj$estimate) == 2L) {
    if (grepl("Welch", method)) "Two-sample Welch t-test" else "Two-sample t-test (equal variances)"
  } else {
    "One-sample t-test"
  }

  # Alternative text
  alt_text <- switch(
    alternative,
    "two.sided" = "two-sided (difference could be either positive or negative)",
    "less"      = "one-sided (testing if the true mean/difference is LESS than the target)",
    "greater"   = "one-sided (testing if the true mean/difference is GREATER than the target)",
    paste0("'", alternative, "'")
  )

  # Null text
  mu_text <- if (!is.null(names(mu_null)) && nzchar(names(mu_null))) {
    paste0(names(mu_null), " = ", as.character(mu_null))
  } else if (length(mu_null)) {
    paste0("mu = ", as.character(mu_null))
  } else {
    NA_character_
  }

  # Default context
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We want to check whether an observed mean (or mean difference) conforms to a target value."
  }

  # "How to read" block (audience-specific)
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (t-test)",
      "- The test compares an observed mean (or mean difference) to a target (the null value).",
      "- The p-value tells how unusual the data would be if the null were true.",
      "- Report the estimate and the confidence interval; explain direction vs the target in plain English.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (t-test)",
      "- State the estimate, t (and df if shown), p, and the confidence interval.",
      "- Interpret direction and practical magnitude in original units; keep significance (p) separate from magnitude.",
      "- Paired t-test uses within-subject differences; Welch handles unequal variances (two-sample).",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (t-test)",
      "- Report estimate, t (df), p, and CI; note branch (paired / Welch) where relevant.",
      "- Discuss effect vs significance using printed values only; avoid inventing diagnostics.",
      "- Be cautious with small df; keep claims within what is printed.",
      sep = "\n"
    )
  )

  # Setup block
  setup_lines <- c(
    paste0("- Test type: ", flavor, "."),
    if (!is.na(mu_text)) paste0("- Null value: ", mu_text, ".") else NULL,
    paste0("- Alternative: ", alt_text, "."),
    paste0("- Confidence: ",
           trainer_core_conf_label(conf_level, fallback = "not reported"), "."),
    "",
    howto_block
  )
  setup <- paste(setup_lines, collapse = "\n")

  # Output requirements (audience-specific or summary-only)
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = flavor
    )
  } else {
    a <- trainer_core_fmt_alpha(alpha)
    conf_str <- trainer_core_conf_label(conf_level)

    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- **What was tested**: say the null in words (mean or mean difference equals the target).\n",
        "- **Report**: t", if (profile$include_df) "(df)" else "", ", p-value, the estimated mean or mean difference, and the ", conf_str, " confidence interval.\n",
        "- **Decision**: compare p to alpha = ", a, ".\n",
        "- **Direction & magnitude**: is the estimate above/below the target, and by roughly how much (in units if known).\n",
        "- **Meaning of p-value**: probability, assuming the null is true, of a result at least this extreme by chance.\n",
        "- **Style**: short sentences; no new calculations.\n\n",
        "**Return only a short explanation (5-8 short sentences).**"
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- **Test**: ", flavor, "; **alternative**: ", alt_text, ".\n",
        "- **Report**: t", if (profile$include_df) "(df)" else "", ", p, estimate, ", conf_str, " CI, and decision at alpha = ", a, ".\n",
        "- **Practical meaning**: direction/magnitude in units; separate effect size (if printed) from significance.\n",
        "- **Assumptions**: Welch for unequal variances; paired uses within-subject differences. Do **not** invent diagnostics.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- **Test**: ", flavor, "; **alternative**: ", alt_text, ". Include t", if (profile$include_df) "(df)" else "", ", p, estimate, ", conf_str, " CI; alpha = ", a, ".\n",
        "- **Branches**: paired -> differences; unequal variances -> Welch/Satterthwaite; avoid inventing normality checks.\n",
        "- **Effect vs significance**: discuss magnitude vs p using printed values only; keep claims conservative with small df.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      )
    )
  }

  # Build final prompt
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = tt_txt,
    output_requirements = output_reqs,
    show_verbatim       = isTRUE(profile$show_verbatim),
    verbatim_title      = "Verbatim output"
  )

  # Return prompt or generate via LLM
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate)
}
