#' Interpret a correlation test (cor.test) with an audience-aware LLM prompt
#'
#' @description
#' Builds a clear, audience-tailored prompt to interpret stats::cor.test() results
#' for Pearson, Spearman, or Kendall correlation. Supports three audiences
#' ("beginner", "applied", "advanced") and an optional summary_only mode.
#'
#' @param ct_obj An htest object returned by stats::cor.test().
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
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(30); y <- 0.5*x + rnorm(30, sd = 0.8)
#' ct <- cor.test(x, y, method = "pearson")
#' cat(trainer_cor_test(ct, audience = "applied", summary_only = FALSE))
trainer_cor_test <- function(ct_obj,
                             introduction = NULL,
                             alpha = 0.05,
                             audience = c("beginner","applied","advanced"),
                             summary_only = FALSE,
                             llm_model = "llama3",
                             generate = FALSE) {

  audience <- match.arg(audience)
  if (is.null(ct_obj) || !inherits(ct_obj, "htest"))
    stop("ct_obj must be an 'htest' from base R cor.test().")

  # Core profile and header
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # Verbatim output
  ct_txt <- trainer_core_capture(ct_obj)

  # Pull fields from htest (safe null-coalescing)
  method      <- tolower(ct_obj$method %||% "")
  alternative <- ct_obj$alternative %||% ""
  conf_level  <- ct_obj$conf.level %||% NA_real_
  data_name   <- ct_obj$data.name %||% ""
  est_name    <- names(ct_obj$estimate) %||% ""

  # Normalize method label (simple)
  method_label <- if (grepl("pearson", method)) {
    "pearson"
  } else if (grepl("spearman", method)) {
    "spearman"
  } else if (grepl("kendall", method)) {
    "kendall"
  } else {
    method
  }

  # Alternative text
  alt_text <- switch(
    alternative,
    "two.sided" = "two-sided (association could be positive or negative)",
    "less"      = "one-sided (testing if association is NEGATIVE)",
    "greater"   = "one-sided (testing if association is POSITIVE)",
    paste0("'", alternative, "'")
  )

  # Default intro
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We test whether two numeric variables move together (are associated)."
  }

  # Labels
  conf_str <- trainer_core_conf_label(conf_level, fallback = "not reported")

  # "How to read" block
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (correlation tests)",
      "- The test checks if two variables **tend to move together** (positive or negative).",
      "- The **p-value** tells how unusual the observed association would be if there were **no association**.",
      "- The **confidence interval** (when printed) shows plausible values for the correlation.",
      "- **Correlation <> causation**; describe association only.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (correlation tests)",
      "- Report the estimate (r/rho/tau), test statistic (t or z)", if (!is.na(conf_level)) paste0(", the ", conf_str, " CI,") else ",",
      " p-value, and the decision vs alpha.",
      "- Give a **practical reading** of direction and strength using printed values only.",
      "- **Assumptions**: Pearson targets linear association and is sensitive to outliers; Spearman/Kendall are rank-based and robust to monotone changes.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (correlation tests)",
      "- Pearson assumes approximate bivariate normality and linear relation; CI often via **Fisher z** (report only if printed).",
      "- Spearman/Kendall rely on rank-based nulls (asymptotic/exact per implementation).",
      "- Interpret **direction** and **magnitude** strictly from printed numbers; do not invent diagnostics.",
      sep = "\n"
    )
  )

  # Setup block (orientation)
  setup_lines <- c(
    paste0("- Correlation method: ",
           if (nzchar(method_label)) paste0(toupper(substr(method_label,1,1)), substr(method_label,2,nchar(method_label))) else "as printed", "."),
    paste0("- Alternative: ", alt_text, "."),
    paste0("- Data: ", if (nzchar(data_name)) data_name else
      "two equal-length numeric vectors (as named in the output)"),
    if (nzchar(est_name)) paste0("- Estimate name in output: ", est_name, ".") else NULL,
    paste0("- Confidence: ", conf_str, "."),
    "",
    howto_block
  )
  setup <- paste(setup_lines, collapse = "\n")

  # Audience-specific output requirements (or summary-only override)
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = "correlation test"
    )
  } else {
    a <- trainer_core_fmt_alpha(alpha)
    meth_hint <- switch(method_label,
                        "pearson"  = "Pearson correlation (linear association; CI reported if available)",
                        "spearman" = "Spearman rank correlation (monotonic association; robust to outliers)",
                        "kendall"  = "Kendall rank correlation (monotonic association; robust to outliers)",
                        if (nzchar(method)) paste0(method, " correlation") else "correlation test")

    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- **What was tested**: do the two variables tend to rise/fall together?\n",
        "- **Report**: correlation estimate (r/rho/tau), the printed statistic (t or z), p-value, and the ", conf_str, " confidence interval if present.\n",
        "- **Decision**: compare p to alpha = ", a, ".\n",
        "- **Plain meaning**: say positive/negative and roughly how strong, using printed values only. **Correlation <> causation**.\n",
        "- **Style**: short sentences; no new calculations.\n\n",
        "**Return only a short explanation (5-8 short sentences).**"
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- **Test**: ", meth_hint, "; alternative: ", alt_text, ".\n",
        "- **Report**: estimate (with printed name), statistic (t or z)",
        if (!is.na(conf_level)) paste0(", ", conf_str, " CI") else "",
        ", p, and decision at alpha = ", a, ".\n",
        "- **Interpretation**: direction (more/less together) and practical strength; separate magnitude from significance.\n",
        "- **Assumptions**: linearity/outliers for Pearson; rank tests are robust/monotone. Do not invent diagnostics.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- **Test**: ", meth_hint, "; alternative: ", alt_text, ". Include estimate, statistic (with df if printed), p, alpha = ", a,
        if (!is.na(conf_level)) paste0(", and ", conf_str, " CI (Pearson when available).") else ".", "\n",
        "- **Nuance**: Pearson CI often via Fisher z (report only if printed). Spearman/Kendall rely on rank-based nulls (AS 89/normal approx/exact per implementation). No new computations.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      )
    )
  }

  # Build final prompt using the core skeleton
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = ct_txt,
    output_requirements = output_reqs,
    show_verbatim       = isTRUE(profile$show_verbatim),
    verbatim_title      = "Verbatim output"
  )

  # Return prompt or generate via LLM
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate)
}
