#' Interpret an F test comparing two variances (var.test) with an audience-aware LLM prompt
#'
#' @description
#' Builds a clear, audience-tailored prompt to interpret a base R stats::var.test() result.
#'
#' @param vt_obj An htest object returned by stats::var.test().
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
#' set.seed(1)
#' x <- rnorm(25, sd = 1.0); y <- rnorm(30, sd = 1.3)
#' vt <- var.test(x, y)
#' cat(trainer_var_test(vt, audience = "applied"))
#' cat(trainer_var_test(vt, audience = "advanced", summary_only = TRUE))
#' @export
trainer_var_test <- function(vt_obj,
                             introduction = NULL,
                             alpha = 0.05,
                             audience = c("beginner","applied","advanced"),
                             summary_only = FALSE,
                             llm_model = "llama3",
                             generate = FALSE) {

  audience <- match.arg(audience)
  if (is.null(vt_obj) || !inherits(vt_obj, "htest"))
    stop("vt_obj must be an 'htest' from base R var.test().")

  # --- Audience profile & header ---------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # --- Verbatim output --------------------------------------------------------
  vt_txt <- trainer_core_capture(vt_obj)

  # --- Extract fields safely --------------------------------------------------
  method       <- vt_obj$method       %||% "F test to compare two variances"
  alternative  <- vt_obj$alternative  %||% ""
  ratio_null   <- vt_obj$null.value   %||% NA_real_
  data_name    <- vt_obj$data.name    %||% ""
  conf_level   <- attr(vt_obj$conf.int, "conf.level") %||% NA_real_

  alt_text <- switch(
    alternative,
    "two.sided" = "two-sided (variance ratio could be > or < the target)",
    "less"      = "one-sided (testing if variance ratio is LESS than the target)",
    "greater"   = "one-sided (testing if variance ratio is GREATER than the target)",
    if (nzchar(alternative)) paste0("'", alternative, "'") else "as reported"
  )

  # Default introduction
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We compare the variability (variance) of two groups using an F test."
  }

  # Label for the null ratio as printed by var.test()
  ratio_label <- if (!is.null(names(ratio_null)) && nzchar(names(ratio_null))) {
    paste0(names(ratio_null), " = ", as.character(ratio_null))
  } else if (length(ratio_null)) {
    paste0("ratio = ", as.character(ratio_null))
  } else {
    "as reported"
  }

  # --- How-to-read block (audience-specific) ---------------------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (F test for variances)",
      "- The test compares the **ratio of variances** between two groups (often vs 1 = equal variances).",
      "- If the **ratio > 1**, the first group is more variable; if **< 1**, it is less variable.",
      "- The **p-value** indicates how unusual the observed ratio would be if the null were true.",
      "- The **confidence interval** for the ratio that does **not include 1** supports different variances.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (F test for variances)",
      "- Report F (df1, df2) and **p**; interpret the **variance ratio** as 'how many times more/less variable'.",
      "- **Two-sided vs one-sided** alternative changes the decision rule.",
      "- Be cautious: the F test is **sensitive to non-normality and outliers**; consider robust alternatives if needed.",
      "- CI for the ratio helps quantify uncertainty; focus on whether it crosses 1.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (F test for variances)",
      "- F = (s1^2 / s2^2) vs an F(df1, df2) reference; asymmetry implies CI bounds are not symmetric around 1.",
      "- Decision via **p** or whether the CI for the ratio excludes 1, given the specified alternative.",
      "- Non-robust to heavy tails/outliers; robust checks (Levene/Brown-Forsythe/Fligner) may be preferable.",
      "- Keep claims within what is printed; do not invent diagnostics.",
      sep = "\n"
    )
  )

  # --- Setup (orientation) with embedded verbatim ----------------------------
  setup <- paste(
    paste0("- Test type: ", method, "."),
    paste0("- Data compared: ", if (nzchar(data_name)) data_name else "two samples (as in output)"),
    paste0("- Alternative: ", alt_text, "."),
    paste0("- Null ratio: ", ratio_label, "."),
    paste0("- Confidence: ", trainer_core_conf_label(conf_level, fallback = "not reported"), "."),
    "",
    howto_block,
    "",
    "### Verbatim Output",
    paste0("```\n", vt_txt, "\n```"),
    sep = "\n"
  )

  # --- Output requirements ----------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = "F test for equality of variances"
    )
  } else {
    a <- trainer_core_fmt_alpha(alpha)
    conf_str <- trainer_core_conf_label(conf_level, fallback = "the reported")

    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- **What was tested**: whether the two groups have the same variability.\n",
        "- **Report**: F", if (profile$include_df) " (df1, df2)" else "", ", p-value, estimated variance ratio, null ratio, and the ", conf_str, " CI.\n",
        "- **Decision**: compare p to alpha = ", a, ".\n",
        "- **Plain meaning**: if ratio > 1, the first group is more variable; if < 1, less variable.\n",
        "- **Assumption note**: assumes normal populations; sensitive to outliers.\n",
        "- **Style**: short sentences; no new calculations."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- **Test**: F test comparing two variances; **alternative**: ", alt_text, ".\n",
        "- **Report**: F", if (profile$include_df) " (df1, df2)" else "", ", p, estimated variance ratio, null ratio, ", conf_str, " CI; decision at alpha = ", a, ".\n",
        "- **Interpretation**: translate the ratio as 'how much more/less variable'.\n",
        "- **Assumptions**: normality and independence; consider robust alternatives if outliers.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- **Test**: F for variance ratio; **alternative**: ", alt_text, ". Include F", if (profile$include_df) " (df1, df2)" else "", ", p, estimate, null ratio, ", conf_str, " CI; alpha = ", a, ".\n",
        "- **Notes**: F is non-robust; log-scale interpretations can help (do not compute if not printed).\n",
        "- **Design caution**: unequal n or outliers can inflate Type I error; consider robust tests (Levene/Brown-Forsythe/Fligner) if needed.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      )
    )
  }

  # --- Build final prompt (verbatim already embedded) ------------------------
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = "",
    output_requirements = output_reqs,
    show_verbatim       = FALSE
  )

  # --- Return prompt or generate via LLM -------------------------------------
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate)
}
