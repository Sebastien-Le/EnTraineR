#' Interpret a Student's t-test (stats::t.test) with an LLM-ready prompt
#'
#' Builds a clear, audience-tailored prompt to interpret a base R \code{stats::t.test()} result.
#' Identifies the test flavor (One-sample, Two-sample, Paired, Welch) and instructs the LLM
#' to use ONLY printed values (p, t, df, CI, estimates) and avoid any new calculations.
#'
#' @param tt_obj An htest object returned by \code{stats::t.test()}.
#' @param introduction Optional character string giving the study context in plain English.
#' @param alpha Numeric significance level used for interpretation (default 0.05).
#' @param audience One of \code{c("beginner","applied","advanced")}.
#' @param summary_only Logical; if TRUE, return a 3-bullet executive summary.
#' @param llm_model Character; model name passed to your generator (default "llama3").
#' @param generate Logical; if TRUE, call \code{trainer_core_generate_or_return()} and return prompt + response.
#'
#' @return If \code{generate = FALSE}, the prompt string. Else a list with \code{prompt}, \code{response}, \code{model}.
#'
#' @examples
#' set.seed(1)
#' tt1 <- t.test(rnorm(20, 0.1), mu = 0)              # one-sample
#' cat(trainer_t_test(tt1, audience = "beginner"))
#'
#' x <- rnorm(18, 0); y <- rnorm(20, 0.3)
#' tt2 <- t.test(x, y, var.equal = FALSE)             # two-sample Welch
#' cat(trainer_t_test(tt2, audience = "applied", summary_only = TRUE))
#'
#' @export
trainer_t_test <- function(tt_obj,
                           introduction = NULL,
                           alpha = 0.05,
                           audience = c("beginner","applied","advanced"),
                           summary_only = FALSE,
                           llm_model = "llama3",
                           generate = FALSE) {

  audience <- match.arg(audience)
  if (is.null(tt_obj) || !inherits(tt_obj, "htest"))
    stop("tt_obj must be an 'htest' object from base R stats::t.test().")

  # --- alpha sanity (soft) ----------------------------------------------------
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    alpha <- 0.05
  }

  # --- Capture verbatim output -----------------------------------------------
  tt_txt <- trainer_core_capture(tt_obj)

  # --- Extract metadata safely ------------------------------------------------
  method       <- tt_obj$method %||% ""
  method_lc    <- tolower(method)
  alternative  <- tt_obj$alternative %||% "two.sided"
  mu_null      <- tt_obj$null.value %||% NA_real_
  conf_level   <- tryCatch(attr(tt_obj$conf.int, "conf.level"), error = function(e) NA_real_)
  conf_level   <- if (is.null(conf_level) || is.na(conf_level)) 0.95 else conf_level

  # Flavor detection (robuste aux variantes d'intitulés)
  is_paired <- grepl("paired", method_lc, perl = TRUE)
  is_two   <- !is.null(tt_obj$estimate) && length(tt_obj$estimate) == 2L
  is_welch <- grepl("welch", method_lc, perl = TRUE)

  flavor <- if (is_paired) {
    "Paired t-test (mean of the differences)"
  } else if (is_two && is_welch) {
    "Two-sample Welch t-test (unequal variances)"
  } else if (is_two) {
    "Two-sample t-test (equal variances)"
  } else {
    "One-sample t-test"
  }

  # Null hypothesis label (imprime sans calcul)
  null_text <- if (!is.na(mu_null[1])) {
    lbl <- names(mu_null)[1] %||% "mean"
    paste0(lbl, " = ", signif(mu_null[1], 4))
  } else {
    "difference = 0"
  }

  # Alternative wording
  alt_human <- switch(
    alternative,
    "less"      = "one-sided (less)",
    "greater"   = "one-sided (greater)",
    "two.sided" = "two-sided",
    alternative
  )

  # --- Default Introduction ---------------------------------------------------
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We interpret a Student's t-test comparing means."
  }

  # --- Audience Profile & Header ---------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)
  conf_lbl <- trainer_core_conf_label(conf_level)

  # --- Output Requirements (no invented numbers) ------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50, bullets = 3, label = flavor
    )
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- Say what is compared (e.g., Group A vs Group B or value vs ", null_text, ").\n",
        "- Decision: significant or not at alpha = ", trainer_core_fmt_alpha(alpha), " (use printed p-value).\n",
        "- Direction: who is higher (use printed estimates or the sign of the estimate of the difference when shown).\n",
        "- Magnitude: describe in plain English using ONLY printed values (avoid new calculations).\n",
        "- One-sentence wrap-up."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- Report t", if (profile$include_df) "(df)" else "", ", p-value, and the decision at alpha = ",
        trainer_core_fmt_alpha(alpha), ". Use scientific notation when printed.\n",
        "- Quote the ", conf_lbl, " CI verbatim and state whether it includes the null (", null_text, ").\n",
        "- Use printed estimates (group means or mean of differences). Do NOT compute unprinted differences.\n",
        "- If Welch appears in the method, note unequal-variance robustness.\n",
        "- End with a practical implication (short)."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- Report t", if (profile$include_df) "(df)" else "", ", p-value, and the ", conf_lbl, " CI; state the decision at alpha = ",
        trainer_core_fmt_alpha(alpha), ".\n",
        "- Interpret the effect using ONLY printed estimates; discuss CI width (precision) and compatibility with the null (", null_text, ").\n",
        "- Note the exact variant (", flavor, ") and alternative (", alt_human, ").\n",
        "- Mention assumption handling when applicable (Welch for heteroscedasticity, paired for within-subject correlation).\n",
        "- Strictly avoid any calculation not present in the output."
      )
    )
  }

  # --- How-to-Read Block -----------------------------------------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (t-test)",
      "- **p-value**: smaller than alpha -> statistically significant.",
      "- **Estimates**: printed averages (or mean of differences).",
      "- **Confidence interval**: plausible range for the (difference in) mean.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (t-test)",
      "- **p-value**: probability of getting a result this extreme if the null were true.",
      "- **CI**: if it includes the null value, the result is not statistically significant at the stated level.",
      "- **Welch**: robust to unequal variances; paired: controls for within-subject variability.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (t-test)",
      "- **Statistic**: t = (estimate - null) / SE; df from the printed output.",
      "- **CI**: set of null values not rejected at level ", trainer_core_fmt_alpha(1 - conf_level), ".",
      "- **Paired vs two-sample**: paired tests the mean of within-pair differences.",
      sep = "\n"
    )
  )

  # --- Compose Setup (embed verbatim) ----------------------------------------
  setup <- paste(
    paste0("- Test variant: ", flavor, "."),
    paste0("- Null hypothesis: ", null_text, "."),
    paste0("- Alternative: ", alt_human, "."),
    "",
    howto_block,
    "",
    "### Verbatim output",
    paste0("```\n", tt_txt, "\n```"),
    sep = "\n"
  )

  # --- Build & Generate ------------------------------------------------------
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = "",  # already embedded
    output_requirements = output_reqs,
    show_verbatim       = FALSE
  )

  trainer_core_generate_or_return(prompt, llm_model, generate)
}
