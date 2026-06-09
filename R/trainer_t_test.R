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
#' @param llm_engine Character; backend engine: "ollama", "gemini", or "none".
#' @param ... Passed to the selected LLM backend when `generate = TRUE`.
#' @param generate Logical; if TRUE, call \code{trainer_core_generate_or_return()} and return prompt + response.
#'
#' @section Privacy:
#' If `generate = TRUE` and `llm_engine` is not `"none"`, the prompt is sent
#' to the selected LLM backend. With external providers such as Gemini, this may
#' include excerpts of statistical outputs and user-provided context.
#'
#' @return An `entrainer_prompt` object. It behaves like a character string
#'   for `cat()`/printing and stores LLM metadata and response as attributes
#'   when `generate = TRUE`.
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
                           generate = FALSE,
                           llm_engine = c("ollama", "gemini", "none"),
                           ...) {

  audience <- match.arg(audience)
  alpha <- trainer_core_check_probability(alpha, "alpha")
  summary_only <- trainer_core_check_flag(summary_only, "summary_only")
  generate <- trainer_core_check_flag(generate, "generate")
  llm_model <- trainer_core_check_string(llm_model, "llm_model")
  llm_engine <- match.arg(llm_engine)
  introduction <- trainer_core_check_optional_string(introduction, "introduction")
  trainer_core_check_htest(tt_obj, "tt_obj", "t-test|t test|Welch|Paired")

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
        "1) **What was tested**: say what is compared (e.g., Group A vs Group B or value vs ", null_text, ").\n",
        "2) **Evidence check**: name the test variant, printed p-value, and printed CI/estimate when available.\n",
        "3) **Decision**: significant or not at alpha = ", trainer_core_fmt_alpha(alpha), " (use printed p-value only).\n",
        "4) **Meaning**: state direction from printed estimates or the sign of the printed difference; if group order is unclear, say so.\n",
        "5) **Boundary**: one sentence on what cannot be concluded from this output (for example causality or unprinted diagnostics)."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) **Evidence used**: report t", if (profile$include_df) "(df)" else "", ", p-value, ", conf_lbl, " CI, and printed estimates exactly enough to justify the interpretation.\n",
        "2) **Decision rule**: compare p to alpha = ", trainer_core_fmt_alpha(alpha), "; state whether the CI is compatible with the null (", null_text, ").\n",
        "3) **Interpretation**: use printed estimates only; do NOT compute unprinted differences; respect the printed group/order direction.\n",
        "4) **Assumption handling**: if Welch appears, note unequal-variance robustness; if paired, note within-pair comparison.\n",
        "5) **Practical implication**: add one short implication only if it follows from the provided context; otherwise state what context is missing."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) **Statistical evidence artifact**: variant = ", flavor, "; alternative = ", alt_human, "; report t", if (profile$include_df) "(df)" else "", ", p-value, ", conf_lbl, " CI, null (", null_text, "), and printed estimates.\n",
        "2) **Decision**: apply alpha = ", trainer_core_fmt_alpha(alpha), "; discuss compatibility with the null and precision from the CI, without computing new values.\n",
        "3) **Effect reading**: interpret direction and magnitude only from printed estimates and the printed order. If order is ambiguous, do not name a group direction.\n",
        "4) **Assumptions / design**: mention Welch, paired structure, independence, and normality only as far as they are relevant; do not invent diagnostics.\n",
        "5) **Unsupported claims**: explicitly avoid causality, unprinted effect sizes, and unprinted robustness checks."
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
      "- **Direction**: depends on the printed estimate/order; do not reverse it.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (t-test)",
      "- **p-value**: probability of getting a result this extreme if the null were true.",
      "- **CI**: set of null values not rejected at level ", trainer_core_fmt_alpha(1 - conf_level), ".",
      "- **Welch**: addresses unequal variances; paired: analyzes within-subject differences.",
      "- **Practical importance** cannot be inferred from the p-value alone.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (t-test)",
      "- **Statistic**: t = (estimate - null) / SE; df from the printed output.",
      "- **CI**: set of null values not rejected at level ", trainer_core_fmt_alpha(1 - conf_level), ".",
      "- **Paired vs two-sample**: paired tests the mean of within-pair differences.",
      "- Keep the interpretation order-consistent; do not add standardized effects unless printed.",
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

  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate, llm_engine = llm_engine, ...)
}
