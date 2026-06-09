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
#' @param llm_engine Character; backend engine: "ollama", "gemini", or "none".
#' @param ... Passed to the selected LLM backend when `generate = TRUE`.
#' @param generate Logical; if TRUE, call the generator and return prompt + response.
#'
#' @section Privacy:
#' If `generate = TRUE` and `llm_engine` is not `"none"`, the prompt is sent
#' to the selected LLM backend. With external providers such as Gemini, this may
#' include excerpts of statistical outputs and user-provided context.
#'
#' @return An `entrainer_prompt` object. It behaves like a character string
#'   for `cat()`/printing and stores LLM metadata and response as attributes
#'   when `generate = TRUE`.
#' @examples
#' set.seed(1)
#' x <- rnorm(30); y <- 0.5*x + rnorm(30, sd = 0.8)
#' ct <- cor.test(x, y, method = "pearson")
#' cat(trainer_cor_test(ct, audience = "applied", summary_only = FALSE))
#' @export
trainer_cor_test <- function(ct_obj,
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
  trainer_core_check_htest(ct_obj, "ct_obj", "correlation|Pearson|Spearman|Kendall")

  # --- Audience profile & header ---------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # --- Verbatim output --------------------------------------------------------
  ct_txt <- trainer_core_capture(ct_obj)

  # --- Safe field extraction --------------------------------------------------
  method_raw  <- ct_obj$method %||% ""
  method_low  <- tolower(method_raw)
  alternative <- ct_obj$alternative %||% ""
  data_name   <- ct_obj$data.name %||% ""
  est_name    <- if (length(names(ct_obj$estimate))) names(ct_obj$estimate)[1] else ""
  conf_level  <- attr(ct_obj$conf.int, "conf.level") %||% NA_real_

  # Normalize method label (simple)
  method_label <- if (grepl("pearson",  method_low)) "pearson" else
    if (grepl("spearman", method_low)) "spearman" else
      if (grepl("kendall",  method_low)) "kendall"  else method_low

  # Alternative text (human readable)
  alt_text <- switch(
    alternative,
    "two.sided" = "two-sided (association could be positive or negative)",
    "less"      = "one-sided (testing if association is NEGATIVE)",
    "greater"   = "one-sided (testing if association is POSITIVE)",
    if (nzchar(alternative)) paste0("'", alternative, "'") else "as reported"
  )

  # Default intro
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We test whether two numeric variables are associated (move together)."
  }

  conf_str <- trainer_core_conf_label(conf_level, fallback = "not reported")

  # --- How-to-read block ------------------------------------------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (correlation tests)",
      "- The test checks if two variables **tend to move together** (positive or negative).",
      "- The **p-value** tells how unusual the observed association would be if there were **no association**.",
      "- The **confidence interval** (when printed) shows plausible values for the correlation.",
      "- **Correlation is different from causation**; describe association only.",
      "- Do not label the association weak/moderate/strong unless a convention is provided.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (correlation tests)",
      "- Report the estimate (r / rho / tau), the printed test statistic,",
      if (!is.na(conf_level)) paste0(" the ", conf_str, " CI,") else "",
      " p-value, and the decision vs alpha.",
      "- Give a **practical reading** of direction and magnitude using printed values only.",
      "- **Assumptions**: Pearson targets linear association and is sensitive to outliers; Spearman/Kendall are rank-based measures of monotone association, not full diagnostic substitutes.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (correlation tests)",
      "- Pearson targets linear association; Spearman/Kendall target monotone/rank association.",
      "- Pearson CI is often via Fisher z (report only if printed); rank tests use exact/asymptotic nulls as implemented.",
      "- Interpret **direction** and **magnitude** strictly from printed numbers; do not invent diagnostics or causal claims.",
      sep = "\n"
    )
  )

  # --- Setup (orientation) with embedded verbatim ----------------------------
  cap_method <- if (nzchar(method_label)) paste0(toupper(substr(method_label,1,1)),
                                                 substr(method_label,2,nchar(method_label)))
  else "As printed"
  setup <- paste(
    paste0("- Correlation method: ", cap_method, "."),
    paste0("- Alternative: ", alt_text, "."),
    paste0("- Data: ", if (nzchar(data_name)) data_name else
      "two equal-length numeric vectors (as named in the output)."),
    if (nzchar(est_name)) paste0("- Estimate name in output: ", est_name, ".") else NULL,
    paste0("- Decision threshold: alpha = ", trainer_core_fmt_alpha(alpha), "."),
    paste0("- Confidence: ", conf_str, "."),
    "",
    howto_block,
    "",
    "### Verbatim Output",
    paste0("```\n", ct_txt, "\n```"),
    sep = "\n"
  )

  # --- Output requirements ----------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = "correlation test"
    )
  } else {
    a <- trainer_core_fmt_alpha(alpha)
    meth_hint <- switch(
      method_label,
      "pearson"  = "Pearson correlation (linear association; CI reported if available)",
      "spearman" = "Spearman rank correlation (monotonic/rank association)",
      "kendall"  = "Kendall rank correlation (monotonic/rank association)",
      if (nzchar(method_raw)) paste0(method_raw, " correlation") else "correlation test"
    )

    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "1) **What was tested**: do the two variables tend to rise/fall together?\n",
        "2) **Evidence check**: report the correlation estimate (r / rho / tau), printed statistic, p-value, and ", conf_str, " confidence interval if present.\n",
        "3) **Decision**: compare p to alpha = ", a, ".\n",
        "4) **Plain meaning**: say positive/negative association using printed values only; do not classify strength unless a convention is provided.\n",
        "5) **Boundary**: correlation is not causation; no new calculations or diagnostics."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) **Evidence used**: ", meth_hint, "; alternative: ", alt_text, "; estimate (with printed name), printed statistic",
        if (!is.na(conf_level)) paste0(", ", conf_str, " CI") else "",
        ", p, and alpha = ", a, ".\n",
        "2) **Decision**: significant or not, using the printed p-value only.\n",
        "3) **Interpretation**: direction and magnitude; separate magnitude from significance and avoid weak/moderate/strong labels unless a convention is supplied.\n",
        "4) **Assumptions / limits**: Pearson = linear association; Spearman/Kendall = monotone/rank association; no invented diagnostics.\n",
        "5) **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words), with no causal claim."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) **Statistical evidence artifact**: ", meth_hint, "; alternative: ", alt_text, "; estimate, printed statistic",
        " (with df only if printed), p, alpha = ", a,
        if (!is.na(conf_level)) paste0(", and ", conf_str, " CI (when printed).") else ".", "\n",
        "2) **Decision**: apply alpha and distinguish significance from effect magnitude.\n",
        "3) **Construct reading**: Pearson = linear association; Spearman/Kendall = monotone/rank association; do not transfer assumptions across methods.\n",
        "4) **Unsupported claims**: no causality, no outlier/linearity diagnostics unless printed, no standardized strength labels unless a convention is given.\n",
        "5) **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
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
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate, llm_engine = llm_engine, ...)
}
