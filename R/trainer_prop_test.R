#' Interpret a proportion test (prop.test) with an audience-aware LLM prompt
#'
#' @description
#' Builds a clear, audience-tailored prompt to interpret stats::prop.test() results
#' (one-sample vs target p, two-sample equality, k-group equality, or k-group vs given p).
#' Aligned with other TraineR trainers: no invented numbers; audience-specific guidance.
#'
#' @param pt_obj An htest object returned by stats::prop.test().
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
#'
#' @examples
#' # One-sample
#' pt1 <- prop.test(x = 56, n = 100, p = 0.5)
#' cat(trainer_prop_test(pt1, audience = "beginner"))
#'
#' # Two-sample
#' pt2 <- prop.test(x = c(42, 35), n = c(100, 90))
#' cat(trainer_prop_test(pt2, audience = "applied", summary_only = TRUE))
#' @export
trainer_prop_test <- function(pt_obj,
                              introduction = NULL,
                              alpha = 0.05,
                              audience = c("beginner","applied","advanced"),
                              summary_only = FALSE,
                              llm_model = "llama3",
                              generate = FALSE) {

  audience <- match.arg(audience)
  if (is.null(pt_obj) || !inherits(pt_obj, "htest"))
    stop("pt_obj must be an 'htest' from base R prop.test().")

  # --- Audience profile & header ---------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # --- Verbatim output --------------------------------------------------------
  pt_txt <- trainer_core_capture(pt_obj)

  # --- Fields & branch detection ---------------------------------------------
  alternative <- pt_obj$alternative %||% ""
  conf_level  <- pt_obj$conf.level %||% NA_real_
  data_name   <- pt_obj$data.name %||% ""
  method_line <- pt_obj$method %||%
    "Test for equal/target proportions (score/chi-squared; continuity correction may apply)"

  est <- pt_obj$estimate %||% NA_real_
  k   <- if (all(is.na(est))) NA_integer_ else length(est)
  null_val <- pt_obj$null.value %||% NULL

  branch <- if (!is.na(k) && k == 1L) {
    "one"                 # one-sample vs target p
  } else if (!is.na(k) && k == 2L && is.null(null_val)) {
    "two"                 # two-sample equality
  } else if (!is.na(k) && k >= 3L && is.null(null_val)) {
    "k>2"                 # k-group equality
  } else if (!is.null(null_val) && (is.na(k) || k >= 2L)) {
    "given"               # k-group test vs given probabilities
  } else {
    "two"
  }

  # Continuity correction hint (2x2) if mentioned by prop.test()
  has_yates <- grepl("continuity correction", tolower(method_line))

  # Alternative label (consistent phrasing)
  alt_text <- switch(
    alternative,
    "two.sided" = "two-sided",
    "less"      = "one-sided (Group 1 proportion LESS / below target)",
    "greater"   = "one-sided (Group 1 proportion GREATER / above target)",
    paste0("'", alternative, "'")
  )

  # Default introduction
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We test proportions: a single proportion vs a target, equality between two groups, or equality across multiple groups."
  }

  # Labels (do not invent numbers)
  conf_str <- trainer_core_conf_label(conf_level, fallback = "not reported")

  # Best-effort textual rendering of printed estimates/nulls (no new calcs)
  est_names <- names(est) %||% NULL
  est_str <- if (!is.null(est) && !all(is.na(est))) {
    if (!is.null(est_names) && length(est) == length(est_names)) {
      paste(paste0(est_names, " = ", format(est, digits = 3)), collapse = "; ")
    } else {
      paste(paste0("p[", seq_along(est), "] = ", format(est, digits = 3)), collapse = "; ")
    }
  } else {
    "not printed"
  }

  nv_str <- if (!is.null(null_val)) {
    nms <- names(null_val)
    if (!is.null(nms) && length(null_val) == length(nms)) {
      paste(paste0(nms, " = ", format(null_val, digits = 3)), collapse = "; ")
    } else {
      paste(paste0("p0[", seq_along(null_val), "] = ", format(null_val, digits = 3)), collapse = "; ")
    }
  } else if (branch %in% c("two","k>2")) {
    "equal proportions across groups"
  } else {
    NA_character_
  }

  # --- How-to-read block ------------------------------------------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (proportion tests)",
      "- The test compares observed **proportions** to a **null claim** (target p or equality across groups).",
      "- The **p-value** shows how unusual the data would be if the null claim were true.",
      "- The **confidence interval** indicates plausible values for a proportion (one-sample) or a difference (two-sample), when printed.",
      "- For 3+ groups or tests vs given probabilities, focus on the global chi-squared and the decision.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (proportion tests)",
      "- Report X^2 (df), **p**, and the ", conf_str, " CI when available (one- or two-sample).",
      "- Translate results into **percentage-point** differences for practical meaning (using printed values only).",
      if (has_yates) "- For 2x2, a **continuity correction** may be applied (as printed)." else "- Large-sample approximation; continuity correction may apply for 2x2.",
      "- For sparse counts, consider exact methods (do not compute here).",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (proportion tests)",
      "- One-sample uses a **score/Wilson-type** approach; multi-group uses **Pearson chi-squared**.",
      "- Two-sample: CI for the **difference** in proportions if printed (often Wilson/Newcombe-style).",
      "- Continuity correction can affect 2x2 approximations; adequacy degrades with sparse cells.",
      "- Keep claims within what is printed; do not assert unprinted diagnostics.",
      sep = "\n"
    )
  )

  # --- Setup (orientation) ----------------------------------------------------
  setup_lines <- c(
    paste0("- Significance threshold: p <= ", format(alpha), "."),
    paste0("- Branch: ",
           switch(branch,
                  one   = "One-sample vs target p",
                  two   = "Two-sample equality",
                  "k>2" = "K-group equality",
                  given = "K-group test vs given p",
                  "Proportion test"), "."),
    paste0("- Alternative: ", alt_text, "."),
    paste0("- Data: ", if (nzchar(data_name)) data_name else "counts of successes and trials as passed to prop.test()"),
    paste0("- Estimates: ", est_str, "."),
    if (!is.na(nv_str)) paste0("- Null value(s): ", nv_str, ".") else NULL,
    paste0("- Confidence: ", conf_str, "."),
    paste0("- Method: ", method_line, "."),
    if (has_yates) "- Note: continuity correction may have been applied (2x2)." else NULL,
    "",
    howto_block
  )
  setup <- paste(setup_lines, collapse = "\n")

  # --- Output requirements ----------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = "proportion test"
    )
  } else {
    a <- trainer_core_fmt_alpha(alpha)

    branch_hint <- switch(
      branch,
      "one"   = "One-sample proportion test (score) vs a target p",
      "two"   = "Two-sample equality of proportions (Pearson chi-squared; Yates correction as indicated)",
      "k>2"   = "K-group equality of proportions (Pearson chi-squared across groups)",
      "given" = "K-group test against given probabilities p (Pearson chi-squared)",
      "Proportion test"
    )

    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- **What was tested**: say what proportion(s) were checked and what the null claims.\n",
        "- **Report**: X^2", if (isTRUE(profile$include_df)) "(df)" else "", ", p-value, sample proportion(s), and ",
        if (branch == "one") paste0(conf_str, " CI for the proportion.") else if (branch == "two") paste0(conf_str, " CI for the difference in proportions.") else "the printed global result (no CI for 3+ groups or given p).", "\n",
        "- **Decision**: compare p to alpha = ", a, ".\n",
        "- **Plain meaning**: above/below target (one-sample) or which group is higher (two-sample).\n",
        "- **Note**: large-sample approximation; very small counts can reduce reliability.\n",
        "- **Style**: short sentences; no new calculations.\n\n",
        "**Return only a short explanation (5-8 short sentences).**"
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- **Test**: ", branch_hint, "; alternative: ", alt_text, ". Method: ", method_line, ".\n",
        "- **Report**: X^2", if (isTRUE(profile$include_df)) "(df)" else "", ", p, sample proportion(s), decision at alpha = ", a, ". ",
        if (branch == "one") paste0("Also report the ", conf_str, " CI for the proportion.") else "",
        if (branch == "two") paste0(" Also report the ", conf_str, " CI for the difference in proportions.") else "", "\n",
        "- **Practical meaning**: direction and magnitude in percentage points.\n",
        "- **Assumptions**: chi-squared approximation; Yates correction may be applied for 2x2. No invented diagnostics.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- **Test**: ", branch_hint, "; alternative: ", alt_text, ". Include X^2", if (isTRUE(profile$include_df)) "(df)" else "", ", p, alpha = ", a,
        if (branch == "one") paste0(", and the ", conf_str, " score-based CI for the proportion.") else "",
        if (branch == "two") paste0(", and the ", conf_str, " CI for the difference in proportions.") else "", "\n",
        "- **Nuance**:\n",
        "  - One-sample uses a score test (Wilson-type CI) as printed.\n",
        "  - Two or more groups use Pearson chi-squared; Yates may apply for 2x2 (as printed).\n",
        "  - No CI for k>2 or for given p across k>2 groups unless printed.\n",
        "- **Small-sample caution**: consider exact binomial (one-sample) or Fisher's exact (2x2) when sparse.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      )
    )
  }

  # --- Build final prompt -----------------------------------------------------
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = pt_txt,
    output_requirements = output_reqs,
    show_verbatim       = isTRUE(profile$show_verbatim),
    verbatim_title      = "Verbatim output"
  )

  # --- Return or generate -----------------------------------------------------
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate)
}
