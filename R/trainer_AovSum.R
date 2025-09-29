# R/trainer_AovSum.R

#' Trainer: Interpret ANOVA (AovSum) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to interpret an ANOVA
#' produced by FactoMineR::AovSum (or a similar object that prints
#' "Ftest" and "Ttest" sections). The function never invents numbers:
#' it only passes verbatim output to the LLM and instructs how to interpret it.
#'
#' @param aovsum_obj An object whose printed output contains sections named
#'   "Ftest" and "Ttest" (e.g., FactoMineR::AovSum).
#' @param introduction Optional character context paragraph for the analysis.
#'   Defaults to a generic description.
#' @param alpha Numeric significance level used as an instruction for the LLM
#'   (no computation is performed by this function). Default 0.05.
#' @param t_test Optional character vector to filter the T-test section by
#'   factor names and/or interactions expressed as "A:B".
#' @param audience Target audience, one of c("beginner","applied","advanced").
#' @param summary_only Logical; if TRUE, return a 3-bullet executive summary
#'   regardless of audience depth (uses trainer_core_summary_only_block()).
#' @param llm_model Character model name for the generator (e.g., "llama3").
#' @param generate Logical; if TRUE, calls trainer_core_generate_or_return()
#'   and returns a list with prompt and response. If FALSE, returns the prompt.
#'
#' @return Character prompt (if generate = FALSE) or a list with
#'   prompt, response, and model.
#'
#' @examples
#' \dontrun{
#' # Example 1: SensoMineR chocolates (requires SensoMineR)
#' if (requireNamespace("SensoMineR", quietly = TRUE)) {
#'   # Load data from SensoMineR
#'   data("chocolates", package = "SensoMineR")
#'   # ANOVA summary with Product and Panelist
#'   res <- FactoMineR::AovSum(Granular ~ Product * Panelist, data = sensochoc)
#'
#'   intro <- "Six chocolates have been evaluated by a sensory panel,
#'   during two days, according to a sensory attribute: granular.
#'   The panel has been trained according to this attribute
#'   and panellists should be reproducible when rating this attribute."
#'   intro <- gsub("\n", " ", intro)
#'   intro <- stringr::str_squish(intro)
#'
#'   p <- trainer_AovSum(res, audience = "beginner",
#'                       t_test = c("Product", "Panelist"),
#'                       introduction = intro)
#'   cat(p)
#' }
#'
#' # Example 2: Poussin dataset (shipped with this package)
#' data(poussin)
#' intro <- "For incubation, 45 chicken eggs were randomly assigned to three batches of 15.
#' Three treatments (different incubation temperatures) were then applied to the batches.
#' We assume that after hatching, all chicks were raised under identical conditions
#' and then weighed at a standard reference age.
#' At that time, the sex of the chicks - a factor known beforehand to cause
#' significant weight differences - could also be observed.
#' The objective is to choose the treatment that maximizes chick weight."
#' intro <- gsub("\n", " ", intro)
#' intro <- stringr::str_squish(intro)
#'
#' res <- FactoMineR::AovSum(Weight ~ Gender * Temperature, data = poussin)
#' p <- trainer_AovSum(res,
#'                     audience = "beginner",
#'                     t_test = c("Gender", "Temperature"),
#'                     introduction = intro)
#' cat(p)
#' }
#' @export
trainer_AovSum <- function(aovsum_obj,
                           introduction = NULL,
                           alpha = 0.05,
                           t_test = NULL,
                           audience = c("beginner","applied","advanced"),
                           summary_only = FALSE,
                           llm_model = "llama3",
                           generate = FALSE) {

  audience <- match.arg(audience)

  # --- Capture printed AovSum output -----------------------------------------
  aovsum_txt <- trainer_core_capture(aovsum_obj)

  # --- Extract Ftest & Ttest tables (verbatim) --------------------------------
  ftest_lines <- trainer_core_extract_block_after(aovsum_txt, "Ftest")
  ttest_lines <- trainer_core_extract_block_after(aovsum_txt, "Ttest")

  # Detect the column header line of the T-test (if present)
  ttest_header <- character(0)
  if (length(ttest_lines)) {
    idx_header <- which(grepl("\\bEstimate\\b", ttest_lines))
    if (length(idx_header)) ttest_header <- ttest_lines[idx_header[1]]
  }

  # --- Default introduction ---------------------------------------------------
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We analyze an ANOVA on a score variable with experimental factors to support decisions."
  }

  # --- T-test filtering pipeline ---------------------------------------------
  detected_main <- trainer_core_detect_main_factors(ttest_lines)

  if (is.null(t_test) || length(t_test) == 0L || all(!nzchar(t_test))) {
    req_main  <- NULL
    req_inter <- NULL
  } else {
    t_req     <- as.character(t_test)
    req_main  <- intersect(t_req[!grepl(":", t_req)], detected_main)
    req_inter <- t_req[grepl(":", t_req)]  # leave as-is; existence checked by filtering
  }
  requested <- c(req_main, req_inter)

  ttest_filtered <- trainer_core_filter_ttest_by_factors(
    ttest_lines,
    keep_factors = if (length(requested)) requested else NULL,
    keep_intercept = TRUE
  )

  # Decide if filtered or not
  is_filtered <- length(requested) > 0

  # Re-attach header only if filtered and not already present
  ttest_to_show <- ttest_filtered
  if (is_filtered && length(ttest_header) && length(ttest_to_show)) {
    first_line_has_header <- grepl("\\bEstimate\\b", ttest_to_show[1])
    if (!first_line_has_header) {
      ttest_to_show <- c(ttest_header, ttest_to_show)
    }
  }

  actually_shown  <- trainer_core_actually_shown(req_main, req_inter, ttest_filtered)

  # --- Audience profile and header -------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # --- Audience-tailored output requirements ---------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = "ANOVA (AovSum)"
    )
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "- **What was tested**: say what each effect represents.\n",
        "- **Report**: F", if (profile$include_df) "(df1, df2)" else "", ", p-value, and decision at alpha = ", trainer_core_fmt_alpha(alpha), ".\n",
        "- **Meaning**: for significant effects, explain in plain English which levels look higher/lower (no new calculations).\n",
        "- **Interactions**: if significant, main effects depend on the other factor (interpret conditionally).\n",
        "- **T-test coefficients**: (Intercept) is the grand mean under sum-to-zero contrasts; each level's estimate is a deviation from the grand mean.\n",
        "- **Style**: short sentences; do not compute new statistics."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "- **F-test**: for each row, report F", if (profile$include_df) "(df1, df2)" else "", ", p, and decision at alpha = ", trainer_core_fmt_alpha(alpha), "; add one practical implication tied to the context.\n",
        "- **Interactions**: if present and significant, interpret main effects as **conditional** (simple effects); avoid unconditional statements.\n",
        "- **T-test (shown terms)**: (Intercept) = grand mean; level estimates are deviations. Make within-factor comparisons only if printed.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words) focusing on decisions."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "- **F-test**: provide F", if (profile$include_df) "(df1, df2)" else "", ", p, alpha decision; mention MS structure only if printed; do not invent corrections.\n",
        "- **Hierarchy**: when interactions are significant, interpret main effects conditionally; note deviation (sum-to-zero) coding.\n",
        "- **T-test**: connect coefficients to reconstructed means only if visible in the output (grand mean + deviation); do not fabricate values.\n",
        "- **Multiplicity/df**: if many rows relative to df, caution about multiplicity without inventing adjustments.\n",
        "- **Takeaway**: <=", profile$max_bullets, " bullets (<= ", profile$max_words_takeaway, " words)."
      )
    )
  }

  # --- "How to read (ANOVA)" block (audience-specific) -----------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (ANOVA)",
      "- F-test (table below): does each effect change the mean?",
      "- T-test: level coefficients with sum-to-zero contrasts; `(Intercept)` is the grand mean.",
      "- If an interaction is significant, interpret main effects **conditionally**.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (ANOVA)",
      "- F-test: decision per effect (p vs alpha) + one applied implication.",
      "- T-test: deviations by level around the grand mean (sum-to-zero contrasts).",
      "- If interaction: report **simple effects**; avoid unconditional claims.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (ANOVA)",
      "- F-tests for model terms; respect hierarchy with interactions.",
      "- T-tests under deviation coding; relate to reconstructed means **only if printed**.",
      "- Many rows vs df -> multiplicity caution (no invented adjustments).",
      sep = "\n"
    )
  )

  # --- Compose setup block (includes verbatim tables) -------------------------
  ttest_title <- if (is_filtered) "### T-test (filtered)" else "### T-test"
  ttest_scope_msg <- if (is_filtered) {
    trainer_core_ttest_scope_msg(t_test, requested, actually_shown)
  } else {
    "T-test shows all coefficients."
  }

  setup <- paste(
    "- Assume sum-to-zero contrasts: coefficients are deviations from the grand mean; `(Intercept)` is the grand mean.",
    "",
    howto_block,
    "",
    "### F-test (full table)",
    "```",
    trainer_core_collapse(ftest_lines),
    "```",
    "",
    ttest_title,
    ttest_scope_msg,
    "```",
    trainer_core_collapse(ttest_to_show),
    "```",
    sep = "\n"
  )

  # We already embedded verbatim tables in setup; do not add a second verbatim block.
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = "",
    output_requirements = output_reqs,
    show_verbatim       = FALSE
  )

  # --- Generate or return -----------------------------------------------------
  trainer_core_generate_or_return(prompt, llm_model, generate)
}
