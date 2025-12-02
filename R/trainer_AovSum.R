#' Trainer: Interpret ANOVA (AovSum) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to interpret an ANOVA
#' produced by FactoMineR::AovSum. The function never invents numbers:
#' it only passes verbatim excerpts to the LLM and instructs how to interpret
#' deviations (sum-to-zero coding) as performance drivers.
#'
#' @param aovsum_obj An object whose printed output contains sections named
#'   "Ftest" and "Ttest" (e.g., \code{FactoMineR::AovSum()}).
#' @param introduction Optional character context paragraph for the analysis.
#'   Defaults to a generic description.
#' @param alpha Numeric significance level used as an instruction for the LLM.
#'   Default 0.05.
#' @param t_test Optional character vector to filter the T-test section by
#'   factor names and/or interactions (e.g. "Factor A" or "Factor A:Factor B").
#' @param audience Target audience, one of c("beginner","applied","advanced").
#' @param summary_only Logical; if TRUE, return a compact 3-bullet executive summary.
#' @param llm_model Character model name for the generator (e.g., "llama3").
#' @param generate Logical; if TRUE, calls trainer_core_generate_or_return().
#'
#' @return Character prompt (if generate = FALSE) or a list.
#'
#' @examples
#' \dontrun{
#' # Example 1: SensoMineR chocolates (requires SensoMineR)
#' if (requireNamespace("SensoMineR", quietly = TRUE)) {
#' # Load data from SensoMineR
#' data("chocolates", package = "SensoMineR")
#' # ANOVA summary with Product and Panelist
#' res <- FactoMineR::AovSum(Granular ~ Product * Panelist, data = sensochoc)
#'
#' intro <- "Six chocolates have been evaluated by a sensory panel,
#' during two days, according to a sensory attribute: granular.
#' The panel has been trained according to this attribute
#' and panellists should be reproducible when rating this attribute."
#' intro <- gsub("\n", " ", intro)
#' intro <- gsub("\\s+", " ", intro)
#' cat(intro)
#'
#' prompt <- trainer_AovSum(res, audience = "beginner",
#'                  t_test = c("Product", "Panelist"),
#'                  introduction = intro)
#' cat(prompt)
#'
#' res <- gemini_generate(prompt, compile_to = "html")
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
#' intro <- gsub("\\s+", " ", intro)
#' cat(intro)
#'
#' res <- FactoMineR::AovSum(Weight ~ Gender * Temperature, data = poussin)
#'
#' prompt <- trainer_AovSum(res,
#'                  audience = "beginner",
#'                  t_test = c("Gender", "Temperature"),
#'                  introduction = intro)
#' cat(prompt)
#'
#' res <- gemini_generate(prompt, compile_to = "html")
#' }
#'
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
  if (is.null(aovsum_obj)) stop("aovsum_obj cannot be NULL.")

  # --- Capture full printed output -------------------------------------------
  aovsum_txt <- if (is.character(aovsum_obj)) {
    paste(aovsum_obj, collapse = "\n")
  } else {
    trainer_core_capture(aovsum_obj)
  }

  # --- Extract Ftest and Ttest (Standard + Heuristic Fallback) ---------------
  ftest_lines <- trainer_core_extract_block_after(aovsum_txt, "Ftest")
  ttest_lines <- trainer_core_extract_block_after(aovsum_txt, "Ttest")

  if (!length(ftest_lines) && !length(ttest_lines)) {
    blocks <- trainer_core_extract_tables_heuristic(aovsum_txt)
    ftest_lines <- trimws(blocks$ftest_lines, which = "both")
    ttest_lines <- trimws(blocks$ttest_lines, which = "both")

    # Ultimate fallback if heuristic also failed: assume everything is F-test
    if (!length(ftest_lines) && !length(ttest_lines)) {
      ftest_lines <- unlist(strsplit(aovsum_txt, "\n", fixed = TRUE), use.names = FALSE)
    }
  }

  # Trim trailing "Signif. codes" in F-test
  if (length(ftest_lines)) {
    sig_idx <- grep("Signif\\. codes:", ftest_lines, perl = TRUE)
    if (length(sig_idx)) ftest_lines <- ftest_lines[seq_len(sig_idx[1] - 1L)]
  }

  # Detect header line in T-test (for potential re-attachment)
  ttest_header <- character(0)
  if (length(ttest_lines)) {
    idx_header <- which(
      grepl("\\bEstimate\\b", ttest_lines, perl = TRUE) |
        grepl("Std\\.?\\s*Error", ttest_lines, perl = TRUE) |
        grepl("Pr\\(>\\|?t\\|?\\)", ttest_lines, perl = TRUE)
    )
    if (length(idx_header)) ttest_header <- ttest_lines[min(idx_header)]
  }

  # --- Default introduction ---------------------------------------------------
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We analyze an ANOVA with experimental factors; interpretation uses deviation (sum-to-zero) contrasts."
  }

  # --- T-test filtering pipeline (space-safe) --------------------------------
  t_req <- if (!is.null(t_test) && length(t_test) && any(nzchar(t_test))) {
    trimws(as.character(t_test))
  } else character(0)

  req_main  <- t_req[!grepl(":", t_req, fixed = TRUE)]
  req_inter <- t_req[ grepl(":", t_req,  fixed = TRUE)]
  requested <- if (length(t_req)) c(req_main, req_inter) else NULL

  ttest_filtered <- trainer_core_filter_ttest_by_factors(
    tt_lines       = ttest_lines,
    keep_factors   = requested,    # NULL -> keep all
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

  # Which requested specs actually appear
  actually_shown <- trainer_core_actually_shown(req_main, req_inter, ttest_filtered)

  # --- Audience profile and header -------------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha, summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # --- Output requirements (domain-agnostic) ---------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50, bullets = 3, label = "ANOVA (AovSum)"
    )
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "1) **Overview**: Indicate which **factors/terms** have an effect (Yes/No from the F-test).\n",
        "2) **Above/below the mean**: For significant factors, state which **levels** are **above** or **below** the global mean (sign of T-test coefficients).\n",
        "3) **Plain language**: Prefer a short, clear sentence rather than raw numbers (avoid jargon).\n",
        "4) **Conclusion**: End with **one sentence** summarizing what matters most.\n",
        "5) **No equations** and **no new statistics**; use only printed values."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) **Model & decision**: Report **F** and **p** for each term and decide at **alpha**.\n",
        "2) **Level profiling**: For significant factors, characterize **levels** by **direction** (deviation around the global mean / Intercept) using T-test coefficients.\n",
        "3) **Interactions**: If an interaction is significant, interpret main effects **conditionally**.\n",
        "4) **Actionable synthesis**: Conclude with a concise, decision-oriented summary.\n",
        "5) **Brevity**: short sentences; do not introduce unprinted statistics."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) **Significance**: Detail **F** and **p** by term; discuss **hierarchy** when interactions exist.\n",
        "2) **Contrasts (sum-to-zero)**: Interpret T-test coefficients as \\(\\mu_i - \\mu\\) (Intercept = global mean), respecting printed **SE** and **p**.\n",
        "3) **Magnitude & precision**: Situate effects relative to the **residual error** and comment on **precision** when shown.\n",
        "4) **Statistical vs practical**: Distinguish statistical and practical importance without extrapolating.\n",
        "5) **Discipline**: Use only printed values; no derived statistics unless printed."
      )
    )
  }

  # --- How-to-read block (domain-agnostic) -----------------------------------
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (ANOVA)",
      "- **F-test**: checks whether a factor/term affects the response overall.",
      "- **T-test (coefficients)**: `(Intercept)` is the global mean; coefficients are deviations around this mean.",
      "- **Signs**: positive = above the mean; negative = below the mean.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (ANOVA)",
      "- **F-test**: identifies global drivers (compare p to alpha).",
      "- **Deviation profile**: coefficients are deviations from the global mean (Intercept).",
      "- **Interactions**: interpret main effects conditionally when an interaction is significant.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (ANOVA)",
      "- **Contrasts**: deviation (sum-to-zero) coding; Intercept = global mean \\(\\mu\\).",
      "- **Omnibus vs partial**: F-test per term; coefficients test \\(H_0: \\mu_i - \\mu = 0\\).",
      "- **Hierarchy**: interpret main effects conditionally in presence of interactions.",
      sep = "\n"
    )
  )

  # --- Compose setup block ----------------------------------------------------
  block_F <- trainer_core_wrap_block("", ftest_lines)
  block_T <- trainer_core_wrap_block(
    "", if (length(ttest_to_show)) ttest_to_show else "(no T-test section detected)"
  )

  ttest_title     <- if (is_filtered) "### T-test (filtered)" else "### T-test"
  ttest_scope_msg <- trainer_core_ttest_scope_msg(t_req, requested, actually_shown)

  setup <- paste(
    paste0("- Significance threshold: p <= ", format(alpha), "."),
    "- Model type: ANOVA with deviation coding (FactoMineR::AovSum).",
    "",
    howto_block,
    "",
    "### F-test (full table)",
    block_F,
    "",
    ttest_title,
    ttest_scope_msg,
    block_T,
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

  # --- Return or generate ----------------------------------------------------
  trainer_core_generate_or_return(prompt, llm_model, generate)
}
