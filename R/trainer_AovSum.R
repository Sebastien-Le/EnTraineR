#' Trainer: Interpret ANOVA (AovSum) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to interpret an ANOVA
#' produced by FactoMineR::AovSum. The function never invents numbers:
#' it only passes verbatim excerpts to the LLM and instructs how to interpret
#' deviations (sum-to-zero coding) as performance drivers.
#'
#' @param x An object whose printed output contains sections named
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
#' @param llm_engine Character; backend engine: "ollama", "gemini", or "none".
#' @param ... Passed to the selected LLM backend when `generate = TRUE`.
#' @param generate Logical; if TRUE, calls trainer_core_generate_or_return().
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
#' prompt <- trainer_aovsum(res, audience = "beginner",
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
#' prompt <- trainer_aovsum(res,
#'                  audience = "beginner",
#'                  t_test = c("Gender", "Temperature"),
#'                  introduction = intro)
#' cat(prompt)
#'
#' res <- gemini_generate(prompt, compile_to = "html")
#' }
#'
#' @export
trainer_aovsum <- function(x,
                           introduction = NULL,
                           alpha = 0.05,
                           t_test = NULL,
                           audience = c("beginner", "applied", "advanced"),
                           summary_only = FALSE,
                           llm_model = "llama3",
                           generate = FALSE,
                           llm_engine = c("ollama", "gemini", "none"),
                           ...) {

  aovsum_obj <- x

  audience <- match.arg(audience)
  alpha <- trainer_core_check_probability(alpha, "alpha")
  summary_only <- trainer_core_check_flag(summary_only, "summary_only")
  generate <- trainer_core_check_flag(generate, "generate")
  llm_model <- trainer_core_check_string(llm_model, "llm_model")
  llm_engine <- match.arg(llm_engine)
  introduction <- trainer_core_check_optional_string(introduction, "introduction")

  if (!is.null(t_test) && (!is.character(t_test) || anyNA(t_test))) {
    stop("`t_test` must be NULL or a character vector without missing values.", call. = FALSE)
  }

  if (is.null(aovsum_obj)) {
    stop("`x` cannot be NULL.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Preferred path: structured AovSum-like object
  # ---------------------------------------------------------------------------
  # A real AovSum object is generally a list-like object containing Ftest and
  # Ttest components. In that case, we should not parse summary(x), because
  # summary(x) only reports the structure of the object, not the statistical
  # tables themselves.

  is_aovsum_like <- is.list(aovsum_obj) &&
    any(c("Ftest", "Ttest") %in% names(aovsum_obj))

  if (is_aovsum_like) {

    ftest_lines <- if ("Ftest" %in% names(aovsum_obj) && !is.null(aovsum_obj$Ftest)) {
      trainer_core_capture(aovsum_obj$Ftest)
    } else {
      character(0)
    }

    ttest_lines <- if ("Ttest" %in% names(aovsum_obj) && !is.null(aovsum_obj$Ttest)) {
      trainer_core_capture(aovsum_obj$Ttest)
    } else {
      character(0)
    }

  } else {

    # -------------------------------------------------------------------------
    # Fallback path: printed text extraction
    # -------------------------------------------------------------------------

    aovsum_txt <- if (is.character(aovsum_obj)) {
      paste(aovsum_obj, collapse = "\n")
    } else {
      trainer_core_capture(aovsum_obj)
    }

    # --- Extract Ftest and Ttest (standard + heuristic fallback) --------------
    ftest_lines <- trainer_core_extract_block_after(aovsum_txt, "Ftest")
    ttest_lines <- trainer_core_extract_block_after(aovsum_txt, "Ttest")

    if (!length(ftest_lines) && !length(ttest_lines)) {
      blocks <- trainer_core_extract_tables_heuristic(aovsum_txt)
      ftest_lines <- trimws(blocks$ftest_lines, which = "both")
      ttest_lines <- trimws(blocks$ttest_lines, which = "both")

      # Ultimate fallback if heuristic also failed: assume everything is F-test
      if (!length(ftest_lines) && !length(ttest_lines)) {
        warning(
          "Could not detect `Ftest` or `Ttest` sections in `x`. ",
          "The full printed output will be used as a fallback, but interpretation may be limited.",
          call. = FALSE
        )

        ftest_lines <- unlist(
          strsplit(aovsum_txt, "\n", fixed = TRUE),
          use.names = FALSE
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Clean extracted blocks
  # ---------------------------------------------------------------------------

  ftest_lines <- trimws(ftest_lines, which = "both")
  ttest_lines <- trimws(ttest_lines, which = "both")

  ftest_lines <- ftest_lines[nzchar(ftest_lines)]
  ttest_lines <- ttest_lines[nzchar(ttest_lines)]

  # Trim trailing "Signif. codes" in F-test
  if (length(ftest_lines)) {
    sig_idx <- grep("Signif\\. codes:", ftest_lines, perl = TRUE)
    if (length(sig_idx)) {
      ftest_lines <- ftest_lines[seq_len(sig_idx[1L] - 1L)]
    }
  }

  # Trim trailing "Signif. codes" in T-test as well
  if (length(ttest_lines)) {
    sig_idx <- grep("Signif\\. codes:", ttest_lines, perl = TRUE)
    if (length(sig_idx)) {
      ttest_lines <- ttest_lines[seq_len(sig_idx[1L] - 1L)]
    }
  }

  # Detect header line in T-test for potential re-attachment after filtering
  ttest_header <- character(0)

  if (length(ttest_lines)) {
    idx_header <- which(
      grepl("\\bEstimate\\b", ttest_lines, perl = TRUE) |
        grepl("Std\\.?\\s*Error", ttest_lines, perl = TRUE) |
        grepl("Pr\\(>\\|?t\\|?\\)", ttest_lines, perl = TRUE)
    )

    if (length(idx_header)) {
      ttest_header <- ttest_lines[min(idx_header)]
    }
  }

  # ---------------------------------------------------------------------------
  # Default introduction
  # ---------------------------------------------------------------------------

  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- "We analyze an ANOVA with experimental factors; interpretation uses deviation (sum-to-zero) contrasts."
  }

  # ---------------------------------------------------------------------------
  # T-test filtering pipeline
  # ---------------------------------------------------------------------------

  t_req <- if (!is.null(t_test) && length(t_test) && any(nzchar(t_test))) {
    trimws(as.character(t_test))
  } else {
    character(0)
  }

  req_main <- t_req[!grepl(":", t_req, fixed = TRUE)]
  req_inter <- t_req[grepl(":", t_req, fixed = TRUE)]

  requested <- if (length(t_req)) {
    c(req_main, req_inter)
  } else {
    NULL
  }

  ttest_filtered <- trainer_core_filter_ttest_by_factors(
    tt_lines       = ttest_lines,
    keep_factors   = requested,
    keep_intercept = TRUE
  )

  is_filtered <- !is.null(requested)
  ttest_to_show <- ttest_filtered

  # Re-attach header if missing after filtering
  if (is_filtered && length(ttest_header) && length(ttest_to_show)) {
    first_has_header <- grepl("\\bEstimate\\b", ttest_to_show[1L], perl = TRUE) ||
      grepl("Std\\.?\\s*Error", ttest_to_show[1L], perl = TRUE) ||
      grepl("Pr\\(>\\|?t\\|?\\)", ttest_to_show[1L], perl = TRUE)

    if (!first_has_header) {
      ttest_to_show <- c(ttest_header, ttest_to_show)
    }
  }

  actually_shown <- trainer_core_actually_shown(
    req_main,
    req_inter,
    ttest_filtered
  )

  # ---------------------------------------------------------------------------
  # Audience profile and header
  # ---------------------------------------------------------------------------

  profile <- trainer_core_audience_profile(
    audience,
    alpha,
    summary_only = summary_only
  )

  header <- trainer_core_prompt_header(profile)

  # ---------------------------------------------------------------------------
  # Output requirements
  # ---------------------------------------------------------------------------

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
        "1) **Evidence check**: first read the F-test table and say which factors/terms have an effect (Yes/No at alpha).\n",
        "2) **Above/below the mean**: only for factors/terms supported by the F-test, use printed T-test coefficients to state which levels are above or below the global mean.\n",
        "3) **Interactions**: if an interaction is significant, explain that the effect depends on the combination of factors; avoid isolated main-effect claims.\n",
        "4) **Conclusion**: end with one plain-language sentence summarizing what matters most.\n",
        "5) **Boundary**: no equations, no unprinted comparisons, no new statistics."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) **Step 1 - Global evidence**: report F and p for each term and decide at alpha before interpreting coefficients.\n",
        "2) **Step 2 - Level profiling**: for significant factors, characterize levels by direction around the global mean (Intercept) using printed T-test coefficients only.\n",
        "3) **Step 3 - Interactions**: if an interaction is significant, interpret main effects conditionally and avoid marginal overstatement.\n",
        "4) **Step 4 - Synthesis**: give a concise decision-oriented summary based only on supported terms.\n",
        "5) **Boundary**: do not introduce unprinted post-hoc tests, effect sizes, pairwise comparisons, or diagnostics."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) **Statistical evidence artifact**: list F-test evidence by term first; identify supported terms at alpha and any interaction hierarchy.\n",
        "2) **Coefficient interpretation**: interpret T-test coefficients as \\(\\mu_i - \\mu\\) (Intercept = global mean) only for relevant/supported terms, respecting printed SE and p.\n",
        "3) **Interactions before main effects**: when interactions are supported, frame main effects as conditional and avoid standalone conclusions.\n",
        "4) **Magnitude & precision**: comment on residual error and precision only when printed; distinguish statistical from practical importance.\n",
        "5) **Unsupported claims**: no unprinted post-hoc tests, effect sizes, diagnostics, or causal claims."
      )
    )
  }

  # ---------------------------------------------------------------------------
  # How-to-read block
  # ---------------------------------------------------------------------------

  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (ANOVA)",
      "- **F-test first**: checks whether a factor/term affects the response overall.",
      "- **T-test second**: `(Intercept)` is the global mean; coefficients are deviations around this mean.",
      "- **Signs**: positive = above the mean; negative = below the mean, but interpret coefficients mainly for supported terms.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (ANOVA)",
      "- **F-test**: identifies global drivers (compare p to alpha) before reading coefficients.",
      "- **Deviation profile**: coefficients are deviations from the global mean (Intercept).",
      "- **Interactions**: interpret main effects conditionally when an interaction is significant; avoid unsupported pairwise contrasts.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (ANOVA)",
      "- **Contrasts**: deviation (sum-to-zero) coding; Intercept = global mean \\(\\mu\\).",
      "- **Omnibus vs partial**: F-test per term; coefficients test \\(H_0: \\mu_i - \\mu = 0\\).",
      "- **Hierarchy**: interpret main effects conditionally in presence of interactions; keep post-hoc claims out unless printed.",
      sep = "\n"
    )
  )

  # ---------------------------------------------------------------------------
  # Compose setup block
  # ---------------------------------------------------------------------------

  block_F <- trainer_core_wrap_block("", ftest_lines)

  block_T <- trainer_core_wrap_block(
    "",
    if (length(ttest_to_show)) {
      ttest_to_show
    } else {
      "(no T-test section detected)"
    }
  )

  ttest_title <- if (is_filtered) {
    "### T-test (filtered)"
  } else {
    "### T-test"
  }

  ttest_scope_msg <- trainer_core_ttest_scope_msg(
    t_req,
    requested,
    actually_shown
  )

  setup <- paste(
    paste0("- Significance threshold: p <= ", format(alpha), "."),
    "- Model type: ANOVA with sum-to-zero contrasts (coefficients are deviations from the global mean).",
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

  # ---------------------------------------------------------------------------
  # Build final prompt
  # ---------------------------------------------------------------------------

  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = "",
    output_requirements = output_reqs,
    show_verbatim       = FALSE
  )

  # ---------------------------------------------------------------------------
  # Return or generate
  # ---------------------------------------------------------------------------

  trainer_core_generate_or_return(
    prompt,
    llm_model = llm_model,
    generate = generate,
    llm_engine = llm_engine,
    ...
  )
}

#' Deprecated alias for `trainer_aovsum()`
#'
#' @rdname trainer_aovsum
#' @param aovsum_obj Deprecated name for `x`.
#' @export
trainer_AovSum <- function(aovsum_obj, ...) {
  .Deprecated("trainer_aovsum")
  trainer_aovsum(x = aovsum_obj, ...)
}
