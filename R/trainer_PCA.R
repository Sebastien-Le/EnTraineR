#' Trainer: Name a PCA dimension (FactoMineR::PCA) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to name and justify a
#' principal component (dimension) from a FactoMineR::PCA object. The function
#' never invents numbers: it passes verbatim excerpts from `summary(pca_obj)`
#' (Individuals/Variables) and `FactoMineR::dimdesc()` filtered at a given
#' significance threshold `proba`, and instructs how to read and name the axis.
#'
#' @param x A PCA object returned by \code{FactoMineR::PCA()}.
#' @param dimension Integer scalar; the dimension (component) to name (default 1).
#' @param proba Numeric in (0,1]; significance threshold used by
#'   \code{FactoMineR::dimdesc()} to characterize the dimension (default 0.05).
#' @param introduction Optional character string giving the study context.
#'   Defaults to a generic description.
#' @param audience One of \code{c("beginner","applied","advanced")}.
#' @param summary_only Logical; if TRUE, return a compact 3-bullet executive
#'   summary (uses \code{trainer_core_summary_only_block()}).
#' @param llm_model Character; model name for your generator backend
#'   (default \code{"llama3"}).
#' @param llm_engine Character; backend engine: "ollama", "gemini", or "none".
#' @param ... Passed to the selected LLM backend when `generate = TRUE`.
#' @param generate Logical; if TRUE, calls
#'   \code{trainer_core_generate_or_return()} and returns a list with
#'   \code{prompt}, \code{response}, and \code{model}. If FALSE, returns the
#'   prompt string.
#'
#' @section Privacy:
#' If `generate = TRUE` and `llm_engine` is not `"none"`, the prompt is sent
#' to the selected LLM backend. With external providers such as Gemini, this may
#' include excerpts of statistical outputs and user-provided context.
#'
#' @return An `entrainer_prompt` object. It behaves like a character string
#'   for `cat()`/printing and stores LLM metadata and response as attributes
#'   when `generate = TRUE`.
#' @export
#'
#'@examples
#' \dontrun{
#' # Example: decathlon (FactoMineR)
#' if (requireNamespace("FactoMineR", quietly = TRUE)) {
#' data(decathlon, package = "FactoMineR")
#'
#' res_pca <- FactoMineR::PCA(decathlon,
#'                 quanti.sup = 11:12,
#'                 quali.sup = 13,
#'                 graph = FALSE)
#'
#' intro <- "A study was conducted on decathlon athletes.
#' Performances on each event were measured and summarized by PCA."
#' intro <- gsub("\n", " ", intro); intro <- gsub("\\s+", " ", intro)
#'
#' prompt <- trainer_pca(res_pca,
#'                 dimension = 1,
#'                 proba = 0.05,
#'                 introduction = intro,
#'                 audience = "applied",
#'                 generate = FALSE)
#'
#' cat(prompt)
#'
#' res <- gemini_generate(prompt, compile_to = "html")
#' }
#' }
trainer_pca <- function(x,
                        dimension = 1L,
                        proba = 0.05,
                        introduction = NULL,
                        audience = c("beginner","applied","advanced"),
                        summary_only = FALSE,
                        llm_model = "llama3",
                        generate = FALSE,
                        llm_engine = c("ollama", "gemini", "none"),
                        ...) {

  pca_obj <- x

  audience <- match.arg(audience)
  proba <- trainer_core_check_probability(proba, "proba", include_upper = TRUE)
  summary_only <- trainer_core_check_flag(summary_only, "summary_only")
  generate <- trainer_core_check_flag(generate, "generate")
  llm_model <- trainer_core_check_string(llm_model, "llm_model")
  llm_engine <- match.arg(llm_engine)
  introduction <- trainer_core_check_optional_string(introduction, "introduction")

  if (is.null(pca_obj) || !inherits(pca_obj, "PCA")) {
    stop("`x` must be a 'PCA' object from FactoMineR::PCA().", call. = FALSE)
  }

  dimension <- trainer_core_check_dimension(dimension, trainer_core_max_dimensions(pca_obj))

  # ---- Core audience profile & header ---------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha = proba,
                                           summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # ---- Default introduction --------------------------------------------------
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- paste0(
      "We aim to name one principal component (dimension) from a PCA on quantitative variables. ",
      "The goal is to provide a short, sign-agnostic name and a concise justification based on printed outputs. ",
      "Variables retained in the dimdesc section are filtered at significance threshold p <= ", format(proba), "."
    )
  }

  # ---- Capture verbatim: summary(pca_obj) -----------------------------------
  sum_txt   <- trainer_core_capture(summary(pca_obj))
  sum_lines <- unlist(strsplit(sum_txt, "\n", fixed = TRUE), use.names = FALSE)

  # Use centralized extraction helper
  indiv_block <- trainer_core_extract_section(
    sum_lines,
    start_pat = "^\\s*Individuals\\b",
    stop_pats = c("^\\s*Variables\\b", "^\\s*Supplementary\\b", "^\\s*$")
  )
  var_block <- trainer_core_extract_section(
    sum_lines,
    start_pat = "^\\s*Variables\\b",
    stop_pats = c("^\\s*Supplementary\\b", "^\\s*$")
  )

  # ---- Capture verbatim: dimdesc() with proba -------------------------------
  dd <- try(FactoMineR::dimdesc(pca_obj, axes = dimension, proba = proba), silent = TRUE)
  dd_block <- character(0)
  if (!inherits(dd, "try-error")) {
    dim_key <- paste0("Dim.", as.integer(dimension))
    if (!is.null(dd[[dim_key]])) {
      dd_block <- trainer_core_capture(dd[[dim_key]])
    } else {
      dd_block <- trainer_core_capture(dd)
    }
  }

  # ---- How-to-read block (audience-specific) --------------------------------
  # IMPROVED: Explicitly mention "Latent Variable" or "Underlying concept" logic
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (PCA dimension naming)",
      "- Think of this axis as a new **'super-variable'** that summarizes the others.",
      "- Name the **underlying theme** that separates the variables on the positive side from those on the negative side.",
      "- **Check both poles**: positive correlations define one pole, negative correlations define the opposite pole.",
      paste0("- Variables in dimdesc are filtered at p <= ", format(proba), ". If dimdesc is empty, use structured evidence only as exploratory and low-confidence."),
      "- Individuals are illustrations only; do not base the name on specific persons.",
      "- Use very short sentences (<= 15 words). No new calculations.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (PCA dimension naming)",
      "- Name the **synthetic index** (latent factor) that drives the observed correlations.",
      "- **Identify the opposition**: explicitly contrast positive-pole variables with negative-pole variables before naming the axis.",
      "- Keep the name sign-agnostic (it represents the continuum, not just one end).",
      paste0("- Use dimdesc correlations at p <= ", format(proba), "; if dimdesc is empty, rely on structured axis evidence only with low confidence."),
      "- Add a 'so what' sentence only when the study context supports it.",
      "- No new calculations; use only printed material.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (PCA dimension naming)",
      "- Conceptualize the dimension as a **latent construct** (linear combination) explaining the variance.",
      "- **Interpret the structural opposition** based on the sign of correlations (positive vs. negative) before proposing a latent construct.",
      "- Ensure the label is sign-agnostic (orientation-free).",
      paste0("- Interpret dimdesc under p <= ", format(proba), "; prioritize variables with higher contribution/cos2 and check coherence."),
      "- You may add a brief stability note (sensitivity to threshold/sample), without new computations.",
      "- Individuals may be cited only as illustrations; they must not determine the axis name.",
      sep = "\n"
    )
  )

  # ---- Structured evidence from FactoMineR object ------------------------------
  structured_block <- paste0(
    "### Structured axis evidence (extracted from the PCA object)\n",
    trainer_core_factor_axis_evidence(
      pca_obj,
      dimension = as.integer(dimension),
      element_label = "variables",
      top_n = 12L
    )
  )

  # ---- Output requirements ---------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- paste0(
      "## Output requirements (SUMMARY-ONLY)\n",
      "- Provide ONLY 3 short bullets (<= 50 words total):\n",
      "  1) Final sign-agnostic name for PCA Dimension ", as.integer(dimension), ".\n",
      "  2) Main positive-vs-negative pole opposition, using only printed evidence.\n",
      "  3) Confidence/limitation: say if dimdesc is weak, empty, or only exploratory.\n",
      "- No new calculations; do not base the name on individuals."
    )
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "1) **Pole evidence**: list the positive pole and negative pole in simple words, using only printed variables.\n",
        "2) **Candidate names**: propose 2 sign-agnostic names for Dimension ", as.integer(dimension), " (2-4 words each), each justified by one printed association pattern.\n",
        "3) **Final name**: choose ONE final name (bold) and give one short sign-agnostic definition of the continuum.\n",
        "4) **Confidence**: if dimdesc is empty or few variables pass the threshold, say the name is exploratory/low-confidence rather than stopping.\n",
        "5) Use short sentences; no new numbers or calculations."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) **Pole evidence**: summarize positive and negative poles separately, prioritizing printed contribution/cos2 and dimdesc evidence.\n",
        "2) **Candidate names**: propose 3 sign-agnostic names for Dimension ", as.integer(dimension), " (2-4 words each), each tied to the pole opposition.\n",
        "3) **Final name**: choose ONE final name (bold) and define the latent continuum in one sentence.\n",
        "4) **Use case**: add one 'so what' sentence only if it follows from the study context; otherwise say what context is missing.\n",
        "5) **Confidence**: explicitly rate the naming evidence as strong/moderate/low based on coherence and printed evidence; no new numbers."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) **Statistical evidence artifact**: separate positive-pole variables, negative-pole variables, dominant contributions/cos2, and dimdesc evidence.\n",
        "2) **Candidate names**: propose 3 sign-agnostic labels and justify each from the printed structural opposition only.\n",
        "3) **Final construct**: choose ONE final name (bold), define the continuum, and state the central opposition.\n",
        "4) **Evidence strength**: discuss coherence, threshold sensitivity, and whether dimdesc is empty/few variables, without new computations.\n",
        "5) **Unsupported claims**: do not reconstruct values, infer causality, or let illustrative individuals determine the name."
      )
    )
  }

  # ---- Compose SETUP with verbatims -----------------------------------------
  dd_title  <- paste0("### dimdesc for Dimension ", as.integer(dimension),
                      " (significant at p <= ", format(proba), ")")
  sum_title <- "### summary() excerpts (context)"

  dd_chunk <- if (length(dd_block)) {
    paste0(dd_title, "\n```\n", dd_block, "\n```")
  } else {
    paste0(dd_title, "\n(not available)")
  }

  sum_chunk <- paste(
    sum_title,
    if (length(indiv_block)) paste0("#### Individuals\n```\n", paste(indiv_block, collapse = "\n"), "\n```") else "#### Individuals\n(not available)",
    if (length(var_block))   paste0("#### Variables\n```\n",   paste(var_block,   collapse = "\n"), "\n```") else "#### Variables\n(not available)",
    sep = "\n\n"
  )

  setup <- paste(
    paste0("- Target dimension: Dim. ", as.integer(dimension), "."),
    paste0("- Significance threshold for dimdesc: p <= ", format(proba), "."),
    "- Use variables to characterize both poles; individuals are illustrative only.",
    "",
    howto_block,
    "",
    structured_block,
    "",
    dd_chunk,
    "",
    sum_chunk,
    sep = "\n"
  )

  # ---- Build final prompt ----------------------------------------------------
  prompt <- trainer_core_build_prompt(
    header              = header,
    context             = introduction,
    setup               = setup,
    verbatim            = "",
    output_requirements = output_reqs,
    show_verbatim       = FALSE
  )

  # ---- Return or generate ----------------------------------------------------
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate, llm_engine = llm_engine, ...)
}

#' Deprecated alias for `trainer_pca()`
#'
#' @rdname trainer_pca
#' @param pca_obj Deprecated name for `x`.
#' @export
trainer_PCA <- function(pca_obj, ...) {
  .Deprecated("trainer_pca")
  trainer_pca(x = pca_obj, ...)
}
