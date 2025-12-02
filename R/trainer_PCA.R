#' Trainer: Name a PCA dimension (FactoMineR::PCA) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to name and justify a
#' principal component (dimension) from a FactoMineR::PCA object. The function
#' never invents numbers: it passes verbatim excerpts from `summary(pca_obj)`
#' (Individuals/Variables) and `FactoMineR::dimdesc()` filtered at a given
#' significance threshold `proba`, and instructs how to read and name the axis.
#'
#' @param pca_obj A PCA object returned by \code{FactoMineR::PCA()}.
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
#' @param generate Logical; if TRUE, calls
#'   \code{trainer_core_generate_or_return()} and returns a list with
#'   \code{prompt}, \code{response}, and \code{model}. If FALSE, returns the
#'   prompt string.
#'
#' @return If \code{generate = FALSE}, a character prompt string.
#'   If \code{generate = TRUE}, a list with \code{prompt}, \code{response}, and \code{model}.
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
#' prompt <- trainer_PCA(res_pca,
#'                 dimension = 1,
#'                 proba = 0.05,
#'                 introduction = intro,
#'                 audience = "applied",
#'                 generate = FALSE)
#'
#' cat(prompt)
#' }
#' }
trainer_PCA <- function(pca_obj,
                        dimension = 1L,
                        proba = 0.05,
                        introduction = NULL,
                        audience = c("beginner","applied","advanced"),
                        summary_only = FALSE,
                        llm_model = "llama3",
                        generate = FALSE) {

  audience <- match.arg(audience)

  if (is.null(pca_obj) || !inherits(pca_obj, "PCA"))
    stop("pca_obj must be a 'PCA' object from FactoMineR::PCA().")

  if (!is.numeric(proba) || length(proba) != 1L || is.na(proba) || proba <= 0 || proba > 1)
    stop("proba must be a single numeric value in (0, 1].")

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
      "- **Check the sign**: Positive correlations pull one way, negative correlations pull the other.",
      paste0("- Variables in dimdesc are filtered at p <= ", format(proba), ". If none pass, say: 'inconclusive at this threshold'."),
      "- Individuals are illustrations only; do not base the name on specific persons.",
      "- Use very short sentences (<= 15 words). No new calculations.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (PCA dimension naming)",
      "- Name the **synthetic index** (latent factor) that drives the observed correlations.",
      "- **Identify the opposition**: Contrast the group of variables with positive correlations vs. those with negative correlations.",
      "- Keep the name sign-agnostic (it represents the continuum, not just one end).",
      paste0("- Use dimdesc correlations at p <= ", format(proba), "; prefer variables with higher contribution/cos2."),
      "- Add one 'so what' sentence on how this axis can be used.",
      "- No new calculations; use only printed material.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (PCA dimension naming)",
      "- Conceptualize the dimension as a **latent construct** (linear combination) explaining the variance.",
      "- **Interpret the structural opposition** based on the sign of correlations (positive vs. negative).",
      "- Ensure the label is sign-agnostic (orientation-free).",
      paste0("- Interpret dimdesc under p <= ", format(proba), "; prioritize variables with higher contribution/cos2 and check coherence."),
      "- You may add a brief stability note (sensitivity to threshold/sample), without new computations.",
      "- Individuals may be cited as archetypes; avoid any reconstruction beyond printed info.",
      sep = "\n"
    )
  )

  # ---- Output requirements ---------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = paste0("PCA Dimension ", as.integer(dimension), " (naming)")
    )
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "1) Propose 2 candidate names for Dimension ", as.integer(dimension), " (2-4 words each), sign-agnostic.\n",
        "2) For each candidate, give 1 short sentence using ONLY printed associations.\n",
        "3) Choose ONE final name (bold) and give 1 short sign-agnostic definition.\n",
        "4) If no variables pass the threshold, say 'inconclusive at this threshold' and stop.\n",
        "5) Use short sentences; no new numbers or calculations."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) Propose 3 candidate names for Dimension ", as.integer(dimension), " (2-4 words each), sign-agnostic.\n",
        "2) For each candidate, give 1 practical sentence that justifies it using ONLY printed associations (prefer higher contribution and cos2 when printed).\n",
        "3) Choose ONE final name (bold) and provide a one-sentence definition that is sign-agnostic.\n",
        "4) Add one 'so what' sentence on how this axis can be used (e.g., segmentation, monitoring, communication).\n",
        "5) If few variables pass the threshold, acknowledge the limitation; no new numbers."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) Propose 3 candidate names for Dimension ", as.integer(dimension), " (2-4 words), sign-agnostic and driven by the dominant loading/correlation pattern.\n",
        "2) Justify each name in 1 sentence referencing ONLY printed info (e.g., contributions, cos2, correlation signs) and, when printed, the dimension variance explained.\n",
        "3) Choose ONE final name (bold) and provide a compact definition; note any observed coherence between variables on each pole.\n",
        "4) Optionally add 1 brief stability remark (sensitivity to threshold/sample) without new computations.\n",
        "5) Do not reconstruct unprinted values; no new calculations."
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
  trainer_core_generate_or_return(prompt, llm_model = llm_model, generate = generate)
}
