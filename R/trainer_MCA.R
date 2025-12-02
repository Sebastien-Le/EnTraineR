#' Trainer: Name an MCA dimension (FactoMineR::MCA) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to name and justify a
#' Multiple Correspondence Analysis (MCA) dimension from a FactoMineR::MCA
#' object. The function never invents numbers: it passes verbatim excerpts from
#' \code{summary(mca_obj)} and \code{FactoMineR::dimdesc()} filtered at a given
#' significance threshold \code{proba}, and instructs how to read and name the axis.
#'
#' @param mca_obj A MCA object returned by \code{FactoMineR::MCA()}.
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
#' @examples
#' \dontrun{
#' # Example: tea (FactoMineR)
#' if (requireNamespace("FactoMineR", quietly = TRUE)) {
#'   data(tea, package = "FactoMineR")
#'   res_mca <- FactoMineR::MCA(tea, quanti.sup = 19, quali.sup = 20:36, graph = FALSE)
#'
#'   intro <- "A survey on tea consumption practices and contexts was summarized by MCA."
#'   intro <- gsub("\n", " ", intro); intro <- gsub("\\s+", " ", intro)
#'
#'   # Applied audience
#'   prompt <- trainer_MCA(res_mca,
#'                         dimension = 1,
#'                         proba = 0.01,
#'                         introduction = intro,
#'                         audience = "applied",
#'                         generate = FALSE)
#'   cat(prompt)
#'
#'   res <- gemini_generate(prompt, compile_to = "html")
#' }
#' }
trainer_MCA <- function(mca_obj,
                        dimension = 1L,
                        proba = 0.05,
                        introduction = NULL,
                        audience = c("beginner","applied","advanced"),
                        summary_only = FALSE,
                        llm_model = "llama3",
                        generate = FALSE) {

  audience <- match.arg(audience)

  if (is.null(mca_obj) || !inherits(mca_obj, "MCA"))
    stop("mca_obj must be an 'MCA' object from FactoMineR::MCA().")

  if (!is.numeric(proba) || length(proba) != 1L || is.na(proba) || proba <= 0 || proba > 1)
    stop("proba must be a single numeric value in (0, 1].")

  # ---- Core audience profile & header ---------------------------------------
  profile <- trainer_core_audience_profile(audience, alpha = proba,
                                           summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # ---- Default introduction --------------------------------------------------
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- paste0(
      "We aim to name one Multiple Correspondence Analysis (MCA) dimension. ",
      "The goal is to provide a short, sign-agnostic name and a concise justification based on printed outputs. ",
      "dimdesc is filtered at significance threshold p <= ", format(proba), "."
    )
  }

  # ---- Capture verbatim: summary(mca_obj) -----------------------------------
  sum_txt   <- trainer_core_capture(summary(mca_obj))
  sum_lines <- unlist(strsplit(sum_txt, "\n", fixed = TRUE), use.names = FALSE)

  # Use centralized extraction helper
  cats_block <- trainer_core_extract_section(
    sum_lines,
    start_pat = "^\\s*Categories\\b",
    stop_pats = c("^\\s*Categorical variables \\(eta2\\)\\b", "^\\s*Supplementary\\b", "^\\s*$")
  )
  eta_block <- trainer_core_extract_section(
    sum_lines,
    start_pat = "^\\s*Categorical variables \\(eta2\\)\\b",
    stop_pats = c("^\\s*Supplementary\\b", "^\\s*$")
  )
  supp_block <- trainer_core_extract_section(
    sum_lines,
    start_pat = "^\\s*Supplementary",
    stop_pats = c("^\\s*$")
  )

  # ---- Capture verbatim: dimdesc() with proba -------------------------------
  dd <- try(FactoMineR::dimdesc(mca_obj, axes = dimension, proba = proba), silent = TRUE)
  dd_block <- character(0)
  if (!inherits(dd, "try-error")) {
    dim_keys <- names(dd)
    target_regex <- paste0("^Dim[\\. ]?", as.integer(dimension), "$")
    key_idx <- which(grepl(target_regex, dim_keys))

    if (length(key_idx)) {
      dd_block <- trainer_core_capture(dd[[ key_idx[1] ]])
    } else {
      dd_block <- trainer_core_capture(dd)
    }
  }

  # ---- How-to-read block (audience-specific) --------------------------------
  # IMPROVED: Explicitly define the "Latent Variable/Profile" concept + Estimate signs
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (MCA dimension naming)",
      "- Think of this axis as a **hidden 'profile'** that separates respondents.",
      "- **Check the sign of the 'Estimate'**: positive values represent one profile, negative values represent the opposite profile.",
      "- Name the **theme** that opposes these two profiles (e.g., 'Modern vs Traditional').",
      paste0("- Variables in dimdesc are filtered at p <= ", format(proba), ". If none pass, say: 'inconclusive at this threshold'."),
      "- eta^2 indicates how much a variable explains the dimension (global link).",
      "- Use very short sentences (<= 15 words). No new calculations.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (MCA dimension naming)",
      "- Name the **synthetic behavioral pattern** (latent factor) that organizes the data.",
      "- **Identify the opposition**: Contrast the 'profile' defined by categories with positive Estimates vs. those with negative Estimates.",
      "- Find the common thread that links categories on the same pole.",
      paste0("- Use dimdesc (p <= ", format(proba), "); prefer variables with high eta^2 (link strength)."),
      "- Add one 'so what' sentence on how this axis can be used.",
      "- No new calculations; use only printed material.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (MCA dimension naming)",
      "- Conceptualize the dimension as a **latent categorical construct** explaining the inertia.",
      "- **Interpret the structural opposition** between the centroids of categories with positive vs. negative Estimates.",
      "- Ensure the label is sign-agnostic (orientation-free).",
      paste0("- Interpret dimdesc under p <= ", format(proba), "; prioritize variables with higher discrimination measures."),
      "- You may add a brief stability note (sensitivity to threshold/sample), without new computations.",
      "- Supplementary elements should be cited as archetypes only.",
      sep = "\n"
    )
  )

  # ---- Output requirements ---------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- trainer_core_summary_only_block(
      words_limit = 50,
      bullets = 3,
      label = paste0("MCA Dimension ", as.integer(dimension), " (naming)")
    )
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "1) Propose 2 candidate names for Dimension ", as.integer(dimension), " (2-4 words each), sign-agnostic.\n",
        "2) For each candidate, give 1 short sentence using ONLY printed categories.\n",
        "3) Choose ONE final name (bold) and give 1 short sign-agnostic definition.\n",
        "4) If no variables pass the threshold, say 'inconclusive at this threshold' and stop.\n",
        "5) Use short sentences; no new numbers or calculations."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) Propose 3 candidate names for Dimension ", as.integer(dimension), " (2-4 words each), sign-agnostic.\n",
        "2) For each candidate, give 1 practical sentence that justifies it by synthesizing the opposition between positive/negative Estimates.\n",
        "3) Choose ONE final name (bold) and provide a one-sentence definition that is sign-agnostic.\n",
        "4) Add one 'so what' sentence on how this axis can be used (e.g., segmentation, profiling).\n",
        "5) If few variables pass the threshold, acknowledge the limitation; no new numbers."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) Propose 3 candidate names for Dimension ", as.integer(dimension), " (2-4 words), sign-agnostic and driven by the dominant latent structure (eta^2/categories).\n",
        "2) Justify each name in 1 sentence referencing ONLY printed info and, when printed, the dimension variance explained.\n",
        "3) Choose ONE final name (bold) and provide a compact definition; note any observed coherence between categories on each pole.\n",
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
    if (length(cats_block)) paste0("#### Categories\n```\n", paste(cats_block, collapse = "\n"), "\n```") else "#### Categories\n(not available)",
    if (length(eta_block))  paste0("#### Categorical variables (eta^2)\n```\n", paste(eta_block, collapse = "\n"), "\n```") else "#### Categorical variables (eta^2)\n(not available)",
    if (length(supp_block)) paste0("#### Supplementary\n```\n", paste(supp_block, collapse = "\n"), "\n```") else NULL,
    sep = "\n\n"
  )

  setup <- paste(
    paste0("- Target dimension: Dim. ", as.integer(dimension), "."),
    paste0("- Significance threshold for dimdesc: p <= ", format(proba), "."),
    "- Use categories to characterize both poles; eta^2 indicates variable strength.",
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
