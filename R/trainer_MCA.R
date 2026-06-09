#' Trainer: Name an MCA dimension (FactoMineR::MCA) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to name and justify a
#' Multiple Correspondence Analysis (MCA) dimension from a FactoMineR::MCA
#' object. The function never invents numbers: it passes verbatim excerpts from
#' \code{summary(mca_obj)} and \code{FactoMineR::dimdesc()} filtered at a given
#' significance threshold \code{proba}, and instructs how to read and name the axis.
#'
#' @param x A MCA object returned by \code{FactoMineR::MCA()}.
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
#'   prompt <- trainer_mca(res_mca,
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
trainer_mca <- function(x,
                        dimension = 1L,
                        proba = 0.05,
                        introduction = NULL,
                        audience = c("beginner","applied","advanced"),
                        summary_only = FALSE,
                        llm_model = "llama3",
                        generate = FALSE,
                        llm_engine = c("ollama", "gemini", "none"),
                        ...) {

  mca_obj <- x

  audience <- match.arg(audience)
  proba <- trainer_core_check_probability(proba, "proba", include_upper = TRUE)
  summary_only <- trainer_core_check_flag(summary_only, "summary_only")
  generate <- trainer_core_check_flag(generate, "generate")
  llm_model <- trainer_core_check_string(llm_model, "llm_model")
  llm_engine <- match.arg(llm_engine)
  introduction <- trainer_core_check_optional_string(introduction, "introduction")

  if (is.null(mca_obj) || !inherits(mca_obj, "MCA")) {
    stop("`x` must be an 'MCA' object from FactoMineR::MCA().", call. = FALSE)
  }

  dimension <- trainer_core_check_dimension(dimension, trainer_core_max_dimensions(mca_obj))

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
      "- **Check both poles**: positive Estimates define one profile; negative Estimates define the opposite profile.",
      "- Name the **theme** that opposes these two profiles (e.g., 'Modern vs Traditional').",
      paste0("- Variables in dimdesc are filtered at p <= ", format(proba), ". If dimdesc is empty, use structured evidence only as exploratory and low-confidence."),
      "- eta^2 indicates how much a variable explains the dimension (global link).",
      "- Use very short sentences (<= 15 words). No new calculations.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (MCA dimension naming)",
      "- Name the **synthetic behavioral pattern** (latent factor) that organizes the data.",
      "- **Identify the opposition**: contrast categories with positive Estimates against categories with negative Estimates before naming the axis.",
      "- Find the common thread that links categories on the same pole.",
      paste0("- Use dimdesc (p <= ", format(proba), "); prefer variables with high eta^2, but treat empty dimdesc as low-confidence."),
      "- Add a 'so what' sentence only when the study context supports it.",
      "- No new calculations; use only printed material.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (MCA dimension naming)",
      "- Conceptualize the dimension as a **latent categorical construct** explaining the inertia.",
      "- **Interpret the structural opposition** between categories with positive vs. negative Estimates before proposing a latent categorical construct.",
      "- Ensure the label is sign-agnostic (orientation-free).",
      paste0("- Interpret dimdesc under p <= ", format(proba), "; prioritize variables with higher discrimination measures."),
      "- You may add a brief stability note (sensitivity to threshold/sample), without new computations.",
      "- Supplementary elements may contextualize or illustrate the axis, but active categories define the name.",
      sep = "\n"
    )
  )

  # ---- Structured evidence from FactoMineR object ------------------------------
  structured_block <- paste0(
    "### Structured axis evidence (extracted from the MCA object)\n",
    trainer_core_factor_axis_evidence(
      mca_obj,
      dimension = as.integer(dimension),
      element_label = "categories",
      top_n = 12L
    )
  )

  # ---- Output requirements ---------------------------------------------------
  if (isTRUE(profile$summary_only)) {
    output_reqs <- paste0(
      "## Output requirements (SUMMARY-ONLY)\n",
      "- Provide ONLY 3 short bullets (<= 50 words total):\n",
      "  1) Final sign-agnostic name for MCA Dimension ", as.integer(dimension), ".\n",
      "  2) Main positive-vs-negative profile opposition, using only printed categories.\n",
      "  3) Confidence/limitation: say if dimdesc is weak, empty, or only exploratory.\n",
      "- No new calculations; supplementary elements may illustrate but not define the name."
    )
  } else {
    output_reqs <- switch(
      profile$audience,
      "beginner" = paste0(
        "## Output requirements (BEGINNER)\n",
        "1) **Pole evidence**: list the positive profile and negative profile in simple words, using only printed categories.\n",
        "2) **Candidate names**: propose 2 sign-agnostic names for Dimension ", as.integer(dimension), " (2-4 words each), each justified by the profile opposition.\n",
        "3) **Final name**: choose ONE final name (bold) and give one short sign-agnostic definition of the continuum/profile.\n",
        "4) **Confidence**: if dimdesc is empty or few categories pass the threshold, say the name is exploratory/low-confidence rather than stopping.\n",
        "5) Use short sentences; no new numbers or calculations."
      ),
      "applied" = paste0(
        "## Output requirements (APPLIED)\n",
        "1) **Pole evidence**: summarize positive and negative profiles separately, prioritizing printed categories and eta^2 evidence.\n",
        "2) **Candidate names**: propose 3 sign-agnostic names for Dimension ", as.integer(dimension), " (2-4 words each), each tied to the positive/negative profile opposition.\n",
        "3) **Final name**: choose ONE final name (bold) and define the latent profile continuum in one sentence.\n",
        "4) **Use case**: add one 'so what' sentence only if it follows from the study context; otherwise say what context is missing.\n",
        "5) **Confidence**: explicitly rate the naming evidence as strong/moderate/low based on coherence and printed evidence; no new numbers."
      ),
      "advanced" = paste0(
        "## Output requirements (ADVANCED)\n",
        "1) **Statistical evidence artifact**: separate positive-profile categories, negative-profile categories, eta^2/global variable links, and dimdesc evidence.\n",
        "2) **Candidate names**: propose 3 sign-agnostic labels and justify each from the printed categorical opposition only.\n",
        "3) **Final construct**: choose ONE final name (bold), define the continuum/profile, and state the central opposition.\n",
        "4) **Evidence strength**: discuss coherence, threshold sensitivity, and whether dimdesc is empty/few categories, without new computations.\n",
        "5) **Unsupported claims**: do not reconstruct values, infer causality, or let supplementary elements determine the name."
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

#' Deprecated alias for `trainer_mca()`
#'
#' @rdname trainer_mca
#' @param mca_obj Deprecated name for `x`.
#' @export
trainer_MCA <- function(mca_obj, ...) {
  .Deprecated("trainer_mca")
  trainer_mca(x = mca_obj, ...)
}
