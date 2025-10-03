#' Trainer: Name an MCA dimension (FactoMineR::MCA) with an LLM-ready prompt
#'
#' Builds an English-only, audience-tailored prompt to name and justify a
#' Multiple Correspondence Analysis (MCA) dimension from a FactoMineR::MCA
#' object. The function never invents numbers: it passes verbatim excerpts from
#' \code{summary(mca_obj)} (categories, eta^2, supplementary parts) and
#' \code{FactoMineR::dimdesc()} filtered at significance threshold \code{proba},
#' then instructs how to read and name the axis.
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
#'   pr <- trainer_MCA(res_mca, dimension = 1, proba = 0.01,
#'                     introduction = intro, audience = "applied",
#'                     generate = FALSE)
#'   cat(pr)
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

  # ---- Audience profile & header --------------------------------------------
  # We pass 'alpha = proba' in the profile to keep the same vocabulary
  profile <- trainer_core_audience_profile(audience, alpha = proba,
                                           summary_only = summary_only)
  header  <- trainer_core_prompt_header(profile)

  # ---- Default introduction --------------------------------------------------
  if (is.null(introduction) || !nzchar(introduction)) {
    introduction <- paste0(
      "We aim to name one Multiple Correspondence Analysis (MCA) dimension. ",
      "Provide a short, sign-agnostic name and justify it from the printed outputs only. ",
      "dimdesc is filtered at p <= ", format(proba), "."
    )
  }

  # ---- Capture verbatim: summary(mca_obj) -----------------------------------
  sum_txt   <- trainer_core_capture(summary(mca_obj))
  sum_lines <- unlist(strsplit(sum_txt, "\n", fixed = TRUE), use.names = FALSE)

  extract_section <- function(lines, start_pat, stop_pats) {
    i <- which(grepl(start_pat, lines, perl = TRUE))
    if (!length(i)) return(character(0))
    start <- i[1]
    stop_idx <- length(lines) + 1L
    for (sp in stop_pats) {
      j_all <- which(grepl(sp, lines, perl = TRUE))
      j <- j_all[j_all > start]
      if (length(j)) stop_idx <- min(stop_idx, j[1])
    }
    out <- lines[start:(stop_idx - 1L)]
    while (length(out) && grepl("^\\s*$", out[length(out)])) out <- out[-length(out)]
    out
  }

  # Pick MCA-relevant blocks commonly printed by summary()
  cats_block <- extract_section(
    sum_lines,
    start_pat = "^\\s*Categories\\b",
    stop_pats = c("^\\s*Categorical variables \\(eta2\\)\\b",
                  "^\\s*Supplementary\\b",
                  "^\\s*$")
  )
  eta_block <- extract_section(
    sum_lines,
    start_pat = "^\\s*Categorical variables \\(eta2\\)\\b",
    stop_pats = c("^\\s*Supplementary categories\\b",
                  "^\\s*Supplementary categorical variables \\(eta2\\)\\b",
                  "^\\s*Supplementary continuous variable\\b",
                  "^\\s*$")
  )
  supp_cat_block <- extract_section(
    sum_lines,
    start_pat = "^\\s*Supplementary categories\\b",
    stop_pats = c("^\\s*Supplementary categorical variables \\(eta2\\)\\b",
                  "^\\s*Supplementary continuous variable\\b",
                  "^\\s*$")
  )
  supp_eta_block <- extract_section(
    sum_lines,
    start_pat = "^\\s*Supplementary categorical variables \\(eta2\\)\\b",
    stop_pats = c("^\\s*Supplementary continuous variable\\b", "^\\s*$")
  )
  supp_cont_block <- extract_section(
    sum_lines,
    start_pat = "^\\s*Supplementary continuous variable\\b",
    stop_pats = c("^\\s*$")
  )

  # ---- Capture verbatim: dimdesc() with proba --------------------------------
  dd <- try(FactoMineR::dimdesc(mca_obj, axes = dimension, proba = proba), silent = TRUE)
  dd_block <- character(0)
  if (!inherits(dd, "try-error")) {
    # MCA dimdesc uses backtick style names or "Dim X"
    dim_keys <- names(dd)
    # Accept "Dim 1", "Dim.1", or similar
    target <- paste0("Dim", if (grepl("^\\d+$", as.character(dimension))) " " else ".", as.integer(dimension))
    # Find a key that matches either "Dim 1" or "Dim.1"
    key_idx <- which(dim_keys %in% c(paste0("Dim ", as.integer(dimension)),
                                     paste0("Dim.", as.integer(dimension))))
    if (length(key_idx)) {
      dd_block <- trainer_core_capture(dd[[ key_idx[1] ]])
    } else {
      # Fallback: print entire dimdesc object (will include all dims)
      dd_block <- trainer_core_capture(dd)
    }
  }

  # ---- How-to-read block (audience-specific) ---------------------------------
  # MCA specifics: eta^2 (share of variance of the dimension explained by a categorical variable),
  # categories' v.test/Estimate indicate the pole and strength; orientation is arbitrary.
  howto_block <- switch(
    profile$audience,
    "beginner" = paste(
      "### How to read (MCA dimension naming)",
      "- Name the axis from the *categories* that load on its two opposite poles.",
      "- Orientation is arbitrary: pick a sign-agnostic name that still makes sense if reversed.",
      paste0("- dimdesc is filtered at p <= ", format(proba), "; use only printed associations."),
      "- eta^2 indicates how much of the dimension is explained by a categorical variable.",
      "- Do not compute anything new; rely only on printed outputs.",
      sep = "\n"
    ),
    "applied" = paste(
      "### How to read (MCA dimension naming)",
      "- Derive a concise label from coherent groups of categories at each pole.",
      "- Keep the name sign-agnostic (works if the axis is flipped).",
      paste0("- Use dimdesc (p <= ", format(proba), ") and, if helpful, the highest eta^2 variables as umbrellas."),
      "- Supplementary variables/categories can illustrate patterns (do not drive the name).",
      "- No new calculations; printed info only.",
      sep = "\n"
    ),
    "advanced" = paste(
      "### How to read (MCA dimension naming)",
      "- Emphasize the dominant categorical structures (eta^2) and the most characteristic categories (v.test/Estimate signs).",
      "- Provide a sign-agnostic name; mention pole tendencies without inventing magnitudes.",
      paste0("- Respect the stated threshold in dimdesc (p <= ", format(proba), "); no inference beyond prints."),
      "- Supplementary (continuous/qualitative) variables may serve as archetypal illustrations only.",
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
    output_reqs <- paste0(
      "## Output requirements (", toupper(profile$audience), ")\n",
      "1) Propose **3 candidate names** for Dimension ", as.integer(dimension),
      " (2-4 words each), based on categories at both poles and high-eta^2 variables (printed only).\n",
      "2) For each candidate, add **one sentence** justifying it using the printed categories/variables (no new numbers).\n",
      "3) Choose **ONE final name** (bold) and provide a **one-sentence definition** that is sign-agnostic.\n",
      "4) *(Optional)* Provide **2 brief pole archetypes** (left/right) with only printed hints (no invented magnitudes).\n",
      "5) Keep it concise and domain-appropriate; do not compute anything new."
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

  # Build summary chunk with typical MCA blocks if available
  sum_parts <- c(
    if (length(cats_block))       paste0("#### Categories\n```\n",       paste(cats_block,       collapse = "\n"), "\n```") else NULL,
    if (length(eta_block))        paste0("#### Categorical variables (eta^2)\n```\n", paste(eta_block, collapse = "\n"), "\n```") else NULL,
    if (length(supp_cat_block))   paste0("#### Supplementary categories\n```\n",   paste(supp_cat_block,   collapse = "\n"), "\n```") else NULL,
    if (length(supp_eta_block))   paste0("#### Supplementary categorical variables (eta^2)\n```\n", paste(supp_eta_block, collapse = "\n"), "\n```") else NULL,
    if (length(supp_cont_block))  paste0("#### Supplementary continuous variable(s)\n```\n", paste(supp_cont_block, collapse = "\n"), "\n```") else NULL
  )
  sum_chunk <- paste(c(sum_title, sum_parts), collapse = "\n\n")
  if (!nzchar(sum_chunk)) sum_chunk <- paste(sum_title, "(not available)", sep = "\n")

  setup <- paste(
    paste0("- Target dimension: Dim. ", as.integer(dimension), "."),
    paste0("- dimdesc significance threshold: p <= ", format(proba), "."),
    "- Use categories at both poles; eta^2 highlights umbrella variables (share of dimension variance).",
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
