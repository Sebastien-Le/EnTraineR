# R/trainer_core.R
# Core utilities for TraineR: audience, prompt building, capture, and parsing
# English-only; no invented numbers; no new statistics.

# ---- Tiny, general helpers ---------------------------------------------------

#' Collapse character vectors into a single string (newline-joined)
#' @param xs Character vector.
#' @return A single character string.
#' @keywords internal
#' @noRd
trainer_core_collapse <- function(xs) if (!length(xs)) "" else paste(xs, collapse = "\n")

#' Capture printed output as text (for htest, model summaries, etc.)
#' @param x Any R object.
#' @return A single character string with printed output.
#' @keywords internal
#' @noRd
trainer_core_capture <- function(x) paste(utils::capture.output(print(x)), collapse = "\n")

#' Null-coalescing operator
#' @param a Value or NULL.
#' @param b Fallback if a is NULL.
#' @return a if not NULL, else b.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Escape non-word characters for regex
#' @param x Character scalar.
#' @return Escaped string safe for regex.
#' @keywords internal
#' @noRd
trainer_core_esc <- function(x) gsub("([\\W])", "\\\\\\1", x, perl = TRUE)

#' Format alpha with 3 decimals
#' @param alpha Numeric.
#' @return Character.
#' @keywords internal
#' @noRd
trainer_core_fmt_alpha <- function(alpha) format(alpha, digits = 3)


# ---- Audience profile & prompt header ---------------------------------------

#' Build an audience profile (beginner / applied / advanced) with optional summary-only mode
#'
#' @param audience Character: one of c("beginner","applied","advanced").
#' @param alpha Numeric alpha (only to instruct the LLM; no computation).
#' @param summary_only Logical; if TRUE, enforce a short 3-bullet executive summary
#'   regardless of audience depth.
#'
#' @return List with flags, tone, and guardrails:
#'   - audience, summary_only, tone
#'   - show_verbatim, show_diagnostics
#'   - include_df, include_equations
#'   - max_bullets, max_words_takeaway
#'   - guard, alpha_round
#' @export
#'
#' @examples
#' trainer_core_audience_profile("applied", 0.05, summary_only = FALSE)
trainer_core_audience_profile <- function(audience = c("beginner","applied","advanced"),
                                          alpha = 0.05,
                                          summary_only = FALSE) {
  audience <- match.arg(audience)
  phr <- list(
    guard       = "NO INVENTED NUMBERS. Use only values present in the output.",
    alpha_round = paste0("Use alpha = ", trainer_core_fmt_alpha(alpha),
                         ". Round to 3 decimals; scientific notation for tiny p-values.")
  )
  prof <- switch(
    audience,
    beginner = list(
      show_verbatim = TRUE, show_diagnostics = TRUE,
      include_df = FALSE, include_equations = FALSE,
      max_bullets = 4, max_words_takeaway = 80,
      tone = "patient and plain English"
    ),
    applied = list(
      show_verbatim = TRUE, show_diagnostics = TRUE,
      include_df = TRUE, include_equations = FALSE,
      max_bullets = 4, max_words_takeaway = 60,
      tone = "concise and practical"
    ),
    advanced = list(
      show_verbatim = TRUE, show_diagnostics = TRUE,
      include_df = TRUE, include_equations = TRUE,
      max_bullets = 5, max_words_takeaway = 100,
      tone = "technical but clear"
    )
  )
  prof$audience <- audience
  prof$summary_only <- isTRUE(summary_only)
  prof$guard <- phr$guard
  prof$alpha_round <- phr$alpha_round
  prof
}

#' Build the standard header for prompts
#' @param profile List from \code{trainer_core_audience_profile()}.
#' @return Character header.
#' @export
#'
#' @examples
#' pr <- trainer_core_audience_profile("applied", 0.05)
#' cat(trainer_core_prompt_header(pr))
trainer_core_prompt_header <- function(profile) {
  paste0(
    "You are a clear, ", profile$tone, " statistician.\n",
    profile$guard, " ", profile$alpha_round, "\n"
  )
}

#' Utility: render a standard 3-bullet summary-only instruction
#'
#' @param words_limit Integer maximum total words (default 50).
#' @param bullets Integer number of bullets (default 3).
#' @param label Character label to include (e.g., the test name).
#' @return Character instruction block.
#' @export
#'
#' @examples
#' cat(trainer_core_summary_only_block(50, 3, "t-test"))
trainer_core_summary_only_block <- function(words_limit = 50,
                                            bullets = 3,
                                            label = "the analysis") {
  paste0(
    "## Output requirements (SUMMARY-ONLY)\n",
    "- Provide ONLY ", bullets, " short bullets (<= ", words_limit, " words total):\n",
    "  1) Direction and approximate magnitude in the study units.\n",
    "  2) Decision at alpha (name the test: ", label, ").\n",
    "  3) One concrete next step (e.g., robustness check, more data).\n",
    "- Do not explain methods; do not compute new statistics."
  )
}

#' Confidence level label helper
#'
#' Returns a short label for a confidence level, e.g. "95%".
#' If \code{conf_level} is \code{NA} or \code{NULL}, returns \code{fallback}.
#'
#' @param conf_level Numeric in (0,1), or NA/NULL.
#' @param fallback Character string to use when \code{conf_level} is missing.
#'   Default is \code{"the reported"}.
#'
#' @return A character scalar such as \code{"95\%"} or the fallback string.
#' @examples
#' trainer_core_conf_label(0.95)
#' trainer_core_conf_label(NA)
#' trainer_core_conf_label(NULL, fallback = "not reported")
#' @export
trainer_core_conf_label <- function(conf_level, fallback = "the reported") {
  if (is.null(conf_level) || is.na(conf_level)) return(fallback)
  paste0(round(100 * conf_level), "%")
}

# ---- Generic prompt skeleton & LLM call -------------------------------------

#' Assemble a standard prompt with common sections
#'
#' @param header Character (from \code{trainer_core_prompt_header}).
#' @param context Character (short paragraph).
#' @param setup Character (bullet list or short lines).
#' @param verbatim Character (raw printed output, will be fenced).
#' @param output_requirements Character (audience-tailored instructions).
#' @param show_verbatim Logical; include verbatim block.
#' @param verbatim_title Character section title.
#' @return Full prompt string.
#' @export
#'
#' @examples
#' trainer_core_build_prompt("H", "Context", "- a\n- b", "raw", "Reqs")
trainer_core_build_prompt <- function(header,
                                      context,
                                      setup,
                                      verbatim,
                                      output_requirements,
                                      show_verbatim = TRUE,
                                      verbatim_title = "Verbatim output") {
  paste0(
    header, "\n",
    "## 1) Context\n", context, "\n\n",
    "## 2) Test setup (for orientation)\n", setup, "\n\n",
    if (isTRUE(show_verbatim)) paste0("## 3) ", verbatim_title, "\n```\n", verbatim, "\n```\n\n") else "",
    output_requirements
  )
}

#' LLM generation helper for TraineR
#'
#' @description
#' Thin wrapper around the chosen LLM backend. By default, uses \pkg{ollamar}
#' if installed; otherwise returns only the prompt so the caller can still
#' inspect it without failing.
#'
#' @param model Character scalar, model name (e.g., "llama3").
#' @param prompt Character scalar, the prompt to send.
#' @param engine Character scalar, backend engine. Currently "ollamar" or "none".
#'   If "none" or if the backend is not available, returns the prompt only.
#' @param ... Passed to the backend generator.
#'
#' @return A list with elements \code{prompt}, \code{response}, \code{model},
#'   and \code{engine}. If the backend isn't available, \code{response} is \code{NULL}.
#' @export
trainer_core_llm_generate <- function(model, prompt, engine = c("ollamar", "none"), ...) {
  engine <- match.arg(engine)

  # If engine "none" is chosen explicitly, just return the prompt
  if (identical(engine, "none")) {
    return(list(prompt = prompt, response = NULL, model = model, engine = "none"))
  }

  # Default engine: ollamar (optional Suggests)
  if (!requireNamespace("ollamar", quietly = TRUE)) {
    # No backend installed -> return prompt only (don't error hard)
    return(list(
      prompt   = prompt,
      response = NULL,
      model    = model,
      engine   = "none",
      note     = "Package 'ollamar' not installed; returning prompt only."
    ))
  }

  # Call the backend
  resp <- tryCatch(
    ollamar::generate(model = model, prompt = prompt, output = "text", ...),
    error = function(e) structure(list(error = conditionMessage(e)), class = "trainer_llm_error")
  )

  list(prompt = prompt, response = resp, model = model, engine = "ollamar")
}


#' Generate or return a prompt, depending on `generate`
#'
#' @param prompt Character prompt to return or send.
#' @param llm_model Character model name.
#' @param generate Logical flag.
#' @return Character prompt or list(prompt, response, model).
#' @export
#'
#' @examples
#' trainer_core_generate_or_return("hello", "llama3", generate = FALSE)
trainer_core_generate_or_return <- function(prompt, llm_model = "llama3", generate = FALSE) {
  if (!generate) return(prompt)
  trainer_core_llm_generate(llm_model, prompt)
}


# ---- ANOVA/LinearModel text extraction & T-test filtering -------------------

#' Extract lines following a header (up to first blank line)
#' @param txt Printed object as a single string.
#' @param header Exact header text to search.
#' @return Character vector of lines until first blank line.
#' @export
#'
#' @examples
#' trainer_core_extract_block_after("Head\nA\n\nB", "Head")
trainer_core_extract_block_after <- function(txt, header) {
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  idx <- grep(paste0("^\\s*", header, "\\s*$"), lines)
  if (!length(idx)) return(character(0))
  out <- character(0)
  for (k in (idx[1] + 1L):length(lines)) {
    ln <- lines[k]
    if (grepl("^\\s*$", ln)) break
    out <- c(out, ln)
  }
  out
}

#' Filter T-test lines by requested factors (main and/or interactions)
#' @param tt_lines Character vector of T-test lines.
#' @param keep_factors Character vector of factor names or "A:B".
#' @param keep_intercept Logical; keep (Intercept) line.
#' @return Filtered character vector.
#' @export
#'
#' @examples
#' trainer_core_filter_ttest_by_factors(c("(Intercept)", "A - a", "A - b:B - c"), "A", TRUE)
trainer_core_filter_ttest_by_factors <- function(tt_lines, keep_factors = NULL, keep_intercept = TRUE) {
  if (!length(tt_lines)) return(character(0))

  # Detect header line position (robust to spacing)
  header_idx <- which(grepl("\\bEstimate\\b", tt_lines) |
                        grepl("Std\\.?\\s*Error", tt_lines) |
                        grepl("\\bt value\\b", tt_lines) |
                        grepl("Pr\\(>\\|?t\\|?\\)", tt_lines))
  header_pos <- if (length(header_idx)) min(header_idx) else NA_integer_

  is_intercept   <- grepl("^\\s*\\(Intercept\\)", tt_lines)
  is_interaction <- grepl(":", tt_lines)

  # Default: no filter -> keep all (but still ensure header is kept)
  if (is.null(keep_factors)) {
    keep <- rep(TRUE, length(tt_lines))
    if (!keep_intercept) keep <- keep & !is_intercept
    # force-keep header if detected
    if (!is.na(header_pos)) keep[seq_len(header_pos)] <- TRUE
    return(tt_lines[keep])
  }

  req_main  <- keep_factors[!grepl(":", keep_factors)]
  req_inter <- keep_factors[ grepl(":", keep_factors)]

  keep_main <- rep(FALSE, length(tt_lines))
  if (length(req_main)) {
    keep_main <- Reduce(`|`, lapply(req_main, function(f) {
      grepl(paste0("^\\s*", trainer_core_esc(f), "\\s-\\s"), tt_lines) & !is_interaction
    }))
  }

  keep_inter <- rep(FALSE, length(tt_lines))
  if (length(req_inter)) {
    keep_inter <- Reduce(`|`, lapply(req_inter, function(f) {
      parts <- strsplit(f, ":", fixed = TRUE)[[1]]
      if (length(parts) != 2) return(rep(FALSE, length(tt_lines)))
      a <- trainer_core_esc(trimws(parts[1])); b <- trainer_core_esc(trimws(parts[2]))
      grepl(
        paste0(
          "^\\s*", a, "\\s-\\s.*:\\s*", b, "\\s-\\s", "|",
          "^\\s*", b, "\\s-\\s.*:\\s*", a, "\\s-\\s"
        ),
        tt_lines
      )
    }))
  }

  keep <- keep_main | keep_inter
  if (keep_intercept) keep <- keep | is_intercept

  # Always keep header rows if detected
  if (!is.na(header_pos)) {
    header_keep <- seq_len(header_pos)
    keep[header_keep] <- TRUE
  }

  tt_lines[keep]
}

#' Detect main-effect factor names present in T-test lines (ignore interactions)
#' @param tt_lines Character vector.
#' @return Character vector of factor names.
#' @export
#'
#' @examples
#' trainer_core_detect_main_factors(c("A - a", "B - b:C - c"))
trainer_core_detect_main_factors <- function(tt_lines) {
  unique(stats::na.omit(sub(
    "^\\s*([^ -][^ -]*)\\s-.*$", "\\1",
    tt_lines[grepl("\\s-\\s", tt_lines) & !grepl(":", tt_lines)]
  )))
}

#' Determine which requested items were actually shown after filtering
#' @param req_main Character vector of requested main factors.
#' @param req_inter Character vector of requested interactions ("A:B").
#' @param ttest_filtered Filtered T-test lines.
#' @return Character vector of actually shown specifiers.
#' @export
#'
#' @examples
#' trainer_core_actually_shown("A", "A:B", c("A - a", "A - a : B - b"))
trainer_core_actually_shown <- function(req_main, req_inter, ttest_filtered) {
  found_main <- character(0)
  if (length(req_main)) {
    found_main <- req_main[vapply(req_main, function(f)
      any(grepl(paste0("^\\s*", trainer_core_esc(f), "\\s-\\s"), ttest_filtered) & !grepl(":", ttest_filtered)),
      logical(1)
    )]
  }
  found_inter <- character(0)
  if (length(req_inter)) {
    found_inter <- req_inter[vapply(req_inter, function(f) {
      parts <- strsplit(f, ":", fixed = TRUE)[[1]]
      if (length(parts) != 2) return(FALSE)
      a <- trainer_core_esc(trimws(parts[1])); b <- trainer_core_esc(trimws(parts[2]))
      any(grepl(
        paste0(
          "^\\s*", a, "\\s-\\s.*:\\s*", b, "\\s-\\s", "|",
          "^\\s*", b, "\\s-\\s.*:\\s*", a, "\\s-\\s"
        ),
        ttest_filtered
      ))
    }, logical(1))]
  }
  c(found_main, found_inter)
}

#' Scope message for T-test section based on requested & found factors
#' @param t_test User request vector.
#' @param requested Vector of normalized requested items.
#' @param actually_shown Vector from \code{trainer_core_actually_shown()}.
#' @return Single character scope message.
#' @export
#'
#' @examples
#' trainer_core_ttest_scope_msg(c("A"), c("A"), c("A"))
trainer_core_ttest_scope_msg <- function(t_test, requested, actually_shown) {
  if (is.null(t_test) || !length(requested)) {
    "T-test section shows all coefficients."
  } else if (length(actually_shown)) {
    paste0("T-test section is **restricted** to: ",
           paste(sort(unique(actually_shown)), collapse = ", "),
           " (plus the Intercept).")
  } else {
    paste0("Requested T-test factor(s) not found in output: ",
           paste(t_test, collapse = ", "),
           ". Showing only the Intercept.")
  }
}
