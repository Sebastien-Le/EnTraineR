# R/trainer_core.R
# Core utilities for EntraineR: audience, prompt building, capture, and parsing
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


#' Construct an EntraineR prompt/result object
#' @keywords internal
#' @noRd
new_entrainer_prompt <- function(prompt,
                                 response = NULL,
                                 model = NULL,
                                 engine = "none",
                                 generated = FALSE) {
  prompt <- trainer_core_check_string(prompt, "prompt")
  generated <- trainer_core_check_flag(generated, "generated")
  structure(
    prompt,
    response = response,
    model = model,
    engine = engine,
    generated = generated,
    class = c("entrainer_prompt", "character")
  )
}

#' Construct a stable LLM response object
#' @keywords internal
#' @noRd
new_entrainer_response <- function(text,
                                   model = NULL,
                                   engine = "gemini",
                                   prompt = NULL,
                                   compile_to = c("none", "html", "docx"),
                                   output_path = NULL) {
  compile_to <- match.arg(compile_to)
  text <- trainer_core_check_string(text, "text")
  out <- list(
    text = text,
    markdown = text,
    model = model,
    engine = engine,
    prompt = prompt,
    compile_to = compile_to,
    output_path = output_path,
    html_path = if (identical(compile_to, "html")) output_path else NULL,
    docx_path = if (identical(compile_to, "docx")) output_path else NULL
  )
  structure(out, class = "entrainer_response")
}


#' Quote literal text for regex using \\Q...\\E (handles spaces, (), +, etc.)
#' @param x Character scalar.
#' @return Quoted string safe for regex (requires perl=TRUE).
#' @keywords internal
#' @noRd
trainer_core_quote <- function(x) paste0("\\Q", x, "\\E")

#' Format alpha with 3 decimals
#' @param alpha Numeric.
#' @return Character.
#' @keywords internal
#' @noRd
trainer_core_fmt_alpha <- function(alpha) format(alpha, digits = 3)





#' Check a non-empty character scalar
#' @param x Value to check.
#' @param arg Argument name used in error messages.
#' @param allow_null Logical; whether NULL is accepted.
#' @return A character scalar or NULL.
#' @keywords internal
#' @noRd
trainer_core_check_string <- function(x, arg = deparse(substitute(x)), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-empty character scalar.", call. = FALSE)
  }
  x
}

#' Check an optional character scalar
#' @keywords internal
#' @noRd
trainer_core_check_optional_string <- function(x,
                                               arg = deparse(substitute(x)),
                                               empty_is_null = TRUE) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be NULL or a character scalar.", call. = FALSE)
  }

  if (!nzchar(x) && isTRUE(empty_is_null)) {
    return(NULL)
  }

  x
}

#' Check a logical scalar
#' @param x Value to check.
#' @param arg Argument name used in error messages.
#' @return TRUE or FALSE.
#' @keywords internal
#' @noRd
trainer_core_check_flag <- function(x, arg = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

#' Check a finite numeric scalar
#' @keywords internal
#' @noRd
trainer_core_check_number <- function(x, arg = deparse(substitute(x)), lower = -Inf, upper = Inf,
                                      include_lower = TRUE, include_upper = TRUE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (ok) {
    ok <- if (include_lower) x >= lower else x > lower
    ok <- ok && if (include_upper) x <= upper else x < upper
  }
  if (!ok) {
    left <- if (include_lower) "[" else "("
    right <- if (include_upper) "]" else ")"
    stop("`", arg, "` must be a single finite numeric value in ",
         left, lower, ", ", upper, right, ".", call. = FALSE)
  }
  x
}

#' Check a probability-like scalar
#' @keywords internal
#' @noRd
trainer_core_check_probability <- function(x, arg = deparse(substitute(x)),
                                           lower = 0, upper = 1,
                                           include_upper = FALSE) {
  trainer_core_check_number(
    x,
    arg = arg,
    lower = lower,
    upper = upper,
    include_lower = FALSE,
    include_upper = include_upper
  )
}

#' Check a positive integer scalar
#' @keywords internal
#' @noRd
trainer_core_check_positive_integer <- function(x, arg = deparse(substitute(x))) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == as.integer(x) && x >= 1L
  if (!ok) {
    stop("`", arg, "` must be a single positive integer.", call. = FALSE)
  }
  as.integer(x)
}

#' Check dimension against the dimensions available in a FactoMineR result
#' @keywords internal
#' @noRd
trainer_core_check_dimension <- function(dimension, max_dim = NULL) {
  dimension <- trainer_core_check_positive_integer(dimension, "dimension")
  if (!is.null(max_dim) && is.finite(max_dim) && dimension > max_dim) {
    stop("`dimension` must be <= ", max_dim, " for this object.", call. = FALSE)
  }
  dimension
}

#' Infer the maximum number of dimensions from a FactoMineR result
#' @keywords internal
#' @noRd
trainer_core_max_dimensions <- function(x) {
  candidates <- list(
    tryCatch(ncol(x$ind$coord), error = function(e) NULL),
    tryCatch(nrow(x$eig), error = function(e) NULL),
    tryCatch(ncol(x$var$coord), error = function(e) NULL)
  )
  candidates <- unlist(candidates)
  candidates <- candidates[is.finite(candidates) & !is.na(candidates)]
  if (!length(candidates)) return(NULL)
  as.integer(max(candidates))
}

#' Check an htest object and, optionally, its printed method
#' @keywords internal
#' @noRd
trainer_core_check_htest <- function(x, arg = deparse(substitute(x)), expected_method = NULL) {
  if (is.null(x) || !inherits(x, "htest")) {
    stop("`", arg, "` must be an object of class `htest`.", call. = FALSE)
  }
  if (!is.null(expected_method)) {
    method <- x$method %||% ""
    if (!grepl(expected_method, method, ignore.case = TRUE, perl = TRUE)) {
      stop("`", arg, "` does not look like the expected test. Expected method matching `",
           expected_method, "`; actual method: `", method, "`.", call. = FALSE)
    }
  }
  invisible(x)
}

#' Human-readable label for alternative hypothesis
#' @param alternative character: "two.sided", "less", "greater"
#' @return character label
#' @keywords internal
#' @noRd
trainer_core_alt_label <- function(alternative) {
  alt <- tolower(alternative %||% "two.sided")
  switch(
    alt,
    "less"      = "one-sided (less)",
    "greater"   = "one-sided (greater)",
    "two.sided" = "two-sided",
    alternative
  )
}

#' Wrap a code block with an optional title
#' @param title character title
#' @param lines character vector content
#' @return single character with fenced code block
#' @keywords internal
#' @noRd
trainer_core_wrap_block <- function(title, lines) {
  if (!length(lines)) return(paste(title, "(not available)", sep = "\n"))
  paste0(
    if (nzchar(title)) paste0(title, "\n") else "",
    "```\n", trainer_core_collapse(lines), "\n```"
  )
}

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
#' @keywords internal
#' @noRd
#'
#' @examples
#' trainer_core_audience_profile("applied", 0.05, summary_only = FALSE)
trainer_core_audience_profile <- function(audience = c("beginner","applied","advanced"),
                                          alpha = 0.05,
                                          summary_only = FALSE) {
  audience <- match.arg(audience)
  alpha <- trainer_core_check_probability(alpha, "alpha")
  summary_only <- trainer_core_check_flag(summary_only, "summary_only")
  phr <- list(
    guard = "NO INVENTED NUMBERS. Use only values explicitly provided in this prompt, either in verbatim output or in structured evidence blocks. Do not compute unprinted values. Separate printed evidence, statistical decision, interpretation, and limits.",
    alpha_round = paste0(
      "Use alpha = ", trainer_core_fmt_alpha(alpha),
      ". Preserve printed values and inequality signs whenever possible; do not add precision beyond the printed output."
    )
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
#' @keywords internal
#' @noRd
#'
#' @examples
#' pr <- trainer_core_audience_profile("applied", 0.05)
#' cat(trainer_core_prompt_header(pr))
trainer_core_prompt_header <- function(profile) {
  paste0(
    "You are a clear, ", profile$tone, " statistician.\n",
    profile$guard, " ", profile$alpha_round, "\n",
    "Use an evidence-first discipline: identify the statistical object, the printed evidence used, the decision rule, and claims that the output does not support.\n"
  )
}

#' Utility: render a standard 3-bullet summary-only instruction
#'
#' @param words_limit Integer maximum total words (default 50).
#' @param bullets Integer number of bullets (default 3).
#' @param label Character label to include (e.g., the test name).
#' @return Character instruction block.
#' @keywords internal
#' @noRd
#'
#' @examples
#' cat(trainer_core_summary_only_block(50, 3, "t-test"))
trainer_core_summary_only_block <- function(words_limit = 50,
                                            bullets = 3,
                                            label = "the analysis") {
  paste0(
    "## Output requirements (SUMMARY-ONLY)\n",
    "- Provide ONLY ", bullets, " short bullets (<= ", words_limit, " words total):\n",
    "  1) Statistical object and decision/name (name the analysis: ", label, ").\n",
    "  2) Main printed evidence pattern: direction, opposition, or driver ONLY if supported by printed values.\n",
    "  3) Key limitation or next step; say when the printed output is insufficient.\n",
    "- Do not explain methods; do not compute new statistics; do not invent practical implications."
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
#' @keywords internal
#' @noRd
trainer_core_conf_label <- function(conf_level, fallback = "the reported") {
  if (is.null(conf_level) ||
      length(conf_level) != 1L ||
      is.na(conf_level) ||
      !is.finite(conf_level)) {
    return(fallback)
  }
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
#' @keywords internal
#' @noRd
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

#' LLM generation helper for EntraineR
#'
#' @description
#' Thin wrapper around the chosen LLM backend. Use `engine = "none"` to
#' keep generation disabled and return the prompt structure without contacting
#' a backend.
#'
#' @param model Character scalar, model name (e.g., "llama3").
#' @param prompt Character scalar, the prompt to send.
#' @param engine Character scalar, backend engine: "ollama", "gemini", or "none".
#'   If "none", no backend is contacted.
#' @param ... Passed to the backend generator.
#'
#' @section Privacy:
#' If `generate = TRUE` and `engine` is not `"none"`, the prompt is sent to
#' the selected LLM backend. With external providers such as Gemini, this may
#' include excerpts of statistical outputs and user-provided context.
#'
#' @return A list with elements \code{prompt}, \code{response}, \code{model},
#'   and \code{engine}. If \code{engine = "none"}, \code{response} is \code{NULL}.
#' @keywords internal
#' @noRd
trainer_core_llm_generate <- function(model = "llama3",
                                      prompt,
                                      engine = c("ollama", "gemini", "none"),
                                      ...) {
  engine <- match.arg(engine)
  model <- trainer_core_check_string(model, "model")
  prompt <- trainer_core_check_string(prompt, "prompt")

  dots <- list(...)

  if (identical(engine, "none")) {
    return(new_entrainer_prompt(
      prompt = prompt,
      response = NULL,
      model = model,
      engine = "none",
      generated = TRUE
    ))
  }

  if (identical(engine, "ollama")) {
    if (!requireNamespace("ollamar", quietly = TRUE)) {
      stop(
        "Package `ollamar` is required for `llm_engine = 'ollama'`.",
        call. = FALSE
      )
    }

    # Arguments meaningful for Gemini but not for Ollama.
    # Passing them to ollamar::generate() can produce errors such as:
    # "Invalid options: api_key".
    gemini_only_args <- c(
      "api_key",
      "base_url",
      "compile_to",
      "open",
      "output_path",
      "force_markdown",
      "max_output_tokens",
      "stop_sequences",
      "timeout",
      "max_tries",
      "backoff_base",
      "backoff_cap"
    )

    ignored <- intersect(names(dots), gemini_only_args)

    if (length(ignored)) {
      warning(
        "The following argument(s) were ignored because `llm_engine = 'ollama'`: ",
        paste(ignored, collapse = ", "),
        call. = FALSE
      )

      dots <- dots[setdiff(names(dots), ignored)]
    }

    resp <- tryCatch(
      do.call(
        ollamar::generate,
        c(
          list(
            model = model,
            prompt = prompt,
            output = "text"
          ),
          dots
        )
      ),
      error = function(e) {
        stop(
          "Ollama generation failed: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    return(new_entrainer_prompt(
      prompt = prompt,
      response = resp,
      model = model,
      engine = "ollama",
      generated = TRUE
    ))
  }

  if (identical(engine, "gemini")) {
    # Arguments meaningful for Ollama but not for Gemini.
    # This prevents accidental forwarding of Ollama model options to
    # gemini_generate().
    ollama_only_args <- c(
      "num_ctx",
      "num_predict",
      "num_keep",
      "repeat_last_n",
      "repeat_penalty",
      "mirostat",
      "mirostat_eta",
      "mirostat_tau",
      "penalize_newline",
      "tfs_z",
      "typical_p",
      "presence_penalty",
      "frequency_penalty"
    )

    ignored <- intersect(names(dots), ollama_only_args)

    if (length(ignored)) {
      warning(
        "The following argument(s) were ignored because `llm_engine = 'gemini'`: ",
        paste(ignored, collapse = ", "),
        call. = FALSE
      )

      dots <- dots[setdiff(names(dots), ignored)]
    }

    resp <- tryCatch(
      do.call(
        gemini_generate,
        c(
          list(
            prompt = prompt,
            model = model
          ),
          dots
        )
      ),
      error = function(e) {
        stop(
          "Gemini generation failed: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    return(new_entrainer_prompt(
      prompt = prompt,
      response = resp,
      model = model,
      engine = "gemini",
      generated = TRUE
    ))
  }

  stop("Unsupported LLM engine: ", engine, call. = FALSE)
}
#' Generate or return a prompt, depending on `generate`
#'
#' @param prompt Character prompt to return or send.
#' @param llm_model Character model name.
#' @param generate Logical flag.
#' @param llm_engine Character backend engine: "ollama", "gemini", or "none".
#' @param ... Passed to the backend generator.
#' @return Character prompt or list(prompt, response, model, engine).
#' @keywords internal
#' @noRd
#'
#' @examples
#' trainer_core_generate_or_return("hello", "llama3", generate = FALSE)
trainer_core_generate_or_return <- function(prompt,
                                            llm_model = "llama3",
                                            generate = FALSE,
                                            llm_engine = c("ollama", "gemini", "none"),
                                            ...) {
  prompt <- trainer_core_check_string(prompt, "prompt")
  llm_model <- trainer_core_check_string(llm_model, "llm_model")
  generate <- trainer_core_check_flag(generate, "generate")
  llm_engine <- match.arg(llm_engine)

  if (!generate) {
    return(new_entrainer_prompt(
      prompt = prompt,
      response = NULL,
      model = llm_model,
      engine = "none",
      generated = FALSE
    ))
  }

  trainer_core_llm_generate(
    model = llm_model,
    prompt = prompt,
    engine = llm_engine,
    ...
  )
}

#' Print an EntraineR prompt/result compactly
#' @param x Object returned by an EntraineR trainer.
#' @param ... Unused.
#' @return Invisibly returns x.
#' @export
print.entrainer_prompt <- function(x, ...) {
  response <- attr(x, "response", exact = TRUE)
  generated <- isTRUE(attr(x, "generated", exact = TRUE))
  engine <- attr(x, "engine", exact = TRUE) %||% "none"
  model <- attr(x, "model", exact = TRUE) %||% "unknown"

  if (generated && !is.null(response)) {
    cat("<EntraineR generated response>\n", sep = "")
    cat("Engine: ", engine, "\n", sep = "")
    cat("Model: ", model, "\n\n", sep = "")
    cat(as.character(response), sep = "")
  } else {
    cat(as.character(x), sep = "")
  }
  invisible(x)
}

#' Backward-compatible print method name
#' @param x Object returned by older EntraineR generators.
#' @param ... Unused.
#' @return Invisibly returns x.
#' @export
print.entrainer_llm_result <- function(x, ...) {
  print.entrainer_prompt(x, ...)
}

#' Convert an EntraineR response to character
#' @param x Object returned by gemini_generate().
#' @param ... Unused.
#' @return Generated text.
#' @export
as.character.entrainer_response <- function(x, ...) {
  x$text %||% x$markdown %||% ""
}

#' Print an EntraineR response compactly
#' @param x Object returned by gemini_generate().
#' @param ... Unused.
#' @return Invisibly returns x.
#' @export
print.entrainer_response <- function(x, ...) {
  cat(as.character(x), sep = "")
  if (!is.null(x$output_path)) {
    cat("\n\n[Written to: ", x$output_path, "]\n", sep = "")
  }
  invisible(x)
}

#' Extract the response from an EntraineR prompt/result object
#' @param x Object returned by an EntraineR trainer.
#' @return The stored LLM response, or NULL.
#' @export
entrainer_response <- function(x) {
  if (inherits(x, "entrainer_response")) {
    return(x$text %||% x$markdown %||% NULL)
  }

  if (inherits(x, "entrainer_prompt")) {
    return(attr(x, "response", exact = TRUE))
  }

  stop(
    "`x` must be an `entrainer_prompt` or `entrainer_response` object.",
    call. = FALSE
  )
}


# ---- Structured FactoMineR extraction helpers --------------------------------

#' Format a small data frame as a markdown-like text table
#' @keywords internal
#' @noRd
trainer_core_format_table <- function(x, digits = 3) {
  if (is.null(x) || !is.data.frame(x) || !nrow(x)) return("(not available)")
  num <- vapply(x, is.numeric, logical(1))
  x[num] <- lapply(x[num], function(z) signif(z, digits = digits))
  paste(utils::capture.output(print(x, row.names = FALSE)), collapse = "\n")
}

#' Extract eigenvalue information for one dimension
#' @keywords internal
#' @noRd
trainer_core_eigen_table <- function(x, dimension) {
  eig <- tryCatch(x$eig, error = function(e) NULL)
  if (is.null(eig) || !NROW(eig) || dimension > NROW(eig)) return(NULL)
  vals <- eig[dimension, , drop = TRUE]
  data.frame(
    metric = names(vals) %||% paste0("V", seq_along(vals)),
    value = as.numeric(vals),
    stringsAsFactors = FALSE
  )
}

#' Extract the strongest row coordinates/contributions/cos2 for a dimension
#' @keywords internal
#' @noRd
trainer_core_axis_table <- function(x,
                                    dimension,
                                    slot = "var",
                                    label = "element",
                                    top_n = 12L) {
  obj <- tryCatch(x[[slot]], error = function(e) NULL)
  if (is.null(obj)) return(NULL)

  coord <- tryCatch(obj$coord[, dimension], error = function(e) NULL)
  if (is.null(coord)) return(NULL)

  contrib <- tryCatch(obj$contrib[, dimension], error = function(e) NULL)
  cos2 <- tryCatch(obj$cos2[, dimension], error = function(e) NULL)

  nm <- names(coord) %||% rownames(obj$coord) %||% paste0(label, seq_along(coord))
  out <- data.frame(
    name = nm,
    coord = as.numeric(coord),
    stringsAsFactors = FALSE
  )
  if (!is.null(contrib)) out$contrib <- as.numeric(contrib)
  if (!is.null(cos2)) out$cos2 <- as.numeric(cos2)

  ord <- order(abs(out$coord), decreasing = TRUE)
  out <- out[ord, , drop = FALSE]
  top_n <- min(trainer_core_check_positive_integer(top_n, "top_n"), nrow(out))
  out[seq_len(top_n), , drop = FALSE]
}

#' Build a structured evidence block for PCA/MCA dimensions
#' @keywords internal
#' @noRd
trainer_core_factor_axis_evidence <- function(x,
                                              dimension,
                                              element_label = "Variables/categories",
                                              top_n = 12L) {
  eig <- trainer_core_eigen_table(x, dimension)
  active <- trainer_core_axis_table(x, dimension, slot = "var", label = element_label, top_n = top_n)
  ind <- trainer_core_axis_table(x, dimension, slot = "ind", label = "individual", top_n = min(8L, top_n))

  parts <- character(0)
  parts <- c(parts, paste0("#### Eigenvalue / inertia for Dim. ", dimension))
  parts <- c(parts, "```", trainer_core_format_table(eig), "```")
  parts <- c(parts, paste0("#### Strongest active ", element_label, " on Dim. ", dimension))
  parts <- c(parts, "```", trainer_core_format_table(active), "```")
  if (!is.null(ind) && nrow(ind)) {
    parts <- c(parts, paste0("#### Illustrative strongest individuals on Dim. ", dimension))
    parts <- c(parts, "```", trainer_core_format_table(ind), "```")
  }
  paste(parts, collapse = "\n")
}

# ---- ANOVA/LinearModel text extraction & T-test filtering -------------------

#' Extract lines following a header (up to first blank line)
#' @param txt Printed object as a single string.
#' @param header Exact header text to search.
#' @return Character vector of lines until first blank line.
#' @keywords internal
#' @noRd
#'
#' @examples
#' trainer_core_extract_block_after("Head\nA\n\nB", "Head")
trainer_core_extract_block_after <- function(txt, header) {
  txt <- trainer_core_check_string(txt, "txt")
  header <- trainer_core_check_string(header, "header")

  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  idx <- grep(paste0("^\\s*", trainer_core_quote(header), "\\s*$"), lines, perl = TRUE)
  if (!length(idx)) return(character(0))

  start <- idx[1L] + 1L
  if (start > length(lines)) return(character(0))

  remaining <- lines[start:length(lines)]
  blank <- which(grepl("^\\s*$", remaining, perl = TRUE))
  end <- if (length(blank)) start + blank[1L] - 2L else length(lines)
  if (end < start) return(character(0))

  lines[start:end]
}

#' Heuristic extractor for F-test / T-test blocks when headers are missing
#' @param txt single string (printed output)
#' @return list(ftest_lines, ttest_lines)
#' @keywords internal
#' @noRd
trainer_core_extract_tables_heuristic <- function(txt) {
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  idx_f <- suppressWarnings(
    grep("(Df|df).*?(Sum Sq|SS).*?(Mean Sq|MS)|F value|Pr\\(>F\\)",
         lines,
         ignore.case = TRUE,
         perl = TRUE)
  )[1]
  idx_t <- suppressWarnings(
    grep("(Estimate).*?(Std\\.?\\s*Error)|t value|Pr\\(>\\|t\\|\\)",
         lines,
         ignore.case = TRUE,
         perl = TRUE)
  )[1]

  ftest_lines <- character(0)
  ttest_lines <- character(0)

  if (!is.na(idx_f) && !is.na(idx_t)) {
    if (idx_f < idx_t) {
      ftest_lines <- lines[idx_f:(idx_t - 1L)]
      ttest_lines <- lines[idx_t:length(lines)]
    } else {
      ttest_lines <- lines[idx_t:(idx_f - 1L)]
      ftest_lines <- lines[idx_f:length(lines)]
    }
  } else if (!is.na(idx_f)) {
    ftest_lines <- lines[idx_f:length(lines)]
  } else if (!is.na(idx_t)) {
    ttest_lines <- lines[idx_t:length(lines)]
  }

  list(ftest_lines = ftest_lines, ttest_lines = ttest_lines)
}

#' Filter T-test lines by requested factors (main and/or interactions)
#' @param tt_lines Character vector of T-test lines.
#' @param keep_factors Character vector of factor names or "A:B".
#' @param keep_intercept Logical; keep (Intercept) line.
#' @return Filtered character vector.
#' @keywords internal
#' @noRd
#'
#' @examples
#' trainer_core_filter_ttest_by_factors(c("(Intercept)", "A - a", "A - b:B - c"), "A", TRUE)
trainer_core_filter_ttest_by_factors <- function(tt_lines, keep_factors = NULL, keep_intercept = TRUE) {
  if (!length(tt_lines)) return(character(0))

  # détecter position du header (si présent)
  header_idx <- which(grepl("\\bEstimate\\b", tt_lines, perl = TRUE) |
                        grepl("Std\\.?\\s*Error", tt_lines, perl = TRUE) |
                        grepl("\\bt value\\b", tt_lines, perl = TRUE) |
                        grepl("Pr\\(>\\|?t\\|?\\)", tt_lines, perl = TRUE))
  header_pos <- if (length(header_idx)) min(header_idx) else NA_integer_

  is_intercept   <- grepl("^\\s*\\(Intercept\\)", tt_lines, perl = TRUE)
  is_interaction <- grepl(":", tt_lines, fixed = TRUE)

  # Pas de filtre -> on garde tout (en respectant keep_intercept)
  if (is.null(keep_factors)) {
    keep <- rep(TRUE, length(tt_lines))
    if (!keep_intercept) keep <- keep & !is_intercept
    if (!is.na(header_pos)) keep[seq_len(header_pos)] <- TRUE
    return(tt_lines[keep])
  }

  req_main  <- keep_factors[!grepl(":", keep_factors, fixed = TRUE)]
  req_inter <- keep_factors[ grepl(":", keep_factors, fixed = TRUE)]

  # match des main effects: "Factor Name - level ..."
  keep_main <- rep(FALSE, length(tt_lines))
  if (length(req_main)) {
    keep_main <- Reduce(`|`, lapply(req_main, function(f) {
      pat <- paste0("^\\s*", trainer_core_quote(trimws(f)), "\\s-\\s")
      grepl(pat, tt_lines, perl = TRUE) & !is_interaction
    }))
  }

  # match des interactions: "A - a : B - b" (ordre symétrique)
  keep_inter <- rep(FALSE, length(tt_lines))
  if (length(req_inter)) {
    keep_inter <- Reduce(`|`, lapply(req_inter, function(f) {
      parts <- strsplit(f, ":", fixed = TRUE)[[1]]
      if (length(parts) != 2) return(rep(FALSE, length(tt_lines)))
      a <- trainer_core_quote(trimws(parts[1]))
      b <- trainer_core_quote(trimws(parts[2]))
      grepl(
        paste0(
          "^\\s*", a, "\\s-\\s.*:\\s*", b, "\\s-\\s", "|",
          "^\\s*", b, "\\s-\\s.*:\\s*", a, "\\s-\\s"
        ),
        tt_lines,
        perl = TRUE
      )
    }))
  }

  keep <- keep_main | keep_inter
  if (keep_intercept) keep <- keep | is_intercept

  # Toujours conserver les lignes d’entête si détectées
  if (!is.na(header_pos)) keep[seq_len(header_pos)] <- TRUE

  tt_lines[keep]
}

#' Detect main-effect factor names present in T-test lines (ignore interactions)
#' Space-safe: captures everything before " - " on non-interaction rows.
#' @param tt_lines Character vector.
#' @return Character vector of factor names.
#' @keywords internal
#' @noRd
trainer_core_detect_main_factors <- function(tt_lines) {
  if (!length(tt_lines)) return(character(0))

  x <- tt_lines
  # enlever header/vides
  drop <- grepl("\\bEstimate\\b", x, perl = TRUE) |
    grepl("Std\\.?\\s*Error", x, perl = TRUE) |
    grepl("Pr\\(>\\|?t\\|?\\)", x, perl = TRUE) |
    grepl("^\\s*$", x, perl = TRUE)
  x <- x[!drop]

  # garder seulement les lignes "main effects" (pas d'interaction) avec " - "
  x <- x[!grepl(":", x, fixed = TRUE)]
  x <- x[grepl("\\s-\\s", x, perl = TRUE)]

  # capturer le nom du facteur avant " - "
  fac <- sub("^\\s*(.+?)\\s-\\s.*$", "\\1", x, perl = TRUE)
  fac <- trimws(fac)
  unique(fac[nzchar(fac)])
}

#' Determine which requested items were actually shown after filtering
#' @param req_main Character vector of requested main factors.
#' @param req_inter Character vector of requested interactions ("A:B").
#' @param ttest_filtered Filtered T-test lines.
#' @return Character vector of actually shown specifiers.
#' @keywords internal
#' @noRd
#'
#' @examples
#' trainer_core_actually_shown("A", "A:B", c("A - a", "A - a : B - b"))
trainer_core_actually_shown <- function(req_main, req_inter, ttest_filtered) {
  found_main <- character(0)
  if (length(req_main)) {
    found_main <- req_main[vapply(req_main, function(f) {
      pat <- paste0("^\\s*", trainer_core_quote(f), "\\s-\\s")
      any(grepl(pat, ttest_filtered, perl = TRUE) & !grepl(":", ttest_filtered, fixed = TRUE))
    }, logical(1))]
  }

  found_inter <- character(0)
  if (length(req_inter)) {
    found_inter <- req_inter[vapply(req_inter, function(f) {
      parts <- strsplit(f, ":", fixed = TRUE)[[1]]
      if (length(parts) != 2) return(FALSE)
      a <- trainer_core_quote(trimws(parts[1])); b <- trainer_core_quote(trimws(parts[2]))
      any(grepl(
        paste0(
          "^\\s*", a, "\\s-\\s.*:\\s*", b, "\\s-\\s", "|",
          "^\\s*", b, "\\s-\\s.*:\\s*", a, "\\s-\\s"
        ),
        ttest_filtered,
        perl = TRUE
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
#' @keywords internal
#' @noRd
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


#' Extract a block of lines between a start pattern and a set of stop patterns
#'
#' @param lines Character vector of output lines.
#' @param start_pat Regex pattern to find the start line.
#' @param stop_pats Character vector of regex patterns for stop signals.
#' @return Character vector of lines (trimmed).
#' @keywords internal
#' @noRd
trainer_core_extract_section <- function(lines, start_pat, stop_pats) {
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
