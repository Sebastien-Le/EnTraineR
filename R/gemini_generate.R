# helper (internal)
.default_user_agent <- function(pkg = "EnTraineR",
                                url = "https://github.com/Sebastien-Le/EnTraineR") {
  ver_pkg  <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "0.0.0")
  ver_r    <- as.character(getRversion())
  ver_httr <- tryCatch(as.character(utils::packageVersion("httr2")), error = function(e) NA_character_)
  if (!is.na(ver_httr)) {
    sprintf("%s/%s (R/%s; httr2/%s; %s)", pkg, ver_pkg, ver_r, ver_httr, url)
  } else {
    sprintf("%s/%s (R/%s; %s)", pkg, ver_pkg, ver_r, url)
  }
}

# helper (internal) to open files cross-platform
.open_file <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    shell.exec(path)
  } else {
    sysname <- Sys.info()[["sysname"]]
    if (identical(sysname, "Darwin")) {
      system2("open", shQuote(path), wait = FALSE)
    } else {
      system2("xdg-open", shQuote(path), wait = FALSE)
    }
  }
  invisible(TRUE)
}

#' Generate text with Google Gemini (Generative Language API) - robust w/ retries
#'
#' @description
#' Minimal wrapper around the Generative Language API ':generateContent' endpoint
#' for text prompts, with retries, exponential backoff, clearer errors, and
#' optional output compilation (HTML/DOCX) with auto-open.
#'
#' @param prompt Character scalar. The user prompt (plain text).
#' @param model Character scalar. Gemini model id (e.g., "gemini-2.5-flash",
#'   "gemini-2.5-pro"). You may also pass "models/..." and it will be normalized.
#' @param api_key Character scalar. API key. Defaults to env var 'GEMINI_API_KEY'.
#' @param user_agent Character scalar. If NULL, a dynamic value is used.
#' @param base_url Character scalar. API base URL.
#' @param temperature Optional numeric in [0, 2].
#' @param top_p Optional numeric in (0, 1].
#' @param top_k Optional integer >= 1.
#' @param max_output_tokens Optional integer > 0.
#' @param stop_sequences Optional character vector.
#' @param system_instruction Optional character scalar.
#' @param safety_settings Optional list passed as-is to the API.
#' @param seed Optional integer seed.
#' @param timeout Numeric seconds for request timeout (default 120).
#' @param verbose Logical; if TRUE, prints URL/retries.
#' @param max_tries Integer. Max attempts (default 5).
#' @param backoff_base Numeric. Initial backoff seconds (default 0.8).
#' @param backoff_cap Numeric. Max backoff seconds (default 8).
#' @param force_markdown Logical. If TRUE, instructs the model to answer in Markdown.
#' @param compile_to Character scalar. One of c("none","html","docx").
#'
#' @return
#' If compile_to = "none": character scalar (raw text as returned by the API).
#' If compile_to = "html": list(markdown = <string>, html_path = <path>), and opens the HTML.
#' If compile_to = "docx": list(markdown = <string>, docx_path = <path>), and opens the DOCX.
#'
#' @importFrom httr2 request req_url_query req_user_agent req_headers
#' @importFrom httr2 req_body_json req_perform resp_body_json req_timeout
#' @importFrom stats runif
#' @importFrom utils packageVersion
#' @export
gemini_generate <- function(
    prompt,
    model = "gemini-2.5-flash",
    api_key = Sys.getenv("GEMINI_API_KEY"),
    user_agent = NULL,
    base_url = "https://generativelanguage.googleapis.com/v1beta",
    temperature = NULL,
    top_p = NULL,
    top_k = NULL,
    max_output_tokens = NULL,
    stop_sequences = NULL,
    system_instruction = NULL,
    safety_settings = NULL,
    seed = NULL,
    timeout = 120,
    verbose = FALSE,
    max_tries = 5,
    backoff_base = 0.8,
    backoff_cap  = 8,
    force_markdown = TRUE,
    compile_to = c("none", "html", "docx")
) {
  compile_to <- match.arg(compile_to)

  # ---- Guardrails ------------------------------------------------------------
  if (!nzchar(api_key)) {
    stop("Set GEMINI_API_KEY env var first, e.g. Sys.setenv(GEMINI_API_KEY = 'YOUR_KEY')")
  }
  if (length(prompt) != 1L || !nzchar(prompt)) {
    stop("`prompt` must be a non-empty character scalar.")
  }

  # Normalize model id
  model_id <- sub("^models/", "", model)

  # Resolve User-Agent
  if (is.null(user_agent) || !nzchar(user_agent)) {
    ua_opt <- getOption("EnTraineR.user_agent", default = NA_character_)
    user_agent <- if (is.na(ua_opt)) .default_user_agent() else ua_opt
  }

  # Strengthen Markdown instruction if requested
  sys_instr <- system_instruction
  if (isTRUE(force_markdown)) {
    add <- "Respond using GitHub-flavored Markdown. Do not wrap the entire response in a code block."
    if (is.null(sys_instr) || !nzchar(sys_instr)) {
      sys_instr <- add
    } else {
      sys_instr <- paste(sys_instr, add)
    }
  }

  # Endpoint & Config
  url <- sprintf("%s/models/%s:generateContent", base_url, model_id)
  if (isTRUE(verbose)) message("POST ", url)

  gen_cfg <- Filter(Negate(is.null), list(
    temperature     = temperature,
    topP            = top_p,
    topK            = top_k,
    maxOutputTokens = max_output_tokens,
    stopSequences   = stop_sequences,
    seed            = seed
  ))

  # Body Construction
  body <- list(
    contents = list(list(
      role  = "user",
      parts = list(list(text = as.character(prompt)))
    ))
  )
  if (length(gen_cfg)) body$generationConfig <- gen_cfg
  if (!is.null(sys_instr)) {
    body$systemInstruction <- list(
      role  = "system",
      parts = list(list(text = as.character(sys_instr)))
    )
  }
  if (!is.null(safety_settings)) body$safetySettings <- safety_settings

  # Build Request
  req <- httr2::request(url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout)

  # ---- Retry Loop ------------------------------------------------------------
  attempt <- 1L
  last_err <- NULL

  while (attempt <= max_tries) {
    j <- NULL
    ok <- FALSE
    resp <- NULL

    tryCatch({
      resp <- httr2::req_perform(req)
      j <- httr2::resp_body_json(resp)
      ok <- TRUE
    }, error = function(e) {
      last_err <<- conditionMessage(e)
    })

    if (ok) {
      # --- Response Parsing ---
      cand <- tryCatch(j$candidates, error = function(e) NULL)
      if (is.null(cand) || length(cand) < 1) {
        fr  <- tryCatch(j$candidates[[1]]$finishReason, error = function(e) NULL)
        sft <- tryCatch(j$promptFeedback$safetyRatings, error = function(e) NULL)
        if (!is.null(fr))  stop(sprintf("No content returned. finishReason: %s", as.character(fr)))
        if (!is.null(sft)) stop("No content returned. Likely blocked by safety settings.")
        stop("No content returned. Empty candidates.")
      }

      c1    <- cand[[1]]
      parts <- tryCatch(c1$content$parts, error = function(e) NULL)
      fr    <- tryCatch(c1$finishReason,   error = function(e) NULL)

      if (is.null(parts) || !length(parts)) {
        stop(sprintf("Candidate has no parts. finishReason: %s", as.character(fr)))
      }

      texts <- vapply(parts, function(p) as.character(if (is.null(p$text)) "" else p$text), character(1))
      out   <- paste(texts[nzchar(texts)], collapse = "")
      if (!nzchar(out)) stop(sprintf("Empty text in first candidate. finishReason: %s", as.character(fr)))

      # --- Post-Processing & Compilation ---
      if (compile_to == "none") {
        return(out)
      }

      md <- out

      # Remove encompassing code fences if present
      if (grepl("^```", md) && grepl("```\\s*$", md)) {
        md <- sub("^```(?:markdown)?\\s*\n?", "", md)
        md <- sub("\\s*```\\s*$", "", md)
      }

      if (compile_to == "html") {
        # Try commonmark to HTML first
        html_txt <- NULL
        if (requireNamespace("commonmark", quietly = TRUE)) {
          html_txt <- tryCatch(commonmark::markdown_html(md, extensions = TRUE), error = function(e) NULL)
        }

        html_path <- tempfile(fileext = ".html")
        if (!is.null(html_txt)) {
          con <- file(html_path, open = "wb", encoding = "UTF-8")
          writeLines(enc2utf8(html_txt), con, useBytes = TRUE)
          close(con)
        } else {
          # Fallback to pandoc via rmarkdown
          if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
            in_md <- tempfile(fileext = ".md")
            con <- file(in_md, open = "wb", encoding = "UTF-8")
            writeLines(enc2utf8(md), con, useBytes = TRUE)
            close(con)
            rmarkdown::pandoc_convert(in_md, to = "html", output = html_path, options = c("--standalone"))
          } else {
            stop("HTML conversion requires 'commonmark' pkg or 'rmarkdown' with Pandoc.")
          }
        }

        # Auto-open HTML
        utils::browseURL(html_path)
        return(list(markdown = md, html_path = html_path))
      }

      if (compile_to == "docx") {
        if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available()) {
          stop("DOCX conversion requires Pandoc available via the 'rmarkdown' package.")
        }

        in_md    <- tempfile(fileext = ".md")
        docx_path <- tempfile(fileext = ".docx")

        con <- file(in_md, open = "wb", encoding = "UTF-8")
        writeLines(enc2utf8(md), con, useBytes = TRUE)
        close(con)

        rmarkdown::pandoc_convert(in_md, to = "docx", output = docx_path)

        # Auto-open DOCX (Word or default app)
        .open_file(docx_path)
        return(list(markdown = md, docx_path = docx_path))
      }

      # Should not be reached
      return(out)
    }

    # --- Error Handling & Retry Logic ---
    retryable <- FALSE
    if (!is.null(last_err)) {
      if (grepl("Timeout was reached", last_err, fixed = TRUE)) retryable <- TRUE
      if (grepl("HTTP 429", last_err, fixed = TRUE)) retryable <- TRUE
      if (grepl("HTTP 5\\d\\d", last_err)) retryable <- TRUE
      if (grepl("Failed to perform HTTP request", last_err, fixed = TRUE)) retryable <- TRUE
    }

    if (!retryable || attempt == max_tries) {
      stop(sprintf("Request failed after %d attempt(s): %s",
                   attempt, if (is.null(last_err)) "<unknown>" else last_err))
    }

    wait <- min(backoff_cap, backoff_base * (2^(attempt - 1)))
    wait <- wait * stats::runif(1, 0.8, 1.25)
    if (isTRUE(verbose)) {
      message(sprintf("Retrying in %.2fs (attempt %d/%d) ...", wait, attempt + 1L, max_tries))
    }
    Sys.sleep(wait)
    attempt <- attempt + 1L
  }

  stop("Unexpected error in gemini_generate().")
}
