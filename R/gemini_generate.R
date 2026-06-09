# helper (internal)
.default_user_agent <- function(pkg = "EntraineR",
                                url = "https://github.com/Sebastien-Le/EntraineR") {
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
      system2("open", path, wait = FALSE)
    } else {
      system2("xdg-open", path, wait = FALSE)
    }
  }

  invisible(TRUE)
}

#' Generate text with Google Gemini (Generative Language API) - robust w/ retries
#'
#' @description
#' Minimal wrapper around the Generative Language API ':generateContent' endpoint
#' for text prompts, with retries, exponential backoff, clearer errors, and
#' optional output compilation (HTML/DOCX). Files are opened only when `open = TRUE`.
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
#' @param output_path Optional character scalar. Destination file for HTML/DOCX output.
#'   If NULL, a temporary file is created.
#' @param open Logical; if TRUE, open the generated HTML/DOCX file. Defaults to
#'   `interactive()`.
#'
#' @return An object of class `entrainer_response` with a stable structure.
#'   The generated text is available in `$text`/`$markdown`; `html_path` or
#'   `docx_path` are populated when `compile_to` is `"html"` or `"docx"`.
#'
#' @section Privacy:
#' This function sends `prompt` to the Google Generative Language API. Do not
#' include confidential data unless this is intended and allowed in your context.
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
    compile_to = c("none", "html", "docx"),
    output_path = NULL,
    open = interactive()
) {
  compile_to <- match.arg(compile_to)

  # ---- Guardrails ------------------------------------------------------------
  prompt <- trainer_core_check_string(prompt, "prompt")
  model <- trainer_core_check_string(model, "model")
  api_key <- trainer_core_check_string(api_key, "api_key")
  base_url <- trainer_core_check_string(base_url, "base_url")
  force_markdown <- trainer_core_check_flag(force_markdown, "force_markdown")
  verbose <- trainer_core_check_flag(verbose, "verbose")
  open <- trainer_core_check_flag(open, "open")
  timeout <- trainer_core_check_number(timeout, "timeout", lower = 0, include_lower = FALSE)
  max_tries <- trainer_core_check_positive_integer(max_tries, "max_tries")
  backoff_base <- trainer_core_check_number(backoff_base, "backoff_base", lower = 0, include_lower = FALSE)
  backoff_cap <- trainer_core_check_number(backoff_cap, "backoff_cap", lower = 0, include_lower = FALSE)

  if (!is.null(temperature)) {
    temperature <- trainer_core_check_number(temperature, "temperature", lower = 0, upper = 2)
  }
  if (!is.null(top_p)) {
    top_p <- trainer_core_check_probability(top_p, "top_p", include_upper = TRUE)
  }
  if (!is.null(top_k)) {
    top_k <- trainer_core_check_positive_integer(top_k, "top_k")
  }
  if (!is.null(max_output_tokens)) {
    max_output_tokens <- trainer_core_check_positive_integer(max_output_tokens, "max_output_tokens")
  }
  if (!is.null(seed)) {
    seed <- trainer_core_check_positive_integer(seed, "seed")
  }
  if (!is.null(stop_sequences) && (!is.character(stop_sequences) || anyNA(stop_sequences))) {
    stop("`stop_sequences` must be NULL or a character vector without missing values.", call. = FALSE)
  }
  system_instruction <- trainer_core_check_optional_string(system_instruction, "system_instruction")
  user_agent <- trainer_core_check_optional_string(user_agent, "user_agent")
  output_path <- trainer_core_check_optional_string(output_path, "output_path")

  if (!is.null(output_path)) {
    parent <- dirname(output_path)

    if (!dir.exists(parent)) {
      stop(
        "The directory of `output_path` does not exist: ",
        parent,
        call. = FALSE
      )
    }

    expected_ext <- switch(
      compile_to,
      html = ".html",
      docx = ".docx",
      none = NULL
    )

    if (!is.null(expected_ext) &&
        !endsWith(tolower(output_path), expected_ext)) {
      stop(
        "`output_path` must end with `", expected_ext,
        "` when `compile_to = \"", compile_to, "\"`.",
        call. = FALSE
      )
    }
  }

  # Normalize model id
  model_id <- sub("^models/", "", model)

  # Resolve User-Agent
  if (is.null(user_agent)) {
    ua_opt <- getOption("EntraineR.user_agent", default = NA_character_)
    user_agent <- if (is.na(ua_opt) || !nzchar(ua_opt)) .default_user_agent() else ua_opt
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

  if (!is.null(stop_sequences)) {
    stop_sequences <- as.list(stop_sequences)
  }

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
        return(new_entrainer_response(
          text = out,
          model = model,
          engine = "gemini",
          prompt = prompt,
          compile_to = "none",
          output_path = NULL
        ))
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

        html_path <- output_path %||% tempfile(fileext = ".html")
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

        if (isTRUE(open)) {
          utils::browseURL(html_path)
        }
        return(new_entrainer_response(
          text = md,
          model = model,
          engine = "gemini",
          prompt = prompt,
          compile_to = "html",
          output_path = html_path
        ))
      }

      if (compile_to == "docx") {
        if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available()) {
          stop("DOCX conversion requires Pandoc available via the 'rmarkdown' package.")
        }

        in_md    <- tempfile(fileext = ".md")
        docx_path <- output_path %||% tempfile(fileext = ".docx")

        con <- file(in_md, open = "wb", encoding = "UTF-8")
        writeLines(enc2utf8(md), con, useBytes = TRUE)
        close(con)

        rmarkdown::pandoc_convert(in_md, to = "docx", output = docx_path)

        if (isTRUE(open)) {
          .open_file(docx_path)
        }
        return(new_entrainer_response(
          text = md,
          model = model,
          engine = "gemini",
          prompt = prompt,
          compile_to = "docx",
          output_path = docx_path
        ))
      }

      # Should not be reached
      return(new_entrainer_response(
        text = out,
        model = model,
        engine = "gemini",
        prompt = prompt,
        compile_to = "none",
        output_path = NULL
      ))
    }

    # --- Error Handling & Retry Logic ---
    retryable <- FALSE
    if (!is.null(last_err)) {
      if (grepl("Timeout was reached", last_err, fixed = TRUE)) retryable <- TRUE
      if (grepl("HTTP 429", last_err, fixed = TRUE)) retryable <- TRUE
      if (grepl("HTTP 5[0-9][0-9]", last_err, perl = TRUE)) {
        retryable <- TRUE
      }
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
