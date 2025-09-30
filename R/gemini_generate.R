#' Generate text with Google Gemini (Generative Language API)
#'
#' @description
#' Minimal wrapper around the Google Generative Language API
#' `:generateContent` endpoint for text prompts.
#'
#' @param prompt Character scalar. The user prompt (plain text).
#' @param model Character scalar. Gemini model id (e.g., "gemini-2.5-flash",
#'   "gemini-2.5-pro"). You may also pass "models/..." and it will be normalized.
#' @param api_key Character scalar. API key. Defaults to env var
#'   `GEMINI_API_KEY`.
#' @param user_agent Character scalar for the HTTP User-Agent header.
#'   Defaults to "EnTraineR/0.1.0 (https://github.com/Sebastien-Le/EnTraineR)".
#' @param base_url Character scalar. API base URL. Default:
#'   "https://generativelanguage.googleapis.com/v1beta".
#' @param temperature Optional numeric in [0, 2]. Sampling temperature.
#' @param top_p Optional numeric in (0, 1]. Nucleus sampling.
#' @param top_k Optional integer >= 1. Top-k sampling.
#' @param max_output_tokens Optional integer > 0. Max tokens in response.
#' @param stop_sequences Optional character vector of stop strings.
#' @param system_instruction Optional character scalar for system instruction.
#' @param safety_settings Optional list passed as-is to the API (advanced).
#' @param seed Optional integer for deterministic sampling (where supported).
#' @param timeout Numeric seconds for HTTP request timeout (default 60).
#' @param verbose Logical; if TRUE prints the resolved URL and model.
#'
#' @return Character scalar with the first candidate's text. If no text is
#'   returned, an informative error is thrown with finish reason or safety info.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(GEMINI_API_KEY = "YOUR_KEY")
#' gemini_generate("Say hello in one short sentence.",
#'                 model = "gemini-2.5-flash")
#' }
#'
#' @importFrom httr2 request req_url_query req_user_agent req_headers
#'   req_body_json req_perform resp_body_json req_timeout
#' @export
gemini_generate <- function(
    prompt,
    model = "gemini-2.5-flash",
    api_key = Sys.getenv("GEMINI_API_KEY"),
    user_agent = "EnTraineR/0.1.0 (https://github.com/Sebastien-Le/EnTraineR)",
    base_url = "https://generativelanguage.googleapis.com/v1beta",
    temperature = NULL,
    top_p = NULL,
    top_k = NULL,
    max_output_tokens = NULL,
    stop_sequences = NULL,
    system_instruction = NULL,
    safety_settings = NULL,
    seed = NULL,
    timeout = 60,
    verbose = FALSE
) {
  # ---- Guardrails ------------------------------------------------------------
  if (!nzchar(api_key)) {
    stop("Set GEMINI_API_KEY env var first, e.g. Sys.setenv(GEMINI_API_KEY = 'YOUR_KEY')")
  }
  if (length(prompt) != 1L || !nzchar(prompt)) {
    stop("`prompt` must be a non-empty character scalar.")
  }
  # Normalize model id: accept "models/xxx" or "xxx"
  model_id <- sub("^models/", "", model)

  # Build endpoint: v1beta/models/{model}:generateContent
  url <- sprintf("%s/models/%s:generateContent", base_url, model_id)
  if (isTRUE(verbose)) message("POST ", url)

  # Build generationConfig with only non-NULL fields
  gen_cfg <- Filter(Negate(is.null), list(
    temperature = temperature,
    topP = top_p,
    topK = top_k,
    maxOutputTokens = max_output_tokens,
    stopSequences = stop_sequences,
    seed = seed
  ))

  # Build content
  contents <- list(list(
    role = "user",
    parts = list(list(text = as.character(prompt)))
  ))

  body <- list(
    contents = contents
  )
  if (length(gen_cfg)) body$generationConfig <- gen_cfg
  if (!is.null(system_instruction)) {
    body$systemInstruction <- list(role = "system",
                                   parts = list(list(text = as.character(system_instruction))))
  }
  if (!is.null(safety_settings)) {
    body$safetySettings <- safety_settings
  }

  # ---- HTTP call -------------------------------------------------------------
  req <- httr2::request(url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout)

  resp <- NULL
  j <- NULL
  # Perform request with helpful error message
  tryCatch({
    resp <- httr2::req_perform(req)
    j <- httr2::resp_body_json(resp)
  }, error = function(e) {
    # Common cause: wrong URL or model -> 404
    stop(sprintf("Request failed before reaching the API: %s", conditionMessage(e)))
  })

  # ---- Parse response --------------------------------------------------------
  # Expect j$candidates[[1]]$content$parts[[i]]$text
  cand <- tryCatch(j$candidates, error = function(e) NULL)

  if (is.null(cand) || length(cand) < 1) {
    # Inspect finishReason / safety if available
    finish <- tryCatch(j$promptFeedback$safetyRatings, error = function(e) NULL)
    fr2    <- tryCatch(j$candidates[[1]]$finishReason, error = function(e) NULL)
    if (!is.null(fr2)) {
      stop(sprintf("No content returned. finishReason: %s", as.character(fr2)))
    }
    if (!is.null(finish)) {
      stop("No content returned. Likely blocked by safety settings.")
    }
    stop("No content returned. Empty candidates.")
  }

  # First candidate
  c1 <- cand[[1]]
  # Safety/finish diagnostics (non-fatal unless no text)
  fr <- tryCatch(c1$finishReason, error = function(e) NULL)

  parts <- tryCatch(c1$content$parts, error = function(e) NULL)
  if (is.null(parts) || !length(parts)) {
    # Sometimes the text is under c1$content$parts[[1]]$text, but check fallback
    stop(sprintf("Candidate has no parts. finishReason: %s", as.character(fr)))
  }

  texts <- vapply(parts, function(p) as.character(p$text %||% ""), character(1))
  out <- paste(texts[nzchar(texts)], collapse = "")

  if (!nzchar(out)) {
    stop(sprintf("Empty text in first candidate. finishReason: %s", as.character(fr)))
  }

  out
}
