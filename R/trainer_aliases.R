#' Deprecated short aliases for EntraineR trainers
#'
#' @description
#' These aliases are kept for backward compatibility only. Prefer the explicit
#' snake_case API: `trainer_chisq_test()`, `trainer_cor_test()`,
#' `trainer_prop_test()`, and `trainer_var_test()`.
#'
#' @param ... Passed to the corresponding trainer function.
#' @return Same return value as the corresponding trainer.
#' @name trainer_deprecated_aliases
NULL

#' @rdname trainer_deprecated_aliases
#' @export
trainer_chisq <- function(...) {
  .Deprecated("trainer_chisq_test")
  trainer_chisq_test(...)
}

#' @rdname trainer_deprecated_aliases
#' @export
trainer_cor <- function(...) {
  .Deprecated("trainer_cor_test")
  trainer_cor_test(...)
}

#' @rdname trainer_deprecated_aliases
#' @export
trainer_prop <- function(...) {
  .Deprecated("trainer_prop_test")
  trainer_prop_test(...)
}

#' @rdname trainer_deprecated_aliases
#' @export
trainer_var <- function(...) {
  .Deprecated("trainer_var_test")
  trainer_var_test(...)
}
