
# Set operations ---------------------------------------------------------------

#' @export
#' @importFrom dplyr distinct
distinct.data.table <- function(.data, ..., .keep_all = FALSE) {
  quos <- quos(..., .named = TRUE)
  dist <- distinct_vars(.data, quos, .keep_all = TRUE)

  if (length(dist$vars) == 0) {
    res <- unique(dist$data, by = NULL)
  } else {
    res <- unique(dist$data, by = dist$vars)
  }

  if (length(dist$vars) > 0 && !.keep_all) {
    res <- res[, dist$vars, with = FALSE]
  }

  res
}
#' @importFrom dplyr distinct_
#' @export
distinct_.data.table <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! dots)
}
#' @export
distinct_.tbl_dt <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! dots)
}

distinct_vars <- function(.data, vars, group_vars = character(), .keep_all = FALSE) {
  stopifnot(is_quosures(vars), is.character(group_vars))

  # If no input, keep all variables
  if (length(vars) == 0) {
    return(list(
      data = .data,
      vars = names(.data),
      keep = names(.data)
    ))
  }

  # If any calls, use mutate to add new columns, then distinct on those
  needs_mutate <- map_lgl(vars, quo_is_lang)
  if (any(needs_mutate)) {
    .data <- mutate(.data, !!! vars[needs_mutate])
  }

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  vars <- intersect(names(.data), c(names(vars), group_vars))

  if (.keep_all) {
    keep <- names(.data)
  } else {
    keep <- unique(vars)
  }

  list(data = .data, vars = vars, keep = keep)
}
