#' @importFrom dplyr do
#' @export
do.tbl_dt <- function(.data, ...) {
  out <- do(as.data.frame(.data), ...)
  tbl_dt(out)
}
#' @export
do.data.table <- function(.data, ...) {
  out <- do(as.data.frame(.data), ...)
  data.table::as.data.table(out)
}

#' @importFrom dplyr do_
#' @export
do_.tbl_dt <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, env, ...)
  do(.data, !!! dots)
}
#' @export
do_.data.table <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, env, ...)
  do(.data, !!! dots)
}
