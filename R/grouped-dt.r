#' A grouped data table.
#'
#' The easiest way to create a grouped data table is to call the \code{group_by}
#' method on a data table or tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a tbl or data frame.
#' @param vars a list of quoted variables.
#' @param copy If \code{TRUE}, will make copy of input.
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' if (require("nycflights13")) {
#' flights_dt <- tbl_dt(flights)
#' group_size(group_by(flights_dt, year, month, day))
#' group_size(group_by(flights_dt, dest))
#'
#' monthly <- group_by(flights_dt, month)
#' summarise(monthly, n = n(), delay = mean(arr_delay))
#' }
grouped_dt <- function(data, vars, copy = TRUE) {
  stopifnot(data.table::is.data.table(data))
  if (length(vars) == 0) return(tbl_dt(data))

  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }

  if (copy) {
    data <- data.table::copy(data)
  }

  data.table::setattr(data, "vars", vars)
  data.table::setattr(data, "class", c("grouped_dt", "tbl_dt", "tbl", class(data)))
  data
}

#' @export
groups.grouped_dt <- function(x) {
  attr(x, "vars")
}

#' @rdname grouped_dt
#' @param x an object to check
#' @export
is.grouped_dt <- function(x) inherits(x, "grouped_dt")

#' @export
print.grouped_dt <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data table ", dplyr::dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(dplyr::groups(x))), "\n", sep = "")
  cat("\n")
  print(dplyr::trunc_mat(x, n = n, width = width))
  invisible(x)
}

#' @importFrom dplyr group_size
#' @export
group_size.grouped_dt <- function(x) {
  dplyr::summarise_(x, n = ~n())$n
}

#' @importFrom dplyr n_groups
#' @export
n_groups.grouped_dt <- function(x) {
  nrow(dt_subset(x, , quote(list(1))))
}

#' @importFrom dplyr group_by
#' @export
group_by.data.table <- function(.data, ..., add = FALSE) {
  groups <- dplyr::group_by_prepare(.data, ..., add = add)
  grouped_dt(groups$data, groups$groups)
}
#' @importFrom dplyr group_by_
#' @export
group_by_.data.table <- function(.data, ..., .dots, add = FALSE) {
  ######## Need test
  groups <- dplyr::group_by_prepare(.data, ..., .dots = .dots, add = add)
  grouped_dt(groups$data, groups$groups)
}

#' @export
#' @importFrom dplyr ungroup
ungroup.grouped_dt <- function(x, ...) {
  data.table::setattr(x, "vars", NULL)
  data.table::setattr(x, "class", setdiff(class(x), "grouped_dt"))
  x
}


# Do ---------------------------------------------------------------------------

#' @export
do.grouped_dt <- function(.data, ...) {
  quos <- quos(...)
  named <- named_args(quos)

  if (!named) {
    j <- f_rhs(quos[[1]])
    env <- f_env(quos)
  } else {
    quos <- map(quos, function(quo) lang("list", quo))
    j <- lang("list", !!! quos)
  }

  out <- dt_subset(.data, , j, env = env, sd_cols = names(.data))

  if (!named) {
    grouped_dt(out, groups(.data))
  } else {
    tbl_dt(out)
  }
}

#' @export
do_.grouped_dt <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, env, ...)
  do(.data, !!! dots)
}

named_args <- function(args) {
  # Arguments must either be all named or all unnamed.
  named <- sum(names2(args) != "")
  if (!(named == 0 || named == length(args))) {
    stop("Arguments to do() must either be all named or all unnamed",
      call. = FALSE)
  }
  if (named == 0 && length(args) > 1) {
    stop("Can only supply single unnamed argument to do()", call. = FALSE)
  }

  # Check for old syntax
  if (named == 1 && names(args) == ".f") {
    stop("do syntax changed in dplyr 0.2. Please see documentation for details",
      call. = FALSE)
  }

  named != 0
}

# Set operations ---------------------------------------------------------------

#' @export
distinct.grouped_dt <- function(.data, ...) {
  quos <- quos(..., .named = TRUE)
  dist <- distinct_vars(.data, quos, group_vars(.data))
  grouped_dt(unique(dist$data, by = dist$vars), groups(.data), copy = FALSE)
}
#' @export
distinct_.grouped_dt <- function(.data, ..., .dots) {
  quos <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! quos)
}

