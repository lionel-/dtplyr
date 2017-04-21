#' @import rlang
NULL

# j <- lazyeval::interp(~ .I[rows], rows = args[[1]]$expr)
# dt_subset(dt, , j, env)

dt_subset <- function(dt, i, j, env = caller_env(), sd_cols = NULL) {
  if (missing(j)) {
    expr <- expr(dt[!! maybe_missing(i)])
  } else {
    expr <- expr(dt[
      !! maybe_missing(i),
      !! j,
      by = list(!!! groups(dt))
    ])
    expr$.SDcols <- sd_cols
  }

  expr <- dt_replace(expr)
  expr <- quo_expr(expr, warn = TRUE)

  return(eval_bare(expr), env)


  env <- new.env(parent = env, size = 2L)
  env$`_dt` <- dt
  env$`_vars` <- deparse_all(groups(dt))

  args <- list(
    i = if (missing(i)) quote(expr =) else dt_replace(i),
    j = if (missing(j)) quote(expr =) else dt_replace(j)
  )

  if (missing(j)) {
    call <- substitute(`_dt`[i], args)
  } else {
    call <- substitute(`_dt`[i, j, by = `_vars`], args)
    call$.SDcols = sd_cols
  }
  # print(call)

  eval(call, env)
}

dt_replace <- function(x) {
  expr_substitute(x,
    . ~ .SD,
    n() ~ .N
  )
}

maybe_missing <- function(x) {
  if (is_missing(x) || quo_is_missing(x)) {
    missing_arg()
  } else {
    x
  }
}

commas <- function(...) paste0(..., collapse = ", ")

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}

drop_last <- function(x) {
  if (length(x) <= 1L) return(NULL)
  x[-length(x)]
}
