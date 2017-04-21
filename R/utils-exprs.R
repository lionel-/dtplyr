
expr_type_of <- function(x) {
  if (missing(x)) {
    return("missing")
  }

  type <- typeof(x)
  if (type %in% c("symbol", "language", "pairlist", "NULL")) {
    type
  } else {
    "literal"
  }
}
switch_expr <- function(.x, ...) {
  switch(expr_type_of(.x), ...)
}

node_walk_replace <- function(node, syms, langs) {
  while(!is_null(node)) {
    car <- node_car(node)
    switch_expr(car,
      symbol = match_node_sym(car, syms, node),
      language =
        if (match_node_lang(car, langs, node)) {
          node_walk_replace(node_cdr(car), syms, langs)
        } else {
          node_walk_replace(car, syms, langs)
        }
    )
    node <- node_cdr(node)
  }
}

expr_substitute <- function(expr, ...) {
  dots <- exprs(...)
  stopifnot(every(dots, is_formula, lhs = TRUE))

  syms <- keep(dots, function(f) is_symbol(f_lhs(f)))
  langs <- keep(dots, function(f) is_lang(f_lhs(f)))

  switch_type(expr,
    formula = ,
    language = {
      expr <- pairlist(duplicate(expr))
      node_walk_replace(expr, syms, langs)
      expr <- node_car(expr)
    },
    symbol = {
      for (i in seq_along(syms)) {
        cur <- syms[[i]]
        if (identical(expr, f_lhs(cur))) {
          expr <- f_rhs(cur)
        }
      }
    }
  )

  expr
}

match_node_sym <- function(x, syms, node, mut = mut_node_car) {
  if (is_null(syms)) {
    return(FALSE)
  }

  for (i in seq_along(syms)) {
    cur <- syms[[i]]
    if (identical(x, f_lhs(cur))) {
      mut(node, f_rhs(cur))
      return(TRUE)
    }
  }

  FALSE
}
match_node_lang <- function(x, langs, node, mut = mut_node_car) {
  if (is_null(langs)) {
    return(FALSE)
  }

  for (i in seq_along(langs)) {
    cur <- langs[[i]]
    pattern <- f_lhs(cur)
    if (identical(node_car(x), node_car(pattern))) {
      mut(node, f_rhs(cur))
      return(TRUE)
    }
  }

  FALSE
}
