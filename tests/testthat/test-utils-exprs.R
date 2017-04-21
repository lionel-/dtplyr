context("utils-exprs")

test_that("can substitute symbols", {
  expr <- quote(foo(bar, ., list(., +.)))

  expect_identical(
    expr_substitute(expr, . ~ baz),
    quote(foo(bar, baz, list(baz, +baz)))
  )
  expect_identical(
    expr_substitute(expr, . ~ baz, bar ~ bam),
    quote(foo(bam, baz, list(baz, +baz)))
  )
})

test_that("can substitute calls", {
  expr <- quote(foo(bar, ., list(., +.)))

  expect_identical(
    expr_substitute(expr, `+`() ~ baz(foo)),
    quote(foo(bar, ., list(., baz(foo))))
  )
})
