test_that("jaya returns correct output", {
  result <- jaya(
    fun = function(x) sum(x^2),
    lower = c(-5, -5),
    upper = c(5, 5),
    popSize = 20,
    maxiter = 50,
    n_var = 2
  )
  expect_type(result, "list")
  expect_true("Best" %in% names(result))
})
