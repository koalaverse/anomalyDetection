context("horns_curve")

# Simulate some data for tests
set.seed(123)
x <- matrix(runif(100), ncol = 10)

test_that("horns_curve provides proper messages and warnings", {
  expect_that(horns_curve(n = 10), throws_error())
  expect_that(horns_curve(p = 10), throws_error())
  expect_that(horns_curve(n = "a"), throws_error())
  expect_that(horns_curve(p = "a"), throws_error())
  expect_that(horns_curve(n = 1:3, p = 1:3), throws_error())
})

test_that("horns_curve output is correct dimensions", {
  expect_equal(length(horns_curve(x)), 10)
  expect_equal(length(horns_curve(n = 5, p = 8)), 8)
  expect_true(is.numeric(horns_curve(n = 5, p = 8)))
  expect_true(is.atomic(horns_curve(n = 5, p = 8)))
})
