set.seed(123)
x <- matrix(runif(100), ncol = 10)

test_that("horns_curve provides proper messages and warnings", {

  expect_that(horns_curve(x, n = 10), throws_error())
  expect_that(horns_curve(x, p = 10), throws_error())
  expect_that(horns_curve(p = 5, n = 10), throws_error())
  expect_that(horns_curve(n = 10, data = NULL), throws_error())
  expect_that(horns_curve(p = 10, data = NULL), throws_error())
  expect_that(horns_curve(n = "a", p = 1, data = NULL), throws_error())
  expect_that(horns_curve(p = "a", data = NULL), throws_error())
  expect_that(horns_curve(p = 5:10, n = 10:10, data = NULL), throws_error())

})

test_that("horns_curve output is correct dimensions", {

  expect_equal(horns_curve(x) %>% length(), 10)
  expect_equal(horns_curve(data = NULL, n = 5, p = 8) %>% length(), 8)
  expect_true(horns_curve(data = NULL, n = 5, p = 8) %>% is.numeric())
  expect_true(horns_curve(data = NULL, n = 5, p = 8) %>% is.atomic())

})


