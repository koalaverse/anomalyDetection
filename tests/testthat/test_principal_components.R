context("principal_components")

set.seed(123)
x <- matrix(rnorm(200*3), ncol = 10)


test_that("principal_components output has correct dimensions", {

  expect_equal(principal_components(x) %>% length(), 5)
  expect_true(principal_components(x) %>% is.list)
  expect_equal(principal_components(x) %>% .[[1]] %>% length(), 10)
  expect_equal(principal_components(x) %>% .[[2]] %>% dim(), c(10, 10))
  expect_equal(principal_components(x) %>% .[[3]] %>% dim(), c(60, 10))
  expect_equal(principal_components(x) %>% .[[4]] %>% length(), 10)
  expect_equal(principal_components(x) %>% .[[5]] %>% length(), 1)

})

test_that("principal_components computes correctly", {

  expect_equal(principal_components(x) %>% .[[1]] %>% .[1] %>% round(3), 1.303)
  expect_equal(principal_components(x) %>% .[[2]] %>% .[[1,1]] %>% round(2), 0.31)
  expect_equal(principal_components(x) %>% .[[3]] %>% .[[1,1]] %>% round(2), -1.33)
  expect_equal(principal_components(x) %>% .[[4]] %>% .[1] %>% round(2), 0.07)
  expect_true(principal_components(x) %>% .[[5]] %>% !.)

})
