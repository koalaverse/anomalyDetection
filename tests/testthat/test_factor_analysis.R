set.seed(123)
x = matrix(rnorm(200*3), ncol = 10)

hc_points <- x %>% horns_curve()

test_that("factor_analysis provides proper messages and warnings", {

  expect_that(factor_analysis(x), throws_error())
  expect_that(factor_analysis(hc_points), throws_error())

})

test_that("factor_analysis output has correct dimensions", {

  expect_equal(factor_analysis(x, hc_points) %>% length(), 5)
  expect_true(factor_analysis(x, hc_points) %>% is.list)
  expect_equal(factor_analysis(x, hc_points) %>% .[[1]] %>% dim(), c(10, 6))
  expect_equal(factor_analysis(x, hc_points) %>% .[[2]] %>% dim(), c(60, 6))
  expect_equal(factor_analysis(x, hc_points) %>% .[[3]] %>% dim(), c(10, 6))
  expect_equal(factor_analysis(x, hc_points) %>% .[[4]] %>% dim(), c(60, 6))
  expect_equal(factor_analysis(x, hc_points) %>% .[[5]] %>% length(), 1)

})

test_that("factor_analysis computes correctly", {

  expect_equal(factor_analysis(x, hc_points) %>% .[[1]] %>% .[1,1] %>% round(3), -0.499)
  expect_equal(factor_analysis(x, hc_points) %>% .[[2]] %>% .[1,1] %>% round(2), 0.97)
  expect_equal(factor_analysis(x, hc_points) %>% .[[3]] %>% .[1,1] %>% round(2), 0.02)
  expect_equal(factor_analysis(x, hc_points) %>% .[[4]] %>% .[1,1] %>% round(2), 0.9)
  expect_equal(factor_analysis(x, hc_points) %>% .[[5]], 6)

})
