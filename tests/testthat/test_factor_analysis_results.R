context("factor_analysis_result")

set.seed(123)
x = matrix(rnorm(200*3), ncol = 10)

fa <- x %>%
  horns_curve() %>%
  factor_analysis(x, hc_points = .)

test_that("factor_analysis_result provides proper messages and warnings", {

  expect_that(factor_analysis_results(), throws_error())
  expect_that(factor_analysis_results(fa, results = 6), throws_error())
  expect_that(factor_analysis_results(fa, results = "wrong"), throws_error())

})

test_that("factor_analysis_result output has correct dimensions", {

  expect_equal(factor_analysis_results(fa) %>% dim(), c(10, 6))
  expect_equal(factor_analysis_results(fa, 1) %>% dim(), c(10, 6))
  expect_equal(factor_analysis_results(fa, 2) %>% dim(), c(60, 6))
  expect_equal(factor_analysis_results(fa, 3) %>% dim(), c(10, 6))
  expect_equal(factor_analysis_results(fa, 4) %>% dim(), c(60, 6))
  expect_equal(factor_analysis_results(fa, 5) %>% length(), 1)

  expect_equal(factor_analysis_results(fa, fa_loadings) %>% dim(), c(10, 6))
  expect_equal(factor_analysis_results(fa, fa_scores) %>% dim(), c(60, 6))
  expect_equal(factor_analysis_results(fa, fa_loadings_rotated) %>% dim(), c(10, 6))
  expect_equal(factor_analysis_results(fa, fa_scores_rotated) %>% dim(), c(60, 6))
  expect_equal(factor_analysis_results(fa, num_factors) %>% length(), 1)

})

test_that("factor_analysis_result gets the right output", {

  skip_on_cran()

  expect_equal(factor_analysis_results(fa) %>% .[1,1] %>% round(3), -0.499)
  expect_equal(factor_analysis_results(fa, 1) %>% .[1,1] %>% round(3), -0.499)
  expect_equal(factor_analysis_results(fa, 2) %>% .[1,1] %>% round(2), 0.97)
  expect_equal(factor_analysis_results(fa, 3) %>% .[1,1] %>% round(2), 0.02)
  expect_equal(factor_analysis_results(fa, 4) %>% .[1,1] %>% round(2), 0.9)
  expect_equal(factor_analysis_results(fa, 5), 6)

})
