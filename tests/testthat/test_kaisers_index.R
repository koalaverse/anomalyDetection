set.seed(123)
x <- matrix(rnorm(200*3), ncol = 10)

x %>%
  horns_curve() %>%
  factor_analysis(x, hc_points = .) %>%
  factor_analysis_results(fa_loadings_rotated)


test_that("kaisers_index provides proper messages and warnings", {

  expect_that(kaisers_index(letters), throws_error())

})

test_that("kaisers_index output is correct object and dimensions", {

  expect_true(kaisers_index(x) %>% is.atomic)
  expect_true(kaisers_index(x) %>% is.numeric)
  expect_equal(kaisers_index(x) %>% length(), 1)

})

test_that("kaisers_index computes appropriately", {

  expect_equal(kaisers_index(x) %>% round(3), 0.397)

})
