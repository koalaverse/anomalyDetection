context("mc_adjust")

set.seed(123)
x <- matrix(runif(100), ncol = 10)

test_that("mc_adjust provides proper messages and warnings", {

  expect_that(x %>% mc_adjust(min_var = .5), throws_error())
  expect_false(x %>% mc_adjust(max_cor = .8) %>% colnames() %>% is.null())

})
