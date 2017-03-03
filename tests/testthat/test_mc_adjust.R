set.seed(123)
x <- matrix(runif(100), ncol = 10)

test_that("mc_adjust provides proper messages and warnings", {

  expect_that(x %>% mc_adjust(), shows_message())
  expect_that(x %>% mc_adjust(min_var = .5), throws_error())
  expect_that(x %>% mc_adjust(max_cor = .5), shows_message())
  expect_that(x %>% mc_adjust(max_cor = .2), throws_error())
  expect_that(x %>% mc_adjust(min_var = .05, max_cor = .5, action = "select"), shows_message())
  expect_false(x %>% mc_adjust(max_cor = .8) %>% colnames() %>% is.null())

})
