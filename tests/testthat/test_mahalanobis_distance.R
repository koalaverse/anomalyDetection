set.seed(123)
x <- data.frame(C1 = rnorm(100), C2 = rnorm(100), C3 = rnorm(100))


test_that("mahalanobis_distance output has correct dimensions", {

  expect_equal(x %>% dplyr::mutate(MD = mahalanobis_distance(x)) %>% dim(), c(100, 4))
  expect_true(x %>% dplyr::mutate(MD = mahalanobis_distance(x)) %>% is.data.frame())

})

