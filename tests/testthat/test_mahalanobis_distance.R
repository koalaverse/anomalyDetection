set.seed(123)
x <- data.frame(C1 = rnorm(100), C2 = rnorm(100), C3 = rnorm(100))


test_that("mahalanobis_distance output has correct dimensions", {

  expect_equal(x %>% dplyr::mutate(MD = mahalanobis_distance(x)) %>% dim(), c(100, 4))
  expect_true(x %>% dplyr::mutate(MD = mahalanobis_distance(x)) %>% is.data.frame())
  expect_equal(x %>% mahalanobis_distance(output = "bd") %>% dim(), c(100, 3))
  expect_true(x %>% mahalanobis_distance(output = "bd") %>% is.matrix())
  expect_equal(x %>% mahalanobis_distance(output = "both") %>% dim(), c(100, 4))
  expect_true(x %>% mahalanobis_distance(output = "both") %>% is.matrix())

})

test_that("mahalanobis_distance computes correctly", {

  expect_equal(x %>% mahalanobis_distance() %>% .[1] %>% round(2), 5.48)
  expect_equal(x %>% mahalanobis_distance(output = "bd") %>% .[1, 1] %>% round(2) %>% .[[1]], 0.71)
  expect_equal(x %>% mahalanobis_distance(output = "bd", normalize = TRUE) %>% .[1, 1] %>% round(2) %>% .[[1]], 0.08)

})
