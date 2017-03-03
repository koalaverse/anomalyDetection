set.seed(123)
x = matrix(rnorm(200*3), ncol = 10)
colnames(x) = paste0("C", 1:ncol(x))

m1 <- x %>% mahalanobis_distance("bd", normalize = TRUE)

test_that("bd_row provides proper messages and warnings", {

  expect_that(bd_row(m1), throws_error())
  expect_that(bd_row(m1, 250), throws_error())
  expect_that(bd_row(m1, 5, n = 20), throws_error())
  expect_equal(tryCatch(bd_row(m1, 1:5), warning = function(w) "warning"), "warning")

})

test_that("bd_row output is correct dimensions", {

  expect_equal(bd_row(m1, 5, n = 5) %>% length(), 5)
  expect_equal(bd_row(m1, 5) %>% length(), 10)
  expect_true(bd_row(m1, 5) %>% is.numeric())
  expect_true(bd_row(m1, 5) %>% is.atomic())

})

test_that("bd_row computes correctly", {

  expect_equal(bd_row(m1, 5, 1) %>% round(2) %>% .[[1]], 1.58)
  expect_equal(bd_row(m1, 36, 1) %>% round(2) %>% .[[1]], 3.02)

})
