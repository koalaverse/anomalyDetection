set.seed(123)
x <- matrix(rnorm(200*3), ncol = 10)


test_that("principal_components output has correct dimensions", {

  expect_equal(principal_components(x) %>% length(), 4)
  expect_true(principal_components(x) %>% is.list)
  expect_equal(principal_components(x) %>% .[[1]] %>% length(), 10)
  expect_equal(principal_components(x) %>% .[[2]] %>% dim(), c(10, 10))
  expect_equal(principal_components(x) %>% .[[3]] %>% dim(), c(10, 10))
  expect_equal(principal_components(x) %>% .[[4]] %>% dim(), c(60, 10))

})

test_that("principal_components computes correctly", {

  expect_equal(principal_components(x) %>% .[[1]] %>% .[1] %>% round(3), 1.703)
  expect_equal(principal_components(x) %>% .[[2]] %>% .[1,1] %>% round(2), -0.38)
  expect_equal(principal_components(x) %>% .[[3]] %>% .[1,1] %>% round(2), -0.5)
  expect_equal(principal_components(x) %>% .[[4]] %>% .[1,1] %>% round(2), 1.27)

})
