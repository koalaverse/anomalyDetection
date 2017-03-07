set.seed(123)
x <- matrix(rnorm(200*3), ncol = 10)

pca <- principal_components(x)

test_that("principal_components_result provides proper messages and warnings", {

  expect_that(principal_components_result(), throws_error())
  expect_that(principal_components_result(pca, results = 6), throws_error())
  expect_that(principal_components_result(pca, results = "wrong"), throws_error())

})

test_that("principal_components_result output has correct dimensions", {

  expect_equal(principal_components_result(pca) %>% length, 10)
  expect_equal(principal_components_result(pca, 1) %>% length(), 10)
  expect_equal(principal_components_result(pca, 2) %>% dim(), c(10, 10))
  expect_equal(principal_components_result(pca, 3) %>% dim(), c(10, 10))
  expect_equal(principal_components_result(pca, 4) %>% dim(), c(60, 10))

  expect_equal(principal_components_result(pca, eigval) %>% length(), 10)
  expect_equal(principal_components_result(pca, eigvec) %>% dim(), c(10, 10))
  expect_equal(principal_components_result(pca, pca_loadings) %>% dim(), c(10, 10))
  expect_equal(principal_components_result(pca, pca_scores) %>% dim(), c(60, 10))

})

test_that("principal_components_result gets the right output", {

  expect_equal(principal_components_result(pca) %>% .[1] %>% round(3), 1.703)
  expect_equal(principal_components_result(pca, 1) %>% .[1] %>% round(3), 1.703)
  expect_equal(principal_components_result(pca, 2) %>% .[1,1] %>% round(2), -0.38)
  expect_equal(principal_components_result(pca, 3) %>% .[1,1] %>% round(2), -0.5)
  expect_equal(principal_components_result(pca, 4) %>% .[1,1] %>% round(2), 1.27)

})
