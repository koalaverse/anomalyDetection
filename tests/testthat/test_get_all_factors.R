
test_that("get_all_factors provides proper messages and warnings", {

  expect_that(get_all_factors(letters), throws_error())

})

test_that("get_all_factors output is a list", {

  expect_true(get_all_factors(1:10) %>% is.list)

})

test_that("get_all_factors computes appropriately", {

  expect_equal(get_all_factors(27) %>% .[[1]], c(1, 3, 9, 27))

})
