test_that("tabulate_state_vector output has correct dimensions", {

  expect_true(tabulate_state_vector(security_logs, 30) %>% is.data.frame())
  expect_equal(tabulate_state_vector(security_logs, 30) %>% dim(), c(10, 43))
  expect_equal(tabulate_state_vector(security_logs, 5, 4, 2) %>% dim(), c(60, 20))

})

test_that("tabulate_state_vector computes correctly", {

  expect_equal(tabulate_state_vector(security_logs, 30) %>% .[[1, 1]], 4)
  expect_equal(tabulate_state_vector(security_logs, 30) %>% .[[4, 4]], 9)
  expect_equal(tabulate_state_vector(security_logs, 30, 4, 2) %>% .[[4, 4]], 0)

})
