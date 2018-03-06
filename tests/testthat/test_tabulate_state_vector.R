context("tabulate_state_vector")

test_that("tabulate_state_vector output has correct dimensions", {

  expect_true(tabulate_state_vector(security_logs, 30) %>% is.data.frame())
  expect_equal(tabulate_state_vector(security_logs, 30) %>% dim(), c(10, 54))
  expect_equal(tabulate_state_vector(security_logs, 5, 4, 2) %>% dim(), c(60, 25))

})

test_that("tabulate_state_vector computes correctly", {

  expect_equal(tabulate_state_vector(security_logs, 30) %>% .[[1, 1]], 4)
  expect_equal(tabulate_state_vector(security_logs, 30) %>% .[[4,2]], 11)
  expect_equal(tabulate_state_vector(security_logs, 30, 4, 2) %>% .[[4, 6]], 9)
  expect_equal(tabulate_state_vector(security_logs, 7,partial_block = F) %>% nrow,42)
  expect_equal(tabulate_state_vector(security_logs, 7,partial_block = T) %>% nrow,43)
  expect_equal(tabulate_state_vector(security_logs, 30, na.rm = T) %>% .[[8,5]],2)

})

test_that("tabulate_state_vector throws correct errors", {

  expect_error(tabulate_state_vector(security_logs, "A"))
  expect_error(tabulate_state_vector(security_logs, 30, "A"))
  expect_error(tabulate_state_vector(security_logs, 30, 40, "A"))
  expect_error(tabulate_state_vector(security_logs, 7,partial_block = F) %>% .[[43,1]])
  expect_error(tabulate_state_vector(security_logs))
  expect_error(tabulate_state_vector())
  expect_error(tabulate_state_vector(4))

})
