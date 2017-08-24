test_that("tabulate_state_vector output has correct dimensions", {

  expect_true(tabulate_state_vector(security_logs, 30) %>% is.data.frame())
  expect_equal(tabulate_state_vector(security_logs, 30) %>% dim(), c(10, 54))
  expect_equal(tabulate_state_vector(security_logs, 5, 4, 2) %>% dim(), c(60, 25))

})

test_that("tabulate_state_vector computes correctly", {

  expect_equal(tabulate_state_vector(security_logs, 30) %>% .[[1, 1]], 4)
  expect_equal(tabulate_state_vector(security_logs, 30) %>% .[[4, 4]], 1)
  expect_equal(tabulate_state_vector(security_logs, 30, 4, 2) %>% .[[4, 4]], 1)
  expect_equal(tabulate_state_vector(security_logs, 7,keep = F) %>% .[[42,50]],6)
  expect_equal(tabulate_state_vector(security_logs, 30, na.rm = T) %>% .[[4,4]],1)


})

test_that("tabulate_state_vector throws correct errors", {

  expect_error(tabulate_state_vector(security_logs, "A"))
  expect_error(tabulate_state_vector(security_logs, 30, "A"))
  expect_error(tabulate_state_vector(security_logs, 7,keep = F) %>% .[[43,1]])

})
