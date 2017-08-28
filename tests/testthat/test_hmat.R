test_that("hmat produces a ggplot object", {
  expect_is(hmat(security_logs,block_length = 5),"ggplot")
  expect_is(security_logs %>% tabulate_state_vector(5) %>%
                 hmat(input = "SV"),"ggplot")
  expect_is(security_logs %>% tabulate_state_vector(5) %>%
                 mc_adjust() %>%
                 mahalanobis_distance("both") %>%
                 hmat(input = "MD"),"ggplot")

})

test_that("hmat error messages", {
  expect_error(hmat(NULL))
  expect_error(hmat(security_logs,input = "SVP",block_length=5))
  expect_error(hmat(6))
  expect_error(hmat(security_logs))
})
