test_that("hmat produces a ggplot object", {
  expect_error(hmat(security_logs,input = "SVP",block_length=5))
})
