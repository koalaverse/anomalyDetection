test_that("inspect_block provides proper messages and warnings", {

  expect_that(inspect_block(data = letters, 30), throws_error())
  expect_that(inspect_block(security_logs, "30"), throws_error())

})

test_that("inspect_block provides proper output", {

  expect_true(is.list(inspect_block(security_logs, 30)))
  expect_equal(inspect_block(security_logs, 30) %>% length(), 10)

})
