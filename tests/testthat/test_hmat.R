p <- security_logs %>%
  anomalyDetection::tabulate_state_vector(5) %>%
  anomalyDetection::mc_adjust() %>%
  anomalyDetection::mahalanobis_distance("both", normalize = FALSE) %>%
  tibble::as_tibble() %>%
  dplyr::mutate_(.dots = list(Block = quote(as.factor(1:n())))) %>%
  dplyr::mutate_(Ranked = ~ rank(-MD, ties = "random")) %>%
  tidyr::gather_("Variable","BD",
                 names(.)[-c(1,length(names(.))-1,length(names(.)))]) %>%
  dplyr::filter_(.dots = ~ Ranked <= 20) %>%
  dplyr::mutate_(Variable = ~ substr(Variable,1,nchar(Variable)-3)) %>%
  ggplot2::ggplot(ggplot2::aes_string(x = "Block", y = "Variable",
                                      color = "MD", size = "BD")) +
  ggplot2::geom_point()

test_that("hmat produces a ggplot object", {
  expect_equal(hmat(security_logs,block_length = 5),p)
  expect_equal(security_logs %>% tabulate_state_vector(5) %>%
                 hmat(input = "SV"),p)
  expect_equal(security_logs %>% tabulate_state_vector(5) %>%
                 mc_adjust() %>%
                 mahalanobis_distance("both") %>%
                 hmat(input = "MD"),p)

})

test_that("hmat error messages", {
  expect_error(hmat(NULL))
  expect_error(hmat(security_logs,input = "SVP",block_length=5))
  expect_error(hmat(6))
  expect_error(hmat(security_logs))
})
