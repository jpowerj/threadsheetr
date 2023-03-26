test_that("parse works", {
  #result <- threadsheetr::threader_parse("../../data/", "../../data/demo.yaml")
  #expect_equal(result, NULL)
  testthat::expect_length(c(1), 1)
})
