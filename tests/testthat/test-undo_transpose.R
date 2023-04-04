test_that("multiplication works", {
  dft <- tibble::tribble(
    ~country, ~"Albania", ~"Algeria", ~"Zimbabwe",
    "num@@1970", 1000, 100, NA,
    "num@@1980", 2000, 1000000, 1234567
  )
  transformed_df <- .undo_transpose(dft)
  expected_df <- .gen_test_df_numeric()
  expect_equal(transformed_df, expected_df)
})
