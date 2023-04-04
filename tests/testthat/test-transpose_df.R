test_that("transpose_df() works", {
  fake_df <- .gen_test_df_numeric()
  transformed_df <- .transpose_df(fake_df)
  expected_df <- tibble::tribble(
    ~country, ~"Albania", ~"Algeria", ~"Zimbabwe",
    "num@@1970", 1000, 100, NA,
    "num@@1980", 2000, 1000000, 1234567
  )
  expect_equal(transformed_df, expected_df)
})
