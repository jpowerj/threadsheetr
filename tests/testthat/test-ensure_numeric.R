test_that("ensure_numeric works without colname", {
  fake_df <- .gen_test_df()
  cleaned_df <- .ensure_numeric(fake_df)
  expected_df <- tibble::tribble(
    ~country, ~`num@@1970`, ~`num@@1980`,
    "Albania", 1000, 2000,
    "Algeria", 100, 1000000,
    "Zimbabwe", NA, 1234567
  )
  expect_equal(cleaned_df, expected_df)
})
