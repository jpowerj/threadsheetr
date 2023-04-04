test_that("remove_commas_df works without colname", {
  fake_df <- .gen_test_df()
  cleaned_df <- .remove_commas_df(fake_df)
  expected_df <- tibble::tribble(
    ~country, ~`num@@1970`, ~`num@@1980`,
    "Albania", "1000", "2000",
    "Algeria", "100", "1000000",
    "Zimbabwe", NA, "1234567"
  )
  expect_equal(cleaned_df, expected_df)
})

test_that("remove_commas_df works with colname", {
  fake_df <- .gen_test_df()
  cleaned_df <- .remove_commas_df(fake_df, colname = "num@@1980")
  expected_df <- tibble::tribble(
    ~country, ~`num@@1970`, ~`num@@1980`,
    "Albania", "1,000", "2000",
    "Algeria", "100", "1000000",
    "Zimbabwe", NA, "1234567"
  )
  expect_equal(cleaned_df, expected_df)
})
