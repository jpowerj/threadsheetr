test_that("apply_rules() retains time column for long datasets", {
  fake_dft <- tibble::tribble(
    ~year, ~"Albania", ~"Algeria", ~"Zimbabwe",
    "1970", 1000, 100, NA,
    "1980", 2000, 1000000, 1234567
  )
  transformed_df <- .apply_rules(df, year_spec)
})
