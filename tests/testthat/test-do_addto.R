test_that("do_addto() works", {
  fake_df <- .gen_test_df_numeric()
  country1 = "Algeria"
  country2 = "Albania"
  fake_dft <- .transpose_df(fake_df)
  transformed_df <- .do_addto(fake_dft, country1, country2)
  expected_df <- tibble::tribble(
    ~country, ~`Albania`, ~`Zimbabwe`,
    "num@@1970", 1100, NA,
    "num@@1980", 1002000, 1234567
  )
  expect_equal(transformed_df, expected_df)
})
