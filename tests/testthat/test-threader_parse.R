test_that(".process_df() correctly parses panel datasets", {
  fake_df <- tibble::tribble(
    ~year, ~country, ~cp_membership_num, ~extra_col, ~notes,
    1971, "Albania", 100, "blah1", "note1",
    NA, "Algeria", 200, "blah2", "note2",
    NA, "Zimbabwe", 300, "blah3", NA,
    1972, "Albania", 400, "blah4", NA,
    NA, "Algeria", 500, "blah5", "note5",
    NA, "Zimbabwe", 600, "blah6", NA,
    1980, "Albania", 700, "blah7", "note7",
    NA, "Algeria", 800, "blah8", "note8",
    NA, "Zimbabwe", 900, "blah9", NA
  )
  spec <- .parse_spec_file(demo_spec_fpath())
  result <- .process_df(fake_df, spec, verbose = TRUE)
  expected_df <- tibble::tribble(
    ~year, ~country, ~cp_membership_num, ~extra_col, ~row_id, ~source_id,
    1971, "Albania", 100, "blah1", 0, "fake.csv",
    1971, "Algeria", 200, "blah2", 1, "fake.csv",
    1971, "Zimbabwe", 300, "blah3", 2, "fake.csv",
    1972, "Albania", 400, "blah4", 3, "fake.csv",
    1972, "Algeria", 500, "blah5", 4, "fake.csv",
    1972, "Zimbabwe", 600, "blah6", 5, "fake.csv",
    1980, "Albania", 700, "blah7", 6, "fake.csv",
    1980, "Algeria", 800, "blah8", 7, "fake.csv",
    1980, "Zimbabwe", 900, "blah9", 8, "fake.csv"
  )
})
