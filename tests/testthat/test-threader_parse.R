# Cross-sectional datasets

test_that(".process_df() correctly parses cross-sectional datasets", {
  data_path <- .test_data_path()
  fname_glob <- "test_cs_*.csv"
  spec_fpath <- .test_cs_spec_fpath()
  result_df <- threader_parse(data_path, spec_fpath, fname_glob = fname_glob,
                              verbose = TRUE)
  expected_df <- tibble::tribble(
    ~year, ~country, ~varname, ~value, ~source_id, ~row_id,
    1971, "albania", "cp_membership", 100, "test_cs_1.csv", 1,
    1972, "albania", "cp_membership", 400, "test_cs_2.csv", 4,
    1980, "albania", "cp_membership", 700, "test_cs_3.csv", 7,
    1971, "algeria", "cp_membership", 200, "test_cs_1.csv", 2,
    1972, "algeria", "cp_membership", 500, "test_cs_2.csv", 5,
    1980, "algeria", "cp_membership", 800, "test_cs_3.csv", 8,
    1971, "zimbabwe", "cp_membership", 300, "test_cs_1.csv", 3,
    1972, "zimbabwe", "cp_membership", 600, "test_cs_2.csv", 6,
    1980, "zimbabwe", "cp_membership", 900, "test_cs_3.csv", 9
  )
  expect_equal(result_df, expected_df)
})

# Panel datasets

test_that(".process_df() correctly parses long panel datasets", {
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
  spec <- .parse_spec_file(.test_panel_spec_fpath())
  result_df <- .process_df(fake_df, spec, verbose = TRUE)
  expected_df <- tibble::tribble(
    ~row_id, ~year, ~country, ~varname, ~value, ~source_id,
    1, 1971, "albania", "cp_membership_num", 100, "in-memory",
    2, 1971, "algeria", "cp_membership_num", 200, "in-memory",
    3, 1971, "zimbabwe", "cp_membership_num", 300, "in-memory",
    4, 1972, "albania", "cp_membership_num", 400, "in-memory",
    5, 1972, "algeria", "cp_membership_num", 500, "in-memory",
    6, 1972, "zimbabwe", "cp_membership_num", 600, "in-memory",
    7, 1980, "albania", "cp_membership_num", 700, "in-memory",
    8, 1980, "algeria", "cp_membership_num", 800, "in-memory",
    9, 1980, "zimbabwe", "cp_membership_num", 900, "in-memory"
  )
  expect_equal(result_df, expected_df)
})

test_that(".process_df() correctly parses wide panel datasets", {
  test_path <- .test_data_path()
  spec_fpath <- .test_panel_spec_fpath()
  wide_panel_glob <- "test_panel_wide.csv"
  result_df <- threader_parse(test_path, spec_fpath, fname_glob = wide_panel_glob, verbose = TRUE)
  expected_df <- tibble::tribble(
    ~row_id, ~year, ~country, ~varname, ~value, ~source_id,
    1, 1971, "albania", "cp_membership_num", 100, "in-memory",
    2, 1971, "algeria", "cp_membership_num", 200, "in-memory",
    3, 1971, "zimbabwe", "cp_membership_num", 300, "in-memory",
    4, 1972, "albania", "cp_membership_num", 400, "in-memory",
    5, 1972, "algeria", "cp_membership_num", 500, "in-memory",
    6, 1972, "zimbabwe", "cp_membership_num", 600, "in-memory",
    7, 1980, "albania", "cp_membership_num", 700, "in-memory",
    8, 1980, "algeria", "cp_membership_num", 800, "in-memory",
    9, 1980, "zimbabwe", "cp_membership_num", 900, "in-memory"
  )
  expect_equal(result_df, expected_df)
})
