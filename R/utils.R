.demo_combined_path <- function() {
  return(system.file("extdata/combined/", package="threadsheetr", mustWork = TRUE))
}

.demo_all_entries_fpath <- function() {
  return(system.file("extdata/combined/", "all_entries.rds", package="threadsheetr",
                     mustWork = TRUE))
}

.demo_data_fpaths <- function() {
  data_path <- .demo_data_path()
  data_glob <- file.path(data_path, "*.csv")
  data_fpaths <- Sys.glob(data_glob)
  return(data_fpaths)
}

.demo_data_path <- function() {
  return(system.file("extdata/cp_membership_num/", package="threadsheetr", mustWork = TRUE))
}

.demo_grid_long_fpath <- function() {
  fname <- "cp_membership_num_num_long.rds"
  return(system.file("extdata/combined/", fname, package="threadsheetr", mustWork = TRUE))
}

.demo_parsed_path <- function() {
  return(system.file("extdata/parsed/", package = "threadsheetr", mustWork = TRUE))
}

.demo_spec_fpath <- function() {
  return(system.file("extdata", "demo_panel.yaml", package = "threadsheetr", mustWork = TRUE))
}

.test_cs_spec_fpath <- function() {
  return(system.file("extdata", "test_cs.yaml", package = "threadsheetr", mustWork = TRUE))
}

.test_data_path <- function() {
  return(system.file("extdata", "data_test", package = "threadsheetr", mustWork = TRUE))
}

.test_panel_long_fpath <- function() {
  return(system.file("extdata", "data_test/test_panel_long.csv", package = "threadsheetr", mustWork = TRUE))
}

.test_panel_spec_fpath <- function() {
  return(system.file("extdata", "test_panel.yaml", package = "threadsheetr", mustWork = TRUE))
}

.test_panel_wide_fpath <- function() {
  return(system.file("extdata", "data_test/test_panel_wide.csv", package = "threadsheetr", mustWork = TRUE))
}

.test_ts_spec_fpath <- function() {
  return(system.file("extdata", "test_ts.yaml", package = "threadsheetr", mustWork = TRUE))
}

.ensure_numeric <- function(df, colname = NULL) {
  # First remove commas
  cleaned_df <- .remove_commas_df(df, colname)
  # Now convert
  if (is.null(colname)) {
    # Convert *all* columns
    old_index <- cleaned_df %>% dplyr::select(1)
    old_numeric <- cleaned_df %>% dplyr::select(-1)
    new_numeric <- old_numeric %>% dplyr::mutate_all(as.numeric)
    new_combined <- dplyr::bind_cols(old_index, new_numeric)
    return(new_combined)
  } else {
    converted_df <- cleaned_df %>%
      dplyr::mutate(!!dplyr::sym(colname) := as.numeric(!!dplyr::sym(colname)))
    return(converted_df)
  }
}

#' Generate a fake tibble, for testing
#'
#' @return Fake tibble
#' @export
#'
#' @examples
.gen_test_df <- function() {
  fake_df <- tibble::tribble(
    ~country, ~`num@@1970`, ~`num@@1980`,
    "Albania", "1,000", "2,000",
    "Algeria", "100", "1,000,000",
    "Zimbabwe", NA, "1,234,567"
  )
  return(fake_df)
}

.gen_test_df_numeric <- function() {
  fake_df <- tibble::tribble(
    ~country, ~`num@@1970`, ~`num@@1980`,
    "Albania", 1000, 2000,
    "Algeria", 100, 1000000,
    "Zimbabwe", NA, 1234567
  )
  return(fake_df)
}

.gen_test_dft <- function() {
  fake_df <- tibble::tribble(
    ~year, ~"Albania", ~"Algeria", ~"Zimbabwe",
    1970, 1000, 100, NA,
    1980, 2000, 1000000, 1234567
  )
  return(fake_df)
}

.get_col_type <- function(df, colname) {
  ct <- df %>% dplyr::select(!!dplyr::sym(colname)) %>%
    dplyr::summarise_all(class) %>% dplyr::pull()
  return(ct)
}

#' Remove commas from a column (or all columns) of a tibble
#'
#' @param df tibble
#' @param colname Optional: list of columns you want to convert
#'
#' @return tibble with commas removed
#' @export
#'
#' @examples
.remove_commas_vec <- function(vec) {
  return(stringr::str_replace_all(vec,",",""))
}

#' Remove commas across all (numeric) columns in a tibble
#'
#' @param df tibble
#'
#' @return New tibble where all numeric columns have commas removed
#' @export
#'
#' @examples
.remove_commas_df <- function(df, colname = NULL) {
  if (is.null(colname)) {
    old_index <- df %>% dplyr::select(1)
    old_numeric <- df %>% dplyr::select(-1)
    new_numeric <- old_numeric %>% dplyr::mutate_all(.remove_commas_vec)
    new_combined <- dplyr::bind_cols(old_index, new_numeric)
    return(new_combined)
  } else {
    # Check if it's numeric or string
    coltype <- .get_col_type(df, colname)
    if (coltype == "numeric") {
      # No conversion needed (no commas in a numeric)
      return(df)
    }
    new_df <- df %>%
      dplyr::mutate(!!dplyr::sym(colname) := .remove_commas_vec(df[,colname] %>% dplyr::pull()))
    return(new_df)
  }
}

.rules_to_tibble <- function(cur_index_spec) {
  # Check if it has any rules
  if (!("rules" %in% names(cur_index_spec))) {
    return(NULL)
  }
  rule_list <- cur_index_spec$rules
  rules_flat <- purrr::flatten(rule_list)
  # This gets val1
  val1 <- names(rules_flat)
  op <- as.character(lapply(rules_flat, names))
  val2 <- as.character(lapply(rules_flat, as.character))
  rule_tibble <- tibble::tibble(val1=val1,op=op,val2=val2)
  return(rule_tibble)
}

.transpose_df <- function(df) {
  # For the adding, because of how dplyr works, its 1000x easier to work with
  # the transpose of df
  dft <- t(df[,-1])
  cols <- purrr::flatten(df[,1])
  colnames(dft) <- cols
  index_header <- names(df)[1]
  dft <- tibble::as_tibble(dft, rownames = index_header, .name_repair = "minimal")
  return(dft)
}

.undo_transpose <- function(dft) {
  df <- t(dft[,-1])
  index_varname <- names(dft)[1]
  index_vals <- dft %>% dplyr::select(dplyr::sym(index_varname)) %>% dplyr::pull()
  index_vec <- c(index_varname, index_vals)
  df_tib <- tibble::as_tibble(df, rownames = index_varname, .name_repair = "minimal")
  colnames(df_tib) <- index_vec
  return(df_tib)
}
