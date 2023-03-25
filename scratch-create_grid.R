load_all()

test_print <- function(thing1) {
  print("Printing thing1...")
  print(thing1)
  return("new thing")
}

fake_estimate <- function(data_row, unit_varname, time_varname, combined_df) {
  cur_unit <- data_row[as.symbol(unit_varname)]
  cur_time <- data_row[as.symbol(time_varname)]
  # Extract the combined_df rows for this unit x time
  cur_estimates <- combined_df %>%
    filter((!!as.symbol(unit_varname) == cur_unit) & (!!as.symbol(time_varname) == cur_time))
  return(length(cur_estimates))
  #return(paste0(cur_unit,", ",cur_time))
}

result <- threadsheetr::threader_create_grid()
num_df <- result$num_df
info_df <- result$info_df
num_fpath <- result$num_fpath

#dp_result <- num_df %>%
#  dplyr::rowwise() %>%
#  dplyr::mutate(estimate = fake_estimate(dplyr::pick(dplyr::everything()),
#                                         "country", "year", combined_df))

# Check the saved file
result_rds <- readRDS(num_fpath)

