#' Step 3: Construct the Grid of Estimates
#'
#' @return
#' @export
#'
#' @examples
threader_create_grid <- function(combined_fpath = "./data/parsed/combined.rds",
                                 spec_fpath = "./data/demo.yaml") {
  # Interpret the combined_fpath as being inside the data dir
  output_path <- dirname(combined_fpath)
  # Load the combined file
  combined_df <- readRDS(combined_fpath)
  spec <- yaml::read_yaml(spec_fpath)
  grid_spec <- spec$grid
  grid_var <- grid_spec$varname
  grid_unit <- grid_spec$unit_of_obs
  grid_time_var <- grid_spec$time_var
  grid_time_start <- grid_spec$time_start
  grid_time_end <- grid_spec$time_end
  grid_exclude <- NULL
  if ("exclude_units" %in% names(grid_spec)) {
    grid_exclude <- grid_spec$exclude_units
  }
  # And create!
  result <- .create_grid(combined_df, grid_var, grid_unit, grid_time_var,
                        grid_time_start,
                        grid_time_end, output_path, exclude=grid_exclude)
  return(result)
}

.create_grid <- function(estimates_df, grid_varname, unit_of_obs, time_varname,
                        time_start, time_end, output_path, exclude = NULL){
  message(paste0("=====[ creating grid for [",grid_varname,"] x [",unit_of_obs,"], t={",time_start,", ..., ",time_end,"} ]====="))
  ## And the source-trustworthiness file
  #trust_fpath = os.path.join(pl.input_path, "trust.csv")
  #trust_df = pd.read_csv(trust_fpath)
  #init_trust_dict(trust_df)
  # Get all the entries for the var we care about
  estimates_df <- estimates_df %>% dplyr::filter(varname == grid_varname)
  # And now filter to the ones within range
  estimates_df <- estimates_df %>%
    dplyr::filter((!!as.symbol(time_varname) >= time_start) & (!!as.symbol(time_varname) <= time_end))
  # And remove langs/countries in exclude list
  if (!is.null(exclude)) {
    estimates_df <- estimates_df %>%
      dplyr::filter(!(!!as.symbol(unit_of_obs) %in% exclude))
  }
  ## Print a summary of the years+units spanned
  # Get the unique years
  unique_times <- estimates_df %>%
    dplyr::distinct(!!as.symbol(time_varname)) %>%
    dplyr::arrange(!!as.symbol(time_varname))
  print_summary(time_varname, unique_times)
  ## And a summary of the langs/countries
  unique_units <- estimates_df %>%
    dplyr::distinct(!!as.symbol(unit_of_obs)) %>%
    dplyr::arrange(!!as.symbol(unit_of_obs))
  print_summary(unit_of_obs, unique_units)
  ## Now we can create the grid, starting with the index column
  num_df_long <- dplyr::cross_join(unique_units, unique_times)
  info_df_long <- num_df_long
  #return_obj <- list(num_df=num_df_long, combined_df=combined_df)
  # Aggregate the combined df so there's one value per country x year
  agg_df <- estimates_df %>%
    dplyr::group_by(country, year) %>%
    dplyr::summarise(est = mean(value))
  # And now merge so that we have a full grid, even for cells without any estimates
  # num_df_long has every possible country x year combo, so we want to left merge
  # the estimates into that
  num_df_merged <- num_df_long %>%
    dplyr::left_join(agg_df, by=c(unit_of_obs,time_varname))
  info_df_merged <- info_df_long %>%
    dplyr::mutate(info = "")
  # Save these completed but long-form dfs
  .save_dfs(num_df_merged, info_df_merged, output_path, grid_varname, "_long")
  # Finally, un-pivot it, so that it's back to being a grid
  num_df_wide <- num_df_merged %>%
    tidyr::pivot_wider(names_from = as.symbol(time_varname),
                       values_from = "est")
  info_df_wide <- info_df_merged %>%
    tidyr::pivot_wider(names_from = as.symbol(time_varname),
                       values_from = "info")
  #num_df_grid <- num_df_merged %>% tidyr::pivot_wider()
  # And save the grids
  saved_fpaths <- .save_dfs(num_df_wide, info_df_wide, output_path,
                            grid_varname, "_wide")
  num_fpath <- saved_fpaths$num
  info_fpath <- saved_fpaths$info
  # And return the dfs
  return_obj <- list(num_df=num_df_wide, info_df=info_df_wide,
                     num_fpath=num_fpath, info_fpath=info_fpath)
  return(return_obj)
}

#' Given a DF of all observations (estimates) for the cell, combine them
#' in some way to generate a single value
#'
#' For now [the default] just average them all, dropping any N/As, but
#' returning np.nan if this results in no entries
#'
#' Combine the rows in a df of estimates into one final estimate
#'
#' @param unit_varname
#' @param unit_val
#' @param time_varname
#' @param time_val
#' @param combined_df
#'
#' @return
#' @export
#'
#' @examples
compute_cell_value <- function(num_long_df, unit_varname, unit_val, time_varname, time_val,
                               combined_df) {
  print(paste0(unit_val,", ",time_val))
  # Get the subset of combined_df containing this unit x year
  cell_df <- combined_df %>%
    filter((!!as.symbol(unit_varname) == unit_val) & (!!as.symbol(time_varname) == time_val))
  # TODO: incorporate trust.csv (via trust_dict) here
  if (length(cell_df) == 0) {
    # No estimates at all
    return(new.env(num_result=np.nan, info_result="0 estimates"))
  }
  # Otherwise, we know there's at least one value. Drop the NAs and return
  # the mean

  #notnull_df <- estimate_df.dropna(subset=["value"])
  #if len(notnull_df) == 0:
  #  # Find out which rows got dropped
  #  dropped_df = estimate_df[estimate_df["value"].isnull()]
  #num_null = len(dropped_df)
  #estimate_str = "|".join(get_estimate_list(dropped_df))
  #estimate_info = f"0 non-null estimates|---|{num_null} null estimates:|{estimate_str}"
  #return np.nan, estimate_info

  # And if we're here, we know there's at least one non-null value
  #entry_values = notnull_df["value"].values
  #all_agree = all([x == entry_values[0] for x in entry_values])
  #if all_agree and len(entry_values) > 1:
  #  conflict_text = " consistent"
  #elif not all_agree and len(entry_values) > 1:
  #  conflict_text = " conflicting"
  #else:
  #  # Only one entry, so can't have conflicts
  #  conflict_text = ""
  ##print(entry_values)
  #final_estimate = entry_values.mean()
  ## And now we need to go round the mean if it's an int var
  #var_spec = pl.get_varspec(str(estimate_df["variable"].iloc[0]))
  #if var_spec.get_datatype() == "int":
  #  final_estimate = round(final_estimate)
  #num_estimates = len(entry_values)
  #estimate_lines = get_estimate_list(notnull_df)
  #estimate_str = "|".join(estimate_lines)
  #estimate_info = f"{num_estimates}{conflict_text} estimates:|{estimate_str}"
  #return final_estimate, estimate_info
  # (An Environment, with $num_result and $info_result properties)
  return(cell_result)
}

#' Produces estimate list, formatted for Google Sheet, from the provided df
#'
#' @return
#' @export
#'
#' @examples
get_estimate_list <- function(estimate_df) {
  estimate_strs <- estimate_df %>% tibble::rowid_to_column(var="index") %>%
    tidyr::unite("values", c(country, value), sep=" ") %>%
    tidyr::unite("output", c(index, values), sep=": ") %>%
    dplyr::select(output) %>% as.list()
  return(estimate_strs)
  #return [f"{rnum}: {rtuple[1]['value']} ({rtuple[1]['source_id']})" for rnum, rtuple in enumerate(estimate_df.iterrows())]
}

#' Prints just the first two and last two entries, with "..." in between, unless
#' `print_all` is set to True
#'
#' @param var_name
#' @param val_list
#' @param print_all
#'
#' @return
#' @export
#'
#' @examples
print_summary <- function(var_name, val_list, print_all = FALSE) {
  print(paste0("Values for grid axis [",var_name,"]:"))
  if (print_all) {
    print(val_list)
    return()
  }
  if (length(val_list) <= 4) {
    print(val_list)
  } else {
    print(paste0("[",val_list[0],", ",val_list[1],", ..., ",val_list[-2],", ",val_list[-1],"]"))
  }
}

#' Helper function that saves the numeric and info dfs together
#'
#' @param num_df
#' @param info_df
#' @param fname_prefix
#'
#' @return
#' @export
#'
#' @examples
.save_dfs <- function(num_df, info_df, output_path, grid_varname,
                      fname_suffix) {
  num_fname_prefix = paste0(grid_varname,"_num",fname_suffix)
  num_rds_fpath = file.path(output_path, paste0(num_fname_prefix,".rds"))
  saveRDS(num_df, num_rds_fpath)
  info_fname_prefix = paste0(grid_varname,"_info",fname_suffix)
  info_rds_fpath = file.path(output_path, paste0(info_fname_prefix,".rds"))
  saveRDS(info_df, info_rds_fpath)
  # And finally, .csv versions for human reading
  num_csv_fpath = file.path(output_path, paste0(num_fname_prefix,".csv"))
  readr::write_csv(num_df, num_csv_fpath)
  info_csv_fpath = file.path(output_path, paste0(info_fname_prefix,".csv"))
  readr::write_csv(info_df, info_csv_fpath)
  return_obj <- list(num=num_rds_fpath, info=info_rds_fpath)
}
