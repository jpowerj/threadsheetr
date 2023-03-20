#' Step 3: Construct the Grid of Estimates
#'
#' @return
#' @export
#'
#' @examples
threader_create_grid <- function(combined_fpath = "./data/parsed/combined.rds") {
  # Load the combined file
  combined_df <- readRDS(combined_fpath)
  estimates <- get_estimate_list(combined_df)
  return(estimates)
}

#' Produces estimate list, formatted for Google Sheet, from the provided df
#'
#' @return
#' @export
#'
#' @examples
get_estimate_list <- function(estimate_df) {
  estimate_strs <- grid_df %>% tibble::rowid_to_column(var="index") %>%
    tidyr::unite("values", c(country, value), sep=" ") %>%
    tidyr::unite("output", c(index, values), sep=": ") %>%
    select(output) %>% as.list()
  return(estimate_strs)
  #return [f"{rnum}: {rtuple[1]['value']} ({rtuple[1]['source_id']})" for rnum, rtuple in enumerate(estimate_df.iterrows())]
}
