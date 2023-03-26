#' Step 2: Combine Parsed Data Files
#'
#' @return Output: a giant csv file with a row for *every* piece of data we have
#' @export
#'
#' @examples
#' combined_df <- threader_combine()
#'
#' @export
threader_combine <- function(parsed_path = "./data/parsed/", verbose = FALSE) {
    parsed_fpaths <- Sys.glob(file.path(parsed_path, "*.rds"))
    if (verbose) {
      print("parsed_fpaths:")
      print(parsed_fpaths)
    }
    # Remove combined.rds, if it already exists, just in case
    parsed_fpaths <- parsed_fpaths[!stringr::str_detect(parsed_fpaths, "combined.rds")]
    full_df <- dplyr::tibble()
    for (cur_fpath in parsed_fpaths) {
      cur_df <- readRDS(cur_fpath)
      full_df <- dplyr::bind_rows(full_df, cur_df)
    }
    csv_output_fpath <- file.path(parsed_path, "combined.csv")
    readr::write_csv(full_df, csv_output_fpath)
    rds_output_fpath <- stringr::str_replace(csv_output_fpath, ".csv", ".rds")
    if (verbose) { print(paste0("Saving to ",rds_output_fpath)) }
    saveRDS(full_df, rds_output_fpath)
    return(full_df)
}
