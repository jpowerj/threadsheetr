#' Print a list of summary statistics
#'
#' Can be used as an initial diagnostic, to check that the pipeline ran successfully
#'
#' @param grid_long_fpath The filepath to the generated *long-format* grid data
#'
#' @return A list with keys `num_obs`, `mean_est`, `num_nonnull`, and `num_null`.
#' @export
#'
#' @examples
#' grid_sumstats <- threader_sumstats()
threader_sumstats <- function(grid_long_fpath = NULL, group_by_var = NULL) {
  if (is.null(grid_long_fpath)) {
    grid_long_fpath <- demo_grid_long_fpath()
  }
  num_df <- readRDS(grid_long_fpath)
  num_obs <- num_df %>% dplyr::count() %>% dplyr::pull()
  mean_est <- num_df %>%
    dplyr::summarize(mean(est, na.rm = TRUE)) %>%
    dplyr::pull()
  num_nonnull <- num_df %>%
    dplyr::filter(!is.na(est)) %>%
    dplyr::count() %>%
    dplyr::pull()
  num_null <- num_obs - num_nonnull
  # Now we do the groupby stats, if requested
  num_groups <- NA
  group_message <- NULL
  if (!is.null(group_by_var)) {
    group_df <- num_df %>% dplyr::group_by(!!as.symbol(group_by_var))
    num_groups <- group_df %>% dplyr::n_groups()
    group_message <- paste0("Grouping by ",group_by_var,": ",num_groups," groups")
  }
  sumstats <- list(
    num_obs=num_obs,
    mean_est=mean_est,
    num_nonnull=num_nonnull,
    num_null=num_null,
    num_groups=num_groups
  )
  message(paste0("N = ",num_obs," (",num_nonnull," non-NA, ",num_null," NA)"))
  message(paste0("Mean Estimate = ",mean_est))
  if (!is.null(group_message)) {
    message(group_message)
  }
  return(sumstats)
}
