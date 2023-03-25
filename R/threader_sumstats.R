threader_sumstats <- function(grid_fpath = "./data/parsed/cp_membership_num_grid.rds") {
  num_df <- readRDS(grid_fpath)
  num_obs <- num_df %>% dplyr::count() %>% dplyr::pull()
  mean_est <- num_df %>% dplyr::summarize(mean(est, na.rm = TRUE)) %>% dplyr::pull()
  num_nonnull <- num_df %>%
    dplyr::filter(!is.na(est)) %>%
    dplyr::count() %>%
    dplyr::pull()
  num_null <- num_obs - num_nonnull
  sumstats <- list(
    num_obs=num_obs,
    mean_est=mean_est,
    num_nonnull=num_nonnull,
    num_null=num_null
  )
  message(paste0("N = ",num_obs," (",num_nonnull," non-NA, ",num_null," NA)"))
  message(paste0("Mean Estimate = ",mean_est))
  return(sumstats)
}
