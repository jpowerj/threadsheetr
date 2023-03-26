demo_combined_path <- function() {
  return(system.file("extdata/combined/", package="threadsheetr", mustWork = TRUE))
}

demo_all_entries_fpath <- function() {
  return(system.file("extdata/combined/", "all_entries.rds", package="threadsheetr",
                     mustWork = TRUE))
}

demo_data_path <- function() {
  return(system.file("extdata/", package="threadsheetr", mustWork = TRUE))
}

demo_grid_long_fpath <- function() {
  fname <- "cp_membership_num_num_long.rds"
  return(system.file("extdata/combined/", fname, package="threadsheetr", mustWork = TRUE))
}

demo_parsed_path <- function() {
  return(system.file("extdata/parsed/", package = "threadsheetr", mustWork = TRUE))
}

demo_spec_fpath <- function() {
  return(system.file("extdata", "demo.yaml", package = "threadsheetr",
                     mustWork = TRUE))
}
