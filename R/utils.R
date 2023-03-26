demo_spec_fpath <- function() {
  return(system.file("extdata", "demo.yaml", package = "threadsheetr", mustWork = TRUE))
}

demo_parsed_path <- function() {
  return(system.file("extdata/parsed/", package = "threadsheetr", mustWork = TRUE))
}

demo_combined_fpath <- function() {
  return(system.file("extdata/parsed/", "combined.rds", package="threadsheetr",
                     mustWork = TRUE))
}

demo_data_path <- function() {
  return(system.file("extdata/", package="threadsheetr", mustWork = TRUE))
}
