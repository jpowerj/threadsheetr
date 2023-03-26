#' Parse .yaml-format spec file
#'
#' @param spec_fpath
#'
#' @return Parsed spec object
.parse_spec_file <- function(spec_fpath = NULL) {
  if (is.null(spec_fpath)) {
    spec_fpath <- demo_spec_fpath()
  }
  # For now this just returns the result from read_yaml
  return(yaml::read_yaml(spec_fpath))
}

.spec_get_excluded <- function(parsed_spec) {
  grid_spec <- parsed_spec$grid
  if ("exclude_units" %in% names(grid_spec)) {
    return(grid_spec$exclude_units)
  } else {
    return(NULL)
  }
}

.spec_get_time_end <- function(parsed_spec) {
  return(parsed_spec$grid$time_end)
}

.spec_get_time_start <- function(parsed_spec) {
  return(parsed_spec$grid$time_start)
}

.spec_get_time_varname <- function(parsed_spec) {
  return(parsed_spec$grid$time_varname)
}

.spec_get_unit <- function(parsed_spec) {
  return(parsed_spec$grid$unit_of_obs)
}

.spec_get_varname <- function(parsed_spec) {
  return(parsed_spec$grid$varname)
}
