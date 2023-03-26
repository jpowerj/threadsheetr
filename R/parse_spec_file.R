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
