#' Parse .yaml-format spec file
#'
#' @param spec_fpath
#'
#' @return Parsed spec object
#'
#' @examples
#' spec <- .parse_spec_file("./data/demo.yaml")
.parse_spec_file <- function(spec_fpath) {
  # For now this just returns the result from read_yaml
  return(yaml::read_yaml(spec_fpath))
}
