.parse_spec_file <- function(spec_fpath) {
  # For now this just returns the result from read_yaml
  return(yaml::read_yaml(spec_fpath))
}
