#' Step 1: Parse Data Files
#'
#' @param data_path Path to the directory containing the raw data you want to parse.
#' @param spec_fpath Filepath to the `.yaml`-format spec file
#' @param verbose Optional: If `TRUE`, prints additional debugging information.
#'
#' @return An environment where each key is an input file, and each value is the tibble generated from the data in that file
#' @export
#'
#' @examples
#' all_dfs <- threader_parse()
threader_parse <- function(data_path = NULL, spec_fpath = NULL,
                           verbose = FALSE) {
  if (is.null(data_path)) {
    data_path <- demo_data_path()
  }
  if (is.null(spec_fpath)) {
    spec_fpath <- demo_spec_fpath()
  }
  # Load data spec
  spec <- yaml::read_yaml(spec_fpath)
  # Find the variable
  varname <- spec$grid$varname
  # Find all variable datafiles
  var_glob <- paste0(data_path,"/",varname,"/*.csv")
  var_fpaths <- Sys.glob(var_glob)
  # Create the "parsed" subfolder if it doesn't already exist
  parsed_path <- paste0(data_path,"/parsed/")
  if (!dir.exists(parsed_path)) {
    dir.create(parsed_path)
  }
  all_dfs <- new.env()
  for (cur_fpath in var_fpaths) {
    print(paste0("Parsing ",cur_fpath))
    cur_fname <- basename(cur_fpath)
    processed_df <- .process_data_file(cur_fpath, spec, verbose=verbose)
    # Serialize the processed_df
    serialized_fname <- stringr::str_replace_all(cur_fname, ".csv", ".rds")
    serialized_fpath <- file.path(parsed_path, serialized_fname)
    saveRDS(processed_df, serialized_fpath)
    rlang::env_poke(all_dfs, cur_fpath, processed_df)
  }
  #env_print(all_dfs)
  return(all_dfs)
}

# Helper Functions

.check_replace_headers <- function(header_row, spec) {
  # First we check the grid rules
  grid_spec <- spec$grid
  # Grid has *one* varname, so just check this varname
  grid_varname <- grid_spec$varname
  grid_alt_names <- grid_spec$alt_names
  header_row <- .check_replace_headers_alts(header_row, spec, grid_varname, grid_alt_names)
  # Now the index rules (plural... so we need to loop)
  index_spec <- spec$index_rules
  for (cur_index_rule in index_spec) {
    cur_varname <- cur_index_rule$varname
    cur_alt_names <- cur_index_rule$alt_names
    header_row <- .check_replace_headers_alts(header_row, spec, cur_varname, cur_alt_names)
  }
  return(header_row)
}

.check_replace_headers_alts <- function(header_row, spec, varname, alt_var_names) {
  # Get the header rules for the grid var
  # See if the main varname is one of the headers
  if (any(header_row == varname, na.rm = TRUE)) {
    # We're good, it has the varname, just return as-is
    return(header_row)
  }
  # Otherwise, check the alternate names
  for (cur_alt_name in alt_var_names) {
    #print("--- loop iter ---")
    if (any(header_row == cur_alt_name, na.rm = TRUE)) {
      header_row <- replace(header_row, header_row == cur_alt_name, varname)
      # We return on first one we find (so that, the order in the .yaml file
      # matters, in terms of priority)
      return(header_row)
    }
  }
  return(header_row)
}

.clean_header_vals <- function(df, spec) {
  # Drop notes columns
  df <- df %>% dplyr::select(-dplyr::contains('notes', ignore.case = TRUE))
  return(df)
}

.clean_index_vals <- function(df, spec) {
  # Get index var names
  index_specs <- spec$index_rules
  for (cur_index_spec in index_specs) {
    cur_index_varname <- cur_index_spec$varname
    # If its not in the df, continue to next var
    if (!(cur_index_varname %in% names(df))) {
      next
    }
    # Check if it should be lowercased
    # Default is, leave as-is
    should_lowercase <- FALSE
    if ("lowercase" %in% names(cur_index_spec)) {
      should_lowercase <- cur_index_spec$lowercase
      if (should_lowercase) {
        # If so, replace it
        df[[cur_index_varname]] <- tolower(df[[cur_index_varname]])
      }
    }
    # And ffill
    should_ffill <- FALSE
    if ("ffill" %in% names(cur_index_spec)) {
      should_ffill <- cur_index_spec$ffill
      if (should_ffill) {
        df <- df %>% tidyr::fill(dplyr::all_of(cur_index_varname))
      }
    }
  }
  return(df)
}

.detect_header_rows <- function(fpath, spec, verbose = FALSE) {
  if (verbose) { print(paste0("detect_header_rows(): ",fpath)) }
  # Load the csv *without* assuming header rows
  df_head <- readr::read_csv(fpath, col_names = FALSE, show_col_types = FALSE)
  # Drop NA index rows
  df_head <- df_head %>% tidyr::drop_na(`X1`)
  # Find number of rows with '\\\\' in the index col
  df_head <- df_head %>%
    dplyr::filter(stringr::str_detect(`X1`,'\\\\'))
  num_headers <- df_head %>% dplyr::count() %>% dplyr::pull()
  print(paste0("Detected ",num_headers," headers"))
  return_obj <- new.env()
  return_obj$num_headers = num_headers
  # Now collapse down into *one* header row, if we need to
  if (num_headers == 0) {
    # The "normal" case, actually: it means that there are no \\ values in the
    # index. So, just assume that the remaining header vals are variable names,
    # and that the index header is already set correctly
    return_obj$header_name = "variable"
  } else if (num_headers == 1) {
    # Parse just the one entry with \\
    header_elts <- .split_header_cell(df_head[1,1] %>% dplyr::pull())
    index_name <- header_elts[1]
    header_name <- header_elts[2]
    # Now we can make the first column name *just* the column label
    df_head[1,1] <- index_name
    # And do replacements if need be
    df_head <- .check_replace_headers(df_head, spec)
    return_obj$index_name = index_name
    return_obj$header_name = header_name
  } else {
    # Multiple header rows, so keep track of the header for each
    # But first do replacement rules on *both* rows
    df_head[1,] <- .check_replace_headers(df_head[1,], spec)
    df_head[2,] <- .check_replace_headers(df_head[2,], spec)
    # Top header
    top_header_elts <- .split_header_cell(df_head[1,1] %>% dplyr::pull())
    # Should *only* have a row label
    top_header_name <- top_header_elts[2]
    # Now the bottom should have a column\\row label
    bottom_header_elts <- .split_header_cell(df_head[2,1] %>% dplyr::pull())
    index_name <- bottom_header_elts[1]
    bottom_header_name <- bottom_header_elts[2]
    my_paste0 <- function(mystr) paste0(mystr, collapse="@@")
    df_head <- df_head %>% dplyr::summarize(dplyr::across(dplyr::everything(), my_paste0))
    # But, update the first cell to be just index_name
    df_head[1,1] <- index_name
    return_obj$index_name = index_name
    return_obj$top_header_name = top_header_name
    return_obj$bottom_header_name = bottom_header_name
  }
  return_obj$df_head = df_head
  return(return_obj)
}


#' Process a raw data file, transforming it into Threadsheets format
#'
#' This is the main function for this file
#'
#' @param fpath The filepath to the dataset you want to process
#' @param spec The parsed specification data (returned by `.parse_spec_file()`)
#' @param verbose Optional: Set to `TRUE` to print more debugging info than usual
#'
#' @return The long-format tibble where each row is an estimate derived from the data file `fpath`
#'
#' long_df <- .process_data_file("./data/cp_membership_num/file.csv", spec, verbose = TRUE)
.process_data_file <- function(fpath, spec, verbose = FALSE) {
  fname <- basename(fpath)
  fname_elts <- stringr::str_split_1(fname, "_")
  if (endsWith(fname_elts[1], "_entries")) {
    #print(paste0("process_data_file(): processing already-long dataset ",fname))
    #return(process_entries_file(fpath, spec))
    print(paste0("Skipping already-long dataset ",fname))
    return(NULL)
  }
  ### Part 1: Detect the headers/data split
  header_result <- .detect_header_rows(fpath, spec, verbose=verbose)
  df_head <- header_result$df_head
  num_headers <- header_result$num_headers
  print(paste0("num_headers: ",num_headers))
  ### Part 2: Load csv
  # Need the keep_default_na=False option otherwise it stores missing vals as
  # np.nan even with dtype=str -___-
  df <- readr::read_csv(fpath, skip=num_headers, show_col_types = FALSE,
                        col_types = list(.default = "c"))
  # If the header detection returned 0, we need to manually set the index name now
  if (num_headers == 0) {
    index_name <- df[1,1] %>% dplyr::pull()
  } else {
    index_name <- header_result$index_name
  }
  ### Parts 3a/3b: Parse index column (3a)/Parse header rows (3b)
  # Annoying: df.columns.names if multiindex, df.columns.name if single index
  #if len(df.columns.names) > 1:
  #  # Multiple header rows. Pandas will load this as where the first column
  #  # of the header rows (what we want to be split) will be treated as a list
  #  # of *column* names
  #  last_colname = df.columns.names[-1]
  # And now we can remove the index-name part of the column headers
  #colnames(df) <- c(index_name, df_head[1,-1] %>% unlist())
  if (num_headers > 0) {
    colnames(df) <- df_head[1,] %>% unlist()
  }
  ### Part 4: Lowercase any strings in the index and remove "notes" row if it exists
  # ORDER MATTERS: Need to clean the headers first, since clean_index depends on
  # having the correct headers
  df <- .clean_header_vals(df, spec)
  df <- .clean_index_vals(df, spec)
  # First we make sure any rename rules are applied
  # Ensure that the tibble is as long as possible
  time_varname <- spec$grid$time_varname
  unit_varname <- spec$grid$unit_of_obs
  grid_varname <- spec$grid$varname
  vars_to_keep <- c(time_varname, unit_varname, grid_varname)
  # But, before we check, we make sure to do any necessary renames
  if (num_headers > 1) {
    # Here we know the unit var will be there
    df_long <- df %>%
      tidyr::pivot_longer(cols = dplyr::contains('@@'), names_pattern = "(.*)@@(.*)",
                          names_to = c("varname",time_varname),
                          values_transform = list("varname"=as.character,time_varname=as.character))
  } else {
    df_long <- df %>% dplyr::select(dplyr::all_of(vars_to_keep)) %>%
      tidyr::pivot_longer(cols = -dplyr::all_of(c(time_varname, unit_varname)), names_to = c("varname"))
  }
  # ### Part 5: Make sure the year, val columns are all-numeric
  #if (num_headers > 1) {
  #  df_long <- df_long %>%
  #    dplyr::mutate(dplyr::across(dplyr::contains('@@'), as.numeric))
  #} else {
  # Remove commas
  df_long <- df_long %>%
    dplyr::mutate(value = stringr::str_replace_all(value,",",""))
  # Convert to numeric
  df_long$value <- as.numeric(df_long$value)
  # ### Part 6: Add row_num and source_id columns
  df_long <- tibble::rowid_to_column(df_long, var="row_id")
  df_long <- df_long %>% dplyr::mutate(source_id = fname)
  # ### Part 7: Run the parser (turning the strings into ints/floats as desired)
  # ### on the value column
  # # parse_row_val needs to have the pipeline object, so this partial function
  # # lets us pass that while still using it in an apply() call
  # df.to_pickle("df_before_parse.pkl")
  # if debug:
  #   # Use a for loop
  #   results = []
  # for row_index, row in df.iterrows():
  #   row_parsed = parse_row_value(row, pl.data_spec, verbose=verbose)
  # results.append(row_parsed)
  # df[['value','unit']] = results
  # else:
  #   # Lambda function
  #   parse_row_partial = lambda x: parse_row_value(x, pl.data_spec)
  # df[["value", "unit"]] = df.apply(parse_row_partial, axis=1)
  # ### Part 5: Extract entries (i.e., turn into a long-format dataset essentially)
  # # Print statements for debugging
  # #print(f"process_file(): About to call extract_entries():\n***\ndf.index.name={df.index.name}\n***\ndf.index={df.index}\n***\ndf.columns={df.columns}")
  # #df.to_pickle("last_df.pkl")
  # # We also need to give it the pipeline (for getting the data specs)
  # #all_entries = extract_entries(pl, df)
  # ### Part 6: Take this entries list and make it into its own df
  # #long_df = pd.DataFrame(all_entries)
  # #breakpoint()
  return(df_long)
}

#
# process_entries_file <- function(fpath, spec) {
#   fname <- basename(fpath)
#   df <- readr::read_csv(fpath)
#   unit_of_obs = df.index.name.lower()
#   # So now that we got the name we don't need it to be an index anymore, we can
#   # convert it to a normal column
#   df.reset_index(inplace=True)
#   # Find the variable name by finding the col header that's not unit_of_obs
#   # and not year
#   var_names = [colname for colname in df.columns if colname.lower() not in [unit_of_obs,"year","notes"]]
#   if len(var_names) != 1:
#     raise Exception("_entries file must have a unique non-year variable")
#   var_name = var_names[0]
#   df["variable"] = var_name
#   # And now we can rename the actual var column "value"
#   df.rename(columns={var_name:"value"},inplace=True)
#   # Apply remapping rules to the index col
#   new_df = apply_index_rules(df, unit_of_obs, index_rules)
#   new_df["row_num"] = np.arange(len(new_df))
#   new_df["source_id"] = fname
#   return new_df
# }

.split_header_cell <- function(header_cell_str) {
  header_elts <- stringr::str_split_1(header_cell_str, '\\\\')
  index_name <- header_elts[1]
  header_name <- header_elts[2]
  return(c(index_name, header_name))
}


#' Check if a variable is present in a tibble
#'
#' We need this because of the special '@@' character, which separates levels
#' of the MultiIndex
#'
#' @param df The tibble you want to check the headers of
#' @param varname The variable you want to look for in the headers of `df`
#'
#' @return `TRUE` if one of the tibble headers is either (a) `varname` itself,
#' or (b) of the form "`varname`@@year"
#'
#' df <- tribble(
#'   ~country, ~`cp_membership_num@@1950`, ~`cp_membership_num@@1970`,
#'   Austria, 10000, 5000,
#'   Zimbabwe, 100, 50
#' )
#' .var_in_df("cp_membership_num", df)
.var_in_df <- function(varname, df) {
  # This gets the *full* list of varnames
  full_varlist <- names(df) %>% stringr::str_split("@@") %>% unlist()
  # And now we can just use %in%
  return(varname %in% full_varlist)
}


