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
threader_parse <- function(data_path = NULL, spec_fpath = NULL, fname_glob = NULL,
                           verbose = FALSE) {
  if (is.null(data_path)) {
    data_path <- .demo_data_path()
  }
  if (is.null(spec_fpath)) {
    spec_fpath <- .demo_spec_fpath()
  }
  if (is.null(fname_glob)) {
    fname_glob = "*.csv"
  }
  # Load data spec
  spec <- .parse_spec_file(spec_fpath)
  # Find the variable
  varname <- spec$grid$varname
  # Find all variable datafiles
  fpath_glob <- file.path(data_path, fname_glob)
  if (verbose) { print(paste0("Finding fpaths using ",fpath_glob))}
  data_fpaths <- Sys.glob(fpath_glob)
  if (verbose) { print(paste0("Found fpaths: ",data_fpaths))}
  # Create the "parsed" subfolder if it doesn't already exist
  parsed_path <- file.path(data_path, "parsed")
  if (!dir.exists(parsed_path)) {
    dir.create(parsed_path)
  }
  all_dfs <- rlang::new_environment()
  for (cur_fpath in data_fpaths) {
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
  #return(all_dfs)
  return_env <- rlang::new_environment()
  rlang::env_poke(return_env, "data", all_dfs)
  rlang::env_poke(return_env, "path", parsed_path)
  return(return_env)
}

# Helper Functions

#' Apply the addto/rename/exclude rules given in the spec file
#'
#' @param df The tibble you want to transform
#' @param cur_index_spec The spec for the index variable you want to transform
#'
#' @return A new tibble with the addto/rename/exclude rules applied
#' @export
#'
#' @examples
.apply_rules <- function(df, cur_index_spec) {
  cur_index_varname <- cur_index_spec$varname
  # First we apply the [rename/exclude] rules for this var, since they don't
  # require parsing string cols into numeric cols
  rule_tibble <- .rules_to_tibble(cur_index_spec)
  # Now we can apply the specific operations
  # 1. Renames
  renames <- rule_tibble %>% dplyr::filter(op == "rename")
  rename_vec <- setNames(renames$val2, renames$val1)
  # And apply
  df <- df %>%
    dplyr::mutate(!!as.symbol(cur_index_varname) := dplyr::recode(!!as.symbol(cur_index_varname), !!!rename_vec))
  # 2. Addto
  adds <- rule_tibble %>% dplyr::filter(op == "addto")
  dft <- .transpose_df(df)
  for (i in 1:nrow(adds)) {
    cur_val1 <- adds[i,"val1"] %>% dplyr::pull()
    cur_val2 <- adds[i,"val2"] %>% dplyr::pull()
    dft <- .do_addto(dft, cur_val1, cur_val2)
  }
  # And now we undo the transpose
  df <- .undo_transpose(dft)
  # Finally, drop the rows with index in excludes
  excludes <- rule_tibble %>% dplyr::filter(val2 == "exclude")
  exclude_vals <- excludes %>% dplyr::select(val1) %>% dplyr::pull()
  # And, for this, we can just drop the list of val1 entries
  df <- df %>% dplyr::filter(!(!!dplyr::sym(cur_index_varname) %in% exclude_vals))
  return(df)
}

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

.clean_index_vals <- function(df, spec, verbose = FALSE) {
  # Get index var names
  index_specs <- spec$index_rules
  for (cur_index_spec in index_specs) {
    cur_index_varname <- cur_index_spec$varname
    # If its not in the df, continue to next var
    if (!(cur_index_varname %in% names(df))) {
      next
    }
    # First, ffill
    should_ffill <- FALSE
    if ("ffill" %in% names(cur_index_spec)) {
      should_ffill <- cur_index_spec$ffill
      if (should_ffill) {
        df <- df %>% tidyr::fill(dplyr::all_of(cur_index_varname))
      }
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
    # Check if there are any rename/addto/exclude rules
    if ("rules" %in% names(cur_index_spec)) {
      # For debugging
      if (verbose) { old_df <- df }
      df <- .apply_rules(df, cur_index_spec)
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

#' Carry out an addto rule
#'
#' @param dft The *transpose* of the tibble, so that each column is a different
#'     unit of observation and each row is a time value
#' @param val1 The column in `dft` whose values we want to add to the `val2` column
#' @param val2 The column in `dft` which is being added to
#'
#' @return New tibble where the values in the `val1` column have been added to
#'     the `val2` column, and the `val1` column has been dropped
#' @export
#'
#' @examples
.do_addto <- function(dft, val1, val2) {
  # If val1 isn't a col in the df (remember that we're working with the transposed
  # version), then there's nothing to add, so we return the df as-is
  if (!(val1 %in% colnames(dft))) {
    return(dft)
  }
  # Check if val2 is already a col in the df
  # If it is, then we can do the adding
  if (val2 %in% colnames(dft)) {
    # Ensure they're both numeric
    dft_num <- dft %>% .ensure_numeric(colname = val1)
    dft_num <- .ensure_numeric(dft_num, colname = val2)
    dft_num <- dft_num %>%
      dplyr::mutate(!!dplyr::sym(val2) := !!dplyr::sym(val2) + !!dplyr::sym(val1))
    # And now we can drop the val1 column
    dft_num <- dft_num %>% dplyr::select(-dplyr::all_of(val1))
    return(dft_num)
  } else {
    # Otherwise, it becomes a rename operation
    # Rename the val1 col to be val2
    dft_num <- dft %>% dplyr::rename(val2 = val1)
    return(dft_num)
  }
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
  if (stringr::str_detect(fpath, "1971-1990_Hoover_Yearbooks.csv")) {
    print("hoover")
  }
  ### Part 1: Detect the headers/data split
  header_result <- .detect_header_rows(fpath, spec, verbose=verbose)
  df_head <- header_result$df_head
  num_headers <- header_result$num_headers
  print(paste0("num_headers: ",num_headers))
  ### Part 2: Load csv
  # Need the keep_default_na=False option otherwise it stores missing vals as
  # np.nan even with dtype=str -___-
  use_default_col_names <- FALSE
  if (num_headers == 0) {
    # Use the headers as-is, parsed by read_csv
    use_default_col_names <- TRUE
  }
  df <- readr::read_csv(fpath, skip=num_headers, show_col_types = FALSE,
                        col_names = use_default_col_names, col_types = list(.default = "c"))
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
  # Now, since we have a df parsed from the original .csv file, we let .process_df()
  # take over (since we want a callable function if the user wants to start with
  # an already-clean df rather than a raw .csv file)
  return(.process_df(df, spec, verbose=verbose))
}

#' Parse an already-cleaned tibble
#'
#' @param df The df, either constructed separately or produced by `.process_data_file()`.
#' @param spec The specification object, returned by `.parse_spec_file()`.
#' @param verbose Optional: If `TRUE`, additional debugging information will be printed.
#'
#' @return A new df with the data from the passed-in `df` in a standardized long format.
#' @export
#'
#' @examples
.process_df <- function(df, spec, dataset_name = NULL, verbose = FALSE) {
  if (is.null(dataset_name)) {
    dataset_name <- "in-memory"
  }
  ### Part 4: Lowercase any strings in the index and remove "notes" row if it exists
  # ORDER MATTERS: Need to clean the headers first, since clean_index depends on
  # having the correct headers
  df <- .clean_header_vals(df, spec)
  df <- .clean_index_vals(df, spec, verbose=verbose)
  # Ensure that the tibble is as long as possible
  grid_varname <- spec$grid$varname
  # Here we process differently based on whether it's
  # (a) *cross-sectional* or *time-series* data, or
  # (b) *panel* data
  if (spec$grid$type == "panel") {
    time_varname <- spec$grid$time_varname
    unit_varname <- spec$grid$unit_of_obs
    vars_to_keep <- c(grid_varname, time_varname, unit_varname)
    # There are three possibilities for the particular .csv: long, wide, or multiindex
    is_multiindex <- any(stringr::str_detect(names(df), "@@"))
    if (is_multiindex) {
      # Turn the MultiIndex into a longer dataset with single index
      df_long <- df %>%
        tidyr::pivot_longer(cols = dplyr::contains('@@'), names_pattern = "(.*)@@(.*)",
                            names_to = c("varname",time_varname),
                            values_transform = list("varname"=as.character,time_varname=as.character))
    } else {
      #is_long <- ((time_varname %in% names(df)) && (unit_varname %in% names(df)))
      # If we're here, we know it's in long form, so we just need to make it
      # one step longer, with "varname" and "value" columns
      df_long <- df %>% dplyr::select(dplyr::all_of(vars_to_keep)) %>%
        tidyr::pivot_longer(cols = -dplyr::all_of(c(time_varname, unit_varname)),
                            names_to = c("varname"))
    }
  } else if (spec$grid$type == "ts") {
    time_varname <- spec$grid$time_varname
    vars_to_keep <- c(grid_varname, time_varname)
    df_long <- df %>% dplyr::select(dplyr::all_of(vars_to_keep)) %>%
      tidyr::pivot_longer(cols = -dplyr::all_of(c(time_varname)),
                          names_to = c("varname"))
  } else {
    # spec$grid$type == "cs"
    unit_varname <- spec$grid$unit_of_obs
    vars_to_keep <- c(grid_varname, unit_varname)
    df_long <- df %>% dplyr::select(dplyr::all_of(vars_to_keep)) %>%
      tidyr::pivot_longer(cols = -dplyr::all_of(c(unit_varname)),
                          names_to = c("varname"))
  }

  # ### Part 5: Make sure the year, val columns are all-numeric
  #if (num_headers > 1) {
  #  df_long <- df_long %>%
  #    dplyr::mutate(dplyr::across(dplyr::contains('@@'), as.numeric))
  #} else {
  # Remove commas
  df_long <- df_long %>%
    dplyr::mutate(value = stringr::str_replace_all(value,",",""))
  # Convert the value col to numeric
  df_long$value <- as.numeric(df_long$value)
  # As well as the timevar col, if panel or time-series
  if ((spec$grid$type == "panel") || (spec$grid$type == "ts")) {
    df_long[[time_varname]] <- as.numeric(df_long[[time_varname]])
  }
  # ### Part 6: Add row_num and source_id columns
  df_long <- tibble::rowid_to_column(df_long, var="row_id")
  df_long <- df_long %>% dplyr::mutate(source_id = dataset_name)
  return(df_long)
}

.process_dfs <- function(dfs, spec, dataset_names = NULL, verbose = FALSE) {
  if (is.null(dataset_names)) {
    dataset_names <- paste0("df",as.character(1:length(dfs)))
  }
  return_env <- rlang::new_environment()
  for (i in 1:length(dfs)) {
    cur_df <- dfs[[i]]
    cur_name <- dataset_names[[i]]
    processed_df <- .process_df(cur_df, spec, dataset_name=cur_name, verbose=verbose)
    rlang::env_poke(return_env, cur_name, processed_df)
  }
  return(return_env)
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


