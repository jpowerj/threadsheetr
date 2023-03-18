#' Parse Data Files
#'
#' @return
#' @export
#'
#' @examples
threadsheetr_parse <- function(data_path, spec_fpath) {
  # Load data spec
  spec <- yaml::read_yaml(spec_fpath)
  # Find the variable
  varname <- spec$grid$varname
  # Find all variable datafiles
  var_glob <- paste0(data_path,"/",v,"/*.csv")
  var_fpaths <- Sys.glob(var_glob)
  print(var_fpaths)
  all_dfs <- new.env()
  for (cur_fpath in var_fpaths) {
    print(paste0("Parsing ",cur_fpath))
    processed_df <- process_data_file(cur_fpath, spec)
    rlang::env_poke(all_dfs, cur_fpath, processed_df)
  }
  #env_print(all_dfs)
  return(all_dfs)
}

clean_index_vals <- function(df) {
  # Get index var name
  index_name <- colnames(df)[1]
  # Get index var type
  index_type <- df %>% summarize_all(class) %>% pull(1)
  # Lowercase index values
  if (index_type == "character") {
    df[[1]] <- tolower(df[[1]])
  }
  return(df)
}

clean_header_vals <- function(df) {
  # Drop notes columns
  df <- df %>% select(-contains('notes', ignore.case = TRUE))
  return(df)
}

split_header_cell <- function(header_cell_str) {
  header_elts <- str_split_one(header_cell_str, '\\\\')
  index_name <- header_elts[1]
  header_name <- header_elts[2]
  return(c(index_name, header_name))
}

detect_header_rows <- function(fpath, spec) {
  # Load the csv *without* assuming header rows
  df_head <- readr::read_csv(fpath, col_names = FALSE, show_col_types = FALSE)
  # Find number of rows with '\\\\' in the index col
  df_head <- df_head %>%
    tidyr::drop_na(X1) %>%
    filter(stringr::str_detect(.$X1,'\\\\'))
  num_headers <- df_head %>% count() %>% pull()
  print(paste0("Detected ",num_headers," headers"))
  return_obj <- new.env()
  return_obj$num_headers = num_headers
  # Now collapse down into *one* header row, if we need to
  if (num_headers == 1) {
    # Parse just the one entry with \\
    header_elts <- split_header_cell(df_head[1,1] %>% dplyr::pull())
    index_name <- header_elts[1]
    header_name <- header_elts[2]
    # Now we can make the first column name *just* the column label
    df_head[1,1] <- index_name
    # And do replacements if need be
    df_head <- check_replace_headers(df_head, spec)
    return_obj$index_name = index_name
    return_obj$header_name = header_name
  } else {
    # Multiple header rows, so keep track of the header for each
    # But first do replacement rules on *both* rows
    df_head[1,] <- check_replace_headers(df_head[1,], spec)
    df_head[2,] <- check_replace_headers(df_head[2,], spec)
    # Top header
    top_header_elts <- split_header_cell(df_head[1,1] %>% dplyr::pull())
    # Should *only* have a row label
    top_header_name <- top_header_elts[2]
    # Now the bottom should have a column\\row label
    bottom_header_elts <- split_header_cell(df_head[2,1] %>% dplyr::pull())
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

#' Check if a variable is present in a tibble
#'
#' We need this because of the special '@@' character, which separates levels
#' of the MultiIndex
#'
#' @param df
#' @param varname
#'
#' @return
#' @export
#'
#' @examples
var_in_df <- function(varname, df) {
  # This gets the *full* list of varnames
  full_varlist <- names(df) %>% stringr::str_split("@@") %>% unlist()
  # And now we can just use %in%
  return(varname %in% full_varlist)
}

check_replace_headers <- function(header_row, spec) {
  # Get the header rules
  all_rules <- spec$header_map
  rule_varnames <- names(all_rules)
  for (cur_varname in rule_varnames) {
    #print("--- loop iter ---")
    other_varname <- all_rules[[cur_varname]]
    header_row <- header_row %>% replace(. == cur_varname, other_varname)
  }
  #print("headers replaced")
  return(header_row)
}

process_data_file <- function(fpath, spec) {
  fname <- basename(fpath)
  fname_elts = str_split_one(fname, "_")
  if (endsWith(fname_elts[1], "_entries")) {
    print(paste0("process_data_file(): processing already-long dataset ",fname))
    return(process_entries_file(fpath, spec))
  }
  ### Part 1: Detect the headers/data split
  header_result <- detect_header_rows(fpath, spec)
  df_head <- header_result$df_head
  num_headers <- header_result$num_headers
  index_name <- header_result$index_name
  print(paste0("num_headers: ",num_headers))
  print(paste0("index_name: ",index_name))
  ### Part 2: Load csv
  # Need the keep_default_na=False option otherwise it stores missing vals as
  # np.nan even with dtype=str -___-
  df <- readr::read_csv(fpath, skip=num_headers, show_col_types = FALSE,
                        col_types = list(.default = "c"))
  ### Parts 3a/3b: Parse index column (3a)/Parse header rows (3b)
  # Annoying: df.columns.names if multiindex, df.columns.name if single index
  #if len(df.columns.names) > 1:
  #  # Multiple header rows. Pandas will load this as where the first column
  #  # of the header rows (what we want to be split) will be treated as a list
  #  # of *column* names
  #  last_colname = df.columns.names[-1]
  # And now we can remove the index-name part of the column headers
  colnames(df) <- c(index_name, df_head[1,-1] %>% unlist())
  ### Part 4: Lowercase any strings in the index and remove "notes" row if it exists
  df <- clean_index_vals(df)
  df <- clean_header_vals(df)
  # ### Part 4: Doing magic with the unstack() function
  # First we make sure any rename rules are applied
  df <- check_replace_headers(df, spec)
  # Ensure that the tibble is as long as possible
  time_var <- spec$grid$time_var
  unit_var <- spec$grid$unit_of_obs
  main_varname <- spec$grid$varname
  vars_to_keep <- c(time_var, unit_var, main_varname)
  df <- df %>%
    tidyr::pivot_longer(cols = contains('@@'), names_pattern = "(.*)@@(.*)",
                        names_to = c("varname",time_var),
                        values_transform = list("varname"=as.character,time_var=as.character))
  # ### Part 5: Make sure the year column is all-numeric and apply the index
  # ### rules to the other axis
  # # There could be a "notes" row, so we convert all entries that can be parsed
  # # as a year to int (rather than .astype(int) which leads to an exception if
  # # there's a "notes" row)
  # #df["year"] = df["year"].apply(lambda x: int(x) if (type(x) == int or plu.year_reg.match(x)) else x)
  # # Except now we drop any "notes" rows
  # df = df[df["year"].apply(str) != "notes"]
  # # Now that we're dropping the notes, we can use the much simpler .astype() approach
  # #breakpoint()
  # df["year"] = df["year"].astype(int)
  # unit_of_obs = next(c for c in df.columns if c not in ["unit","variable","year"])
  # print(f"unit_of_obs={unit_of_obs}")
  # # First we check whether or not this unit_of_obs is part of our study
  # if unit_of_obs not in pl.data_spec.get_index_rules():
  #   # If not, we just jump to the next one
  #   return None
  # # And now we apply the index rules to the unit_of_obs column
  # apply_index_rules(df, unit_of_obs, pl.data_spec.get_index_rules())
  # # And also that the unit is capitalized (each word) and trimmed of whitespace
  # df[unit_of_obs] = df[unit_of_obs].apply(plu.clean_index_val)
  # ### Part 6: Add row_num and source_id columns
  # # We need to use this special add_df_col() helper function because sometimes
  # # the DFs have a single row of col headers but sometimes MultiIndex columns
  # df = plu.add_df_col(df, "row_num", np.arange(len(df)))
  # # And a column with just the source_id
  # df = plu.add_df_col(df, "source_id", fname)
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
  return(df)
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
