fpath <- "./data/cp_membership/1971-1990_Hoover_Yearbooks.csv"

fname <- basename(fpath)
fname_elts = str_split_one(fname, "_")
if (endsWith(fname_elts[1], "_entries")) {
  print(paste0("process_data_file(): processing already-long dataset ",fname))
  return(process_entries_file(fpath, spec))
}
### Part 1: Detect the headers/data split
df_head <- detect_header_rows(fpath)
num_hrows <- df_head %>% count() %>% pull()
print(paste0("process_data_file(): Parsing ",fname," with ",num_hrows," header rows"))
### Part 2: Load csv
# Need the keep_default_na=False option otherwise it stores missing vals as
# np.nan even with dtype=str -___-
df <- readr::read_csv(fpath, skip=num_hrows)
### Parts 3a/3b: Parse index column (3a)/Parse header rows (3b)
# Annoying: df.columns.names if multiindex, df.columns.name if single index
#if len(df.columns.names) > 1:
#  # Multiple header rows. Pandas will load this as where the first column
#  # of the header rows (what we want to be split) will be treated as a list
#  # of *column* names
#  last_colname = df.columns.names[-1]
index_name <- str_split_one(hrows, '\\')[1]
header_name <- str_split_one(hrows, '\\')[2]
# And now we can remove the index-name part of the column headers
colnames(df) <- c(header_name, df_head[1,-1] %>% unlist())
