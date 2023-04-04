library(threadsheetr)

fake_df <- threadsheetr::.gen_test_df()

cleaned_df <- threadsheetr::.remove_commas(fake_df)
