load_all()

local_fpath <- "./data/parsed/cp_membership_num_num_wide.csv"

gh_username <- "jpowerj"
gh_repo_name <- "ts-grids"
gh_repo_path <- file.path("public_grids", "cp_membership_num_num_wide.csv")
commit_msg <- "New version"
gh_name <- "JPJ"
gh_email <- "jjacobs3@cs.stanford.edu"

#content <- readr::read_file("./data/parsed/cp_membership_num_num_wide.csv")
#content_raw <- readr::read_file_raw("./data/parsed/cp_membership_num_num_wide.csv")
content_bin <- readBin(local_fpath, raw(), file.info(local_fpath)$size)
#content_b64 <- base64enc::base64encode(serialize(content, NULL))
content_b64 <- base64enc::base64encode(content_bin)

# Check if file exists
existing_sha <- tryCatch(
  {
    gh_result <- gh(
  'GET /repos/{owner}/{repo}/contents/{path}',
  owner = 'jpowerj',
  repo = 'ts-grids',
  path = 'public_grids/cp_membership_num_num_wide.csv'
  #headers = list('X-GitHub-Api-Version'='2022-11-28')
  )
    gh_result$sha
  },
  error=function(cond) {
    print("Error")
    return(NULL)
  }
)

#old_sha <- exists_result$sha

api_str <- paste0('PUT /repos/',gh_username,'/',gh_repo_name,'/contents/',gh_repo_path)
if (is.null(existing_sha)) {
  push_result <- gh(api_str,
     owner = gh_username,
     repo = gh_repo_name,
     path = gh_repo_path,
     message = commit_msg,
     committer = list(name=gh_name, email=gh_email),
     content = content_b64,
     headers=list('X-GitHub-Api-Version'='2022-11-28')
  )
} else {
  # Need to provide the old sha
  push_result <- gh(api_str,
     owner = gh_username,
     repo = gh_repo_name,
     path = gh_repo_path,
     message = commit_msg,
     committer = list(name=gh_name, email=gh_email),
     sha = existing_sha,
     content = content_b64,
     headers = list('X-GitHub-Api-Version'='2022-11-28')
  )
}



#push_results <- threadsheetr::threader_push_to_git("./data/parsed/", "./data/demo.yaml")
