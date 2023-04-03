#' Push estimate grid to GitHub
#'
#' @param combined_path Path to the directory containing the combined (wide-format)
#'     grid datasets you want to push to git
#' @param spec_fpath Filepath to the `.yaml`-format spec file.
#' @param repo_name Optional: Defaults to `"ts-grids"`, provide a different value
#'     if you'd like to push to a differently-named repo.
#'
#' @return An environment with keys `num_result`, the return value from the GH API
#'     call pushing the numeric grid, and `info_result`, the return value from
#'     the GH API call pushing the info grid
#' @export
#'
#' @examples
#' # Not run, since it requires authentication (via `.env` file)
#' #gh_result <- threader_push_to_git()
threader_push_to_git <- function(combined_path = NULL, spec_fpath = NULL,
                                 repo_name = "ts-grids", verbose = FALSE) {
  if (is.null(combined_path)) {
    combined_path <- demo_combined_path()
  }
  if (is.null(spec_fpath)) {
    spec_fpath <- demo_spec_fpath()
  }
  # The idea here is: get a list of .csvs in the output_dir, then push the
  #   # contents of this folder onto GitHub
  #   # Load the personal access token
  dotenv::load_dot_env()
  # The `gh` library checks for the GITHUB_TOKEN environment variable, so make
  # sure that's set
  #gh_token <- Sys.getenv("GITHUB_TOKEN")
  gh_username <- Sys.getenv("GITHUB_USERNAME")
  gh_repo_name <- repo_name
  # Try to get the repo
  # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  repo_obj <- tryCatch(
    {
      (grid_repo <- gh::gh(paste0("GET /repos/",gh_username,"/",gh_repo_name)))
    },
    error=function(cond) {
      if (cond$response_content$message == "Not Found") {
        # Doesn't exist, will need to create it
        return(gh::gh("POST /user/repos", name = gh_repo_name))
      }
      stop(paste0("Unknown error: ", cond))
    }
  )
  # # Cool. So if we're here then the repo exists, and we can either add new
  # # files to it or update the files already in it
  # # The idea here is (1) put all the .csv files we want on Git into the
  # # public_grids folder, then (2) push them all into a Gist
  # # But, there may be outputs from other projects in that dir. So, make sure
  # # we only export the ones for this project (the options to the Threader
  # # constructor
  # Export the .csvs for the grid var
  spec <- .parse_spec_file(spec_fpath)
  # Get the grid var name so we can derive the .csv fpaths
  grid_varname <- spec$grid$varname
  num_fname <- paste0(grid_varname,"_est_wide.csv")
  num_fpath <- file.path(combined_path, num_fname)
  if (verbose) { print(paste0("Pushing numeric file ",num_fpath))}
  # And push
  num_repo_fpath <- file.path(num_fname)
  num_push_result <- .push_file(
    gh_username = gh_username,
    gh_repo_name = gh_repo_name,
    gh_repo_path = num_repo_fpath,
    commit_msg = paste0("New numeric grid for ",grid_varname),
    file_fpath = num_fpath
  )
  info_fname <- paste0(grid_varname,"_info_wide.csv")
  info_fpath <- file.path(combined_path, info_fname)
  if (verbose) { print(paste0("Pushing info file ",info_fpath))}
  # Load it as raw text
  info_repo_fpath <- file.path(info_fname)
  info_push_result <- .push_file(
    gh_username = gh_username,
    gh_repo_name = gh_repo_name,
    gh_repo_path = info_repo_fpath,
    commit_msg = paste0("New info grid for ",grid_varname),
    file_fpath = info_fpath
  )
  return_obj <- list(num_result=num_push_result,
                     info_result=info_push_result)
  return(return_obj)
  # # Print the download urls
  # for cur_file_result in file_results:
  #   print(cur_file_result['content'].download_url)
}

.push_file <- function(gh_username, gh_repo_name, gh_repo_path, commit_msg,
                          file_fpath, gh_name = "Threadsheets",
                          gh_email = "none@threadsheets.null") {
  content_bin <- readBin(file_fpath, raw(), file.info(file_fpath)$size)
  content_b64 <- base64enc::base64encode(content_bin)

  # Check if file exists
  existing_sha <- tryCatch(
    {
      gh_result <- gh::gh(
        'GET /repos/{owner}/{repo}/contents/{path}',
        owner = gh_username,
        repo = gh_repo_name,
        path = gh_repo_path
      )
      gh_result$sha
    },
    error=function(cond) {
      # Return NULL if file does not exist
      return(NULL)
    }
  )
  #old_sha <- exists_result$sha
  api_str <- paste0('PUT /repos/',gh_username,'/',gh_repo_name,'/contents/',gh_repo_path)
  if (is.null(existing_sha)) {
    push_result <- gh::gh(api_str,
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
    push_result <- gh::gh(api_str,
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
  return(push_result)
}

#
# import glob
# import os
#
# import dotenv
# import github
#
# def create_grid_repo(g):
#   result = g.get_user().create_repo("ts-grids", private=True)
# return result
#
# #dir(repo_obj)
# def custom_get_contents(repo):
#   # A wrapper around repo_obj.get_contents("") that returns [] if no files
#   # (instead of throwing an annoying exception)
#   try:
#   contents = repo.get_contents("")
# except github.GithubException as e:
#   return []
# return contents
#
# def custom_get_file(repo, filename):
#   # Another wrapper, returns None if file is not already in repo
#   repo_contents = custom_get_contents(repo)
# for cur_content in repo_contents:
#   if cur_content.path == filename:
#   return cur_content
# return None
#
# # Load the csv contents into strings, then into github.InputFileContent objects
# def load_csv_contents(fpaths):
#   # {filename: str} dict
#   str_dict = {}
# # {filename: InputFileContent} dict
# content_dict = {}
# for cur_fpath in fpaths:
#   filename = os.path.basename(cur_fpath)
# with open(cur_fpath, 'r', encoding='utf-8') as f:
#   csv_content = f.read().strip()
# str_dict[filename] = csv_content
# csv_content_obj = github.InputFileContent(csv_content)
# content_dict[filename] = csv_content_obj
# return content_dict, str_dict
#
# def run(pl, var_list=None):
#   # The idea here is: get a list of .csvs in the output_dir, then push the
#   # contents of this folder onto GitHub
#   # Load the personal access token
#   dotenv.load_dotenv(override=True)
# git_token = os.getenv("GH_TOKEN")
# g = github.Github('jpowerj', git_token)
# user = g.get_user()
# # See if the ts-grids repo already exists
# repo_exists = False
# repo_obj = None
# repos = list(user.get_repos())
# for cur_repo in repos:
#   if "ts-grids" in cur_repo.full_name:
#   repo_exists = True
# repo_obj = cur_repo
# break
# # Now we create the repo if it doesn't exist
# if not repo_exists:
#   repo_obj = create_grid_repo(g)
# # Cool. So if we're here then the repo exists, and we can either add new
# # files to it or update the files already in it
# print(repo_obj)
# # The idea here is (1) put all the .csv files we want on Git into the
# # public_grids folder, then (2) push them all into a Gist
# #grid_csv_fpath = "./public_grids/num_students_grid.csv"
# #info_csv_fpath = "./public_grids/num_students_info.csv"
# # But, there may be outputs from other projects in that dir. So, make sure
# # we only export the ones for this project (the options to the Threader
# # constructor
# if var_list is None:
#   # Just export all .csvs found
#   all_csv_fpaths = glob.glob(os.path.join(pl.output_path, "grid", "*.csv"))
# else:
#   # Export the .csvs for this var
#   all_csv_fpaths = []
# for cur_varname in var_list:
#   var_csv_fpaths = glob.glob(os.path.join(pl.output_path, "grid", f"{cur_varname}*.csv"))
# all_csv_fpaths.extend(var_csv_fpaths)
# input_dict, str_dict = load_csv_contents(all_csv_fpaths)
# file_results = []
# for cur_fname, content_str in str_dict.items():
#   var_name = "_".join(cur_fname.split(".")[0].split("_")[:-1])
# # Here's a tricky part, though -- gotta check if the file exists in the
# # repo already
# file_obj = custom_get_file(repo_obj, cur_fname)
# if file_obj is None:
#   # File doesn't already exist, so we create it
#   create_result = repo_obj.create_file(cur_fname, var_name, content_str)
# file_results.append(create_result)
# else:
#   # File already exists in repo, so we need to update it
#   old_sha = file_obj.sha
# update_result = repo_obj.update_file(cur_fname, var_name, content_str, old_sha)
# file_results.append(update_result)
# # Print the download urls
# for cur_file_result in file_results:
#   print(cur_file_result['content'].download_url)
#
# def api_example():
#   """
#     Example from the docs
#     """
# with open(".gitpw", "r", encoding='utf-8') as f:
#   git_pw = f.read().strip()
# g = Github("jpowerj", git_pw)
# # Then play with your Github objects:
# user = g.get_user()
# print()
# for repo in g.get_user().get_repos():
#   print(repo.name)
#
# def main():
#   push_to_git(None)
