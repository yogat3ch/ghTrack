

#' Retrieve a summary of contributions for a repo
#'
#' @inheritParams commits_list
#' @param file_types \code{chr} file types to consider
#' @param exclude_main \code{lgl} Whether to filter out the main branch. Default `TRUE`
#' @param enable_resume \code{lgl} Whether to fetch API calls from the mock API repository if they've been saved in a previous run.
#' @param include_all_unique_commit_details \code{lgl} Whether to include the details about all unique commits contributing to the summary.
#' @param include_branch_details \code{lgl} Whether to include the commit list for each branch.
#' @param ... reserved for future arguments.
#' @param verbose \code{lgl} whether to print status messages
#' @import httptest
#' @return \code{list}
#' @export
#'

total_contributions_summary <-
  function(owner,
           repo,
           author = NULL,
           since = NULL,
           until = NULL,
           file_types = c("R", "Rmd", "js", "scss", "svelte"),
           exclude_main = TRUE,
           enable_resume = TRUE,
           include_all_unique_commit_details = FALSE,
           include_branch_details = FALSE,
           ...,
           verbose = FALSE) {



    library(httptest)
  if (verbose) {
    pb <- progress::progress_bar$new(format = "[:spin] :current :bar :percent :elapsedfull", total = 3)
  }

  if (enable_resume) {
    httptest::.mockPaths(dirs$inst("cassettes"))
  }

  if (verbose)
    pb$message("Retrieving branches")
  reqs <- list()
  reqs$all_branches <- rlang::expr(all_branches <- get_branches(owner = owner, repo = repo))

  if (enable_resume) {
    req_fail <- try(httptest::with_mock_api(reqs$all_branches))
  }
  if (!enable_resume || UU::is_error(req_fail)) {
    httptest::capture_requests(reqs$all_branches)
  }

  all_branches <- get_branches(owner = owner, repo = repo)


  # Filter main branch
  if (exclude_main)
    all_branches <- purrr::keep_at(all_branches, ~!.x %in% c("main", "master"))

  if (verbose) {
    pb$tick()
    pb$message("Retrieving all commits")
  }

  # Retrieving all commits on all branches
  reqs$all_commits <- rlang::expr({
    all_commits <-
      purrr::map(all_branches,
                 ~ commits_list(
                   sha = .x$commit$sha,
                   owner = owner,
                   repo = repo,
                   author = author,
                   since = since,
                   until = until
                 ))
  })

  if (enable_resume) {
    req_fail <- try(httptest::with_mock_api(reqs$all_commits))
  }
  if (!enable_resume || UU::is_error(req_fail)) {
    httptest::capture_requests(reqs$all_commits)
  }

  if (verbose) {
    pb$tick()
    pb$message("Munging commits to tbl")
  }
  # Remove branches with no activity
  all_commits <- purrr::keep(all_commits, UU::is_legit)
  # Remove commits with nothing
  all_commits <- purrr::map_depth(all_commits, 2, ~purrr::keep(.x, ~UU::is_legit(.x) &&
                                                                 # Some empty commits are just a 0 row tbl
                                                                 (nrow(.x) %||% 1) != 0))
  # Commits are repeated on each branch,
  # Collapse all commits down to single row
  all_commits_collapsed <- purrr::map_depth(all_commits, 2, \(.x) {
    # Remove parents because a commit can have multiple and this will interfere with adding to a row
    tibble::tibble_row(!!!unlist(.x[!names(.x) %in% "parents"]))
  }) |>
    purrr::map(dplyr::bind_rows)

  all_commits_collapsed <- dplyr::bind_rows(all_commits_collapsed, .id = "branch.name")
  if (!nrow(all_commits_collapsed)) {
    UU::gwarn("No commits matching criteria in {repo}. Returning NULL")
    return(NULL)
  }

  all_unique_commits <- dplyr::distinct(all_commits_collapsed, sha, .keep_all = TRUE)
  if (verbose) {
    pb$tick()
    pb$message("Fetching commit details")
  }

  # Do this in a non-destructive manner
  reqs$all_commit_details <- rlang::expr(all_commit_details <- purrr::pmap(all_unique_commits, \(...) {
    .x <- list(...)
    commit_get(owner, repo, .x$sha)}))
  if (enable_resume) {
    req_fail <- try(httptest::with_mock_api(reqs$all_commit_details))
  }
  if (!enable_resume || UU::is_error(req_fail)) {
    httptest::capture_requests(reqs$all_commit_details)
  }


  # Collapse all commit details
  all_commit_details_collapsed <- commit_detail_file_df(all_commit_details)
  names(all_commit_details_collapsed) <- purrr::map_chr(all_commit_details_collapsed, "sha")

  # Turn into a df of just the files
  out <- purrr::list_rbind(purrr::map(all_commit_details_collapsed, "files"), names_to = "commit_sha")
  out <- commit_file_type_filter(out, file_types) |>
    commits_list_summarise_files()
  if (verbose)
    pb$terminate()

  out <- purrr::compact(list(summary = out,
              if (include_all_unique_commit_details)
                all_unique_commit_details = all_unique_commit_details_collapsed,
              if (include_branch_details)
                all_commits_collapsed
              ))
  return(out)
}


lines_per_week <- function(contributions) {

  contributions$contribution_summary |>
    dplyr::summarize(adds = sum(additions),
                     dels = sum(deletions),
                     count = dplyr::n()) |>
    tail(1)
}

