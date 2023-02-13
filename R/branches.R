
#' Get branch info
#'
#' @inheritParams commits_list
#' @param protected \code{lgl} Setting to `true` returns only protected branches. When set to `false`, only unprotected branches are returned. Omitting this parameter returns all branches.
#' @param branch \code{chr} Branch SHA, for querying info about a single branch
#' @param branches \code{list} of Branches, to concatenate in a paginated query. Mostly of internal use in other functions.
#' @seealso [Github API Documentation](https://docs.github.com/en/rest/branches/branches)
#' @return \code{list} of Branch info (does not include file info)
#' @export
get_branches <-
  function(owner,
           repo,
           protected = NULL,
           per_page = 100,
           page = 1,
           branch = NULL,
           # Used for recursive calls
           branches = NULL) {

    query_params <- list(
      protected = protected,
      per_page = per_page,
      page = page
    )
    single_branch <- !is.null(branch)
    ep <- paste("GET", ifelse(single_branch, "/repos/{owner}/{repo}/branches/{branch}", "/repos/{owner}/{repo}/branches"))
    .branches <-
      rlang::exec(gh::gh,
                  endpoint = glue::glue(ep),
                  !!!query_params
      )
    if (!single_branch) {
      branches <- get_all_pages(.branches)
      names(branches) <- purrr::map_chr(branches, "name")
    } else {
      branches <- .branches
    }

    return(branches)
  }




branch_authors <- function(branches) {
  purrr::imap_chr(branches, ~.x$author$login)
}

branch_keep_author <- function(branches, author) {
  purrr::keep(branches, ~(.x$author$login == author) %|0|% FALSE)
}

