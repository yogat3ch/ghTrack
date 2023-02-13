#' Get PRs
#'
#' @inheritParams commits_list
#' @param state \code{chr} Either `open`, `closed`, or `all` to filter by state.
#' Default: `open`
#' Can be one of: `open, closed, all`
#' @param head \code{chr} Filter pulls by head user or head organization and branch name in the format of `user:ref-name` or `organization:ref-name`. For example: `github:new-script-format` or `octocat:test-branch`.
#' @param base \code{chr} Filter pulls by base branch name. Example: `gh-pages`.
#' @param sort \code{chr} What to sort results by. `popularity` will sort by the number of comments. `long-running` will sort by date created and will limit the results to pull requests that have been open for more than a month and have had activity within the past month.
#'  Default: `created`
#'  Can be one of: `created, updated, popularity, long-running`
#' @param direction \code{chr} The direction of the sort. Default: `desc` when sort is created or sort is not specified, otherwise `asc`.
#' Can be one of: `asc`, `desc`
#'
#' @return \code{list} of pulls
#' @export
#' @seealso [Github API Documentation](https://docs.github.com/en/rest/pulls/pulls)

get_pulls <- function(
    owner,
    repo,
    state = "open",
    head = NULL,
    base = NULL,
    sort = "created",
    direction = "desc",
    per_page = 100,
    page = NULL
) {

  query_params <- list(
    state = state,
    head = head,
    base = base,
    sort = sort,
    direction = direction,
    per_page = per_page,
    page = page
  )
  pulls <-
    rlang::exec(gh::gh,
                endpoint = glue::glue("GET /repos/{owner}/{repo}/pulls"),
                !!!query_params
    ) |>
    {\(.x) {rlang::set_names(.x, pull_number(.x))}}()
  pulls <- get_all_pages(pulls)
  return(pulls)
}


pull_number <- function(pulls) {
  purrr::map_int(pulls, ~.x$number)
}
pull_author <- function(pulls) {
  purrr::map_chr(pulls, ~.x$user$login)
}

pull_keep_author <- function(pulls, author) {
  purrr::keep(pulls, ~.x$user$login == author)
}
