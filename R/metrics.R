get_all_contributor_commit_activity <- function(owner, repo) {
  resp <- gh::gh(glue::glue("GET /repos/{owner}/{repo}/stats/contributors"))
  resp <- get_all_pages(resp)
  return(resp)
}
#' Get all contributor commit activity
#'
#' @param activity \code{list} from `get_all_contributor_commit_activity`
#' @param contributors \code{chr} of contributors to include
#' @param collapse_to \code{chr} one of `week`, `authors`, `repos` where `week` collapses the week field to a data.frame. `authors` collapses the week field and authors field to a data.frame with `authors` as a column. `repos` collapses the week field, authors field, and repos to a data.frame with `authors` and `week` as columns
#'
#' @return \code{list} of all activity
#' @export
#'

commit_activity_df <- function(activity, contributors = NULL, collapse_to = "week") {
  out <- purrr::map_if(activity, ~!rlang::is_empty(.x), \(.x) {
    authors <- if (!is.null(contributors))
      purrr::keep(.x, \(.x) {.x$author$login %in% contributors})
    else
      .x

    authors <- rlang::set_names(authors, purrr::map_chr(authors, ~.x$author$login))

    authors <- purrr::map(authors, ~{
      `<-`(.x$weeks, dplyr::bind_rows(.x$weeks) |> dplyr::mutate(w = lubridate::as_datetime(w)))
      .x
    })
    if (collapse_to %in% c("authors", "repos"))
      authors <- purrr::list_rbind(purrr::map(authors, "weeks"), names_to = "authors")
    authors
  })
  if (collapse_to == "repos")
    out <- purrr::list_rbind(purrr::keep(out, is.data.frame), names_to = "repo")
  return(out)
}

viz_all_contributor_commit_activity <- function(activity, contributors = NULL, start = lubridate::today() - lubridate::years(1), end = lubridate::today()) {
  weeks <- list(from = lubridate::floor_date(start, "week"),
                to = lubridate::floor_date(end, "week"),
                by = "week")
  data <- tibble::tibble(w = lubridate::as_datetime(do.call(seq.Date, weeks)), a = NA_integer_, d = NA_integer_, c = NA_integer_)
  activity <- commit_activity_df(activity, contributors, collapse_to = "authors")



}
summarise_all_contributor_commit_activity <- function(activity, contributors = NULL, start = lubridate::now() - lubridate::years(1), end = lubridate::now()) {

  activity <- commit_activity_df(activity, contributors, collapse_to = "repos") |>
    dplyr::filter(dplyr::between(w, lubridate::floor_date(start, "week"),  lubridate::floor_date(end, "week"))) |>
    dplyr::group_by(authors) |>
      dplyr::summarise(a = sum(a, na.rm = TRUE),
                       d = sum(a, na.rm = TRUE),
                       c = sum(a, na.rm = TRUE))
}
