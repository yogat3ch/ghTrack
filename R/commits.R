
#' Get Commit List
#' @description See \href{List Commits}{https://docs.github.com/en/rest/commits/commits#list-commits}
#' @param owner \code{chr} The account owner of the repository. The name is not case sensitive.
#' @param repo \code{chr} The name of the repository. The name is not case sensitive.
#' @param author \code{chr} GitHub login or email address by which to filter by commit author.
#' @param sha \code{chr} SHA or branch to start listing commits from. Default: the repository’s default branch (usually master).
#' @param path \code{chr} Only commits containing this file path will be returned.
#' @param since \code{chr/POSIXct} Only show notifications updated after the given time. This is a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param until \code{chr/POXIXct} Only commits before this date will be returned. This is a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param per_page \code{num} The number of results per page (max 100).
#' @param page \code{num} Page number of the results to fetch.
#'
#' @return \code{list}
#' @export

commits_list <- function(owner, repo, author = NULL, sha = NULL, path = NULL, since = NULL, until = NULL, per_page = 100, page = 1) {


  .dates <- purrr::compact(list(since = since, until = until))
  if (UU::is_legit(.dates)) {
    # Coerce dates to ISO8601 for the API
    e <- environment()
    purrr::iwalk(.dates, ~assign(.y, envir = e, value = lubridate::format_ISO8601(lubridate::as_datetime(.x), usetz = TRUE, precision = "ymdhm")))
  }


  query_params <- list(
    author = author,
    sha = sha,
    path = path,
    since = since,
    until = until,
    per_page = per_page,
    page = page
  )
  .commits <-
    rlang::exec(gh::gh,
                endpoint = glue::glue("GET /repos/{owner}/{repo}/commits"),
                !!!query_params
    )

  commits <- get_all_pages(.commits)
  names(commits) <- purrr::map_chr(commits, ~substr(.x$sha, 0, 6))

  if (length(commits) > 0) {
    # Remove merges, as the person merging did not write all the code a merge entails
    out <- purrr::keep(commits, ~stringr::str_detect(.x$commit$message, "^Merge", negate = TRUE))
  } else {
    out <- NULL
  }
  return(out)
}


#' Title
#'
#' @inheritParams commits_list
#' @param ref \code{chr} Sha of the commit
#'
#' @return \code{list} of commit details
#' @export

commit_get <- function(owner, repo, ref, page = 1, per_page = 100) {
  gh::gh(glue::glue("GET /repos/{owner}/{repo}/commits/{ref}"),
         page = page,
         per_page = per_page)
}


commits_non_merged <- function(commits) {
  purrr::keep(commits, ~stringr::str_detect(.x$commit$message, "^[Mm]erge", negate = TRUE))
}

commit_detail_file_df <- function(branches) {
  purrr::map_if(branches, UU::is_legit, \(.x) {
    .x$files <- dplyr::bind_rows(.x$files) |> dplyr::mutate(author = .x$author$login %||% NA_character_)
    .x
  })
}

#' @export
commits_list_summarise_files <- function(branches) {
  UseMethod("commits_list_summarise_files")
}
#' @export
commits_list_summarise_files.list <- function(branches) {
  purrr::map_if(branches, UU::is_legit, \(.x) {
    if (is.data.frame(.x$files) && nrow(.x$files))
      .x$files <-  dplyr::group_by(.x$files, file_type, author) |>
        dplyr::summarise(
          sha = paste0(sha, collapse = ", "),
          filenames = paste0(filename, collapse = ", "),
          additions = sum(additions, na.rm = TRUE),
          deletions = sum(deletions, na.rm = TRUE)
        )
    .x
  })
}
#' @export
commits_list_summarise_files.tbl <- function(branches) {
  dplyr::group_by(branches, file_type, author) |>
    dplyr::summarise(
      sha = paste0(sha, collapse = ", "),
      filenames = paste0(filename, collapse = ", "),
      additions = sum(additions, na.rm = TRUE),
      deletions = sum(deletions, na.rm = TRUE)
    )
}

commit_file_type_filter <- function(branches, file_types) {
  if (is.data.frame(branches)) {
    dplyr::mutate(branches,
                  file_type = UU::ext(filename)) |>
      dplyr::filter(file_type %in% file_types)
  } else {
    purrr::map_if(branches, UU::is_legit, \(.x) {
      if (is.data.frame(.x$files) && nrow(.x$files))
        .x$files <- dplyr::mutate(.x$files,
                                  file_type = UU::ext(filename)) |>
          dplyr::filter(file_type %in% file_types)
      .x
    })
  }

}
