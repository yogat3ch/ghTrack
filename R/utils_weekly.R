#' Generate a Markdown list of all PRs and their requestors since Date
#'
#' @inheritParams get_pulls
#' @param since \code{Date} Return all PRs since this Date
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' weekly_prs()
weekly_prs <-
  function(owner = "Martin-McCoy",
           repo = "dmdu",
           state = "all",
           since = lubridate::as_date(lubridate::today() - lubridate::weeks(1)) |>
             lubridate::floor_date("week"),
           regex_remove = "[dD]ev to [mM]ain"
  ) {
    dmdu_pulls <- get_pulls(owner, repo, state = state) |>
      # Coerce string dates to datetimes
      purrr::map(\(.x) {
        purrr::modify_at(.x, \(.x) {
          stringr::str_detect(.x, "_at$")
        }, lubridate::as_datetime)
      })

    weeks_pulls <- purrr::keep(dmdu_pulls, \(.x) {
      (.x$created_at > since) &&
        stringr::str_detect(.x$title, UU::regex_or(regex_remove), negate = TRUE)
    })

    purrr::map_chr(weeks_pulls, \(.x) {
      glue::glue(" - #{.x$number} @{.x$user$login}")
    }) |>
      glue::glue_collapse("\n")

  }
