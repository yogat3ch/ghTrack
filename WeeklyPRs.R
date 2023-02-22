org <- "Martin-McCoy"
dmdu_pulls <- get_pulls(org, "dmdu", state= "all") |>
  # Coerce string dates to datetimes
  purrr::map(\(.x) {
    purrr::modify_at(.x, \(.x) {
      stringr::str_detect(.x, "_at$")
    }, lubridate::as_datetime)
  })

weeks_pulls <- purrr::keep(dmdu_pulls, \(.x) {
  .x$created_at > lubridate::as_date(lubridate::today() - lubridate::weeks(1)) |>
    lubridate::floor_date("week")
})

purrr::map_chr(weeks_pulls, \(.x) {glue::glue(" - #{.x$number} @{.x$user$login}")}) |>
  glue::glue_collapse("\n")
