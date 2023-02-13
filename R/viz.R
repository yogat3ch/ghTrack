contributions_by_repo <- function(contributions, font_family = "Poppins") {

  data <- contributions_summary(contributions)


  out <- plotly::plot_ly(
    data,
    type = "bar",
    x = ~ repo,
    y = ~ total,
    color = ~ Language,
    text = ~paste0("additions: ", additions, " | ", "deletions:", deletions),
    legendgrouptitle = list(
      font = list(
        family = font_family,
        size = 14
      ),
      text = "Language"
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      margin = list(
        t = 50,
        b = 25,
        l = 25
      ),

      title = list(
        text = glue::glue("Total Contributions"),
        font  = list(
          family = font_family,
          size = 18
        )
      ),
      xaxis = list(
        tickfont  = list(
          family = font_family,
          size = 10
        ),
        title = list(
          text = "Total Contributions (additions & deletions)",
          font  = list(
            family = font_family,
            size = 12
          )
        )
      ),
      yaxis = list(
        tickfont  = list(
          family = font_family,
          size = 10
        ),
        title = list(
          text = "Repository",
          font  = list(
            family = font_family,
            size = 12
          )
        )
      )
    )

  return(out)
}

contributions_summary <- function(contributions) {
  dplyr::bind_rows(purrr::map(purrr::compact(contributions), "summary"), .id = "repo") |>
    dplyr::mutate(Language = dplyr::if_else(file_type == "Rmd", "R", toupper(file_type))) |>
    dplyr::group_by(repo, Language) |>
    dplyr::summarise(additions = sum(additions, na.rm = TRUE),
                     deletions = sum(deletions, na.rm = TRUE),
                     total = sum(additions, deletions, na.rm = TRUE))
}

contributions_total <- function(contributions_summary) {
  dplyr::group_by(contributions_summary, Language) |>
    dplyr::summarise(dplyr::across(-repo, sum, na.rm = TRUE)) |>
    dplyr::rename_with(\(.x) {stringr::str_to_title(.x)})
}

contributions_total_chr <- function(contributions_total) {
  purrr::pmap_vec(.ptype = character(), contributions_total, \(...) {
    .x <- list(...)
    names(.x)[1] <- "Language"
    glue::glue_collapse(purrr::imap_chr(.x, \(.x, .y) {paste0(.y, ":", .x)}), sep = " | ")
  })
}
