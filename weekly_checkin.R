



owner <- "Martin-McCoy"
author <- "yogat3ch"
# Add list of repos for which you are a contributor
# Add repo names below
repos <- c("dmdu")  |>  rlang::set_names()
debug(total_contributions)
all_contributions <- purrr::map(repos, total_contributions, owner = owner, author = "keatonwilson") |>
  purrr::compact()
saveRDS(all_contributions, file = glue::glue("{author}_contributions_{Sys.Date()}.rds"))
total <- purrr::map_dfr(all_contributions, lines_per_week, .id = "repo") |>
  dplyr::filter(dplyr::across(dplyr::everything(), ~.x != 0))

write(as.character(item_list), "~/Jobs/COHHIO/Weekly_checkin.html")
write(as.character(knitr::kable(total, "html")), "~/Jobs/COHHIO/Weekly_checkin.html", append = TRUE)
