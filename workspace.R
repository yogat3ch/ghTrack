org <- "Martin-McCoy"
authors <- c("yogat3ch", "keatonwilson", "lydiableifuss", "ncoston-virga", "maria-VL", "wyliehampson")
dmdu_pulls <- get_pulls(org, "dmdu", state= "all")
my_pulls <- pull_keep_author(dmdu_pulls, "yogat3ch")
repo_activity <- purrr::map(active_repos, ~{
  get_all_contributor_commit_activity(org, .x)
})
addtl_activity <- purrr::keep(repo_activity, rlang::is_empty) |>
  names() |>
  rlang::set_names() |>
  purrr::map(~get_all_contributor_commit_activity(org, .x))

purrr::iwalk(addtl_activity, ~{
  repo_activity[[.y]] <<- .x
})

viz <- viz_all_contributor_commit_activity(repo_activity, contributors)
contributions <- list(
  dmdu = NULL
)
contributions$dmdu <- total_contributions_summary(org, "dmdu", "yogat3ch")
contributions <- readRDS("data/contributions.rds")
repos <- tibble::tribble(
  ~ repo,
  ~ exclude_main,
  # "RiverViz",
  # TRUE,
  # "shinyVirga",
  # FALSE,
  # "virgaUtils",
  # FALSE,
  # "crssDB",
  # FALSE,
  # "BOR",
  # FALSE,
  # "crcDatabase",
  # TRUE,
  # "virgaVault",
  # FALSE,
  # "vlcrss",
  # FALSE,
  "bs4Dash",
  TRUE,
  "crsspr_viz",
  TRUE,
  "crssETL",
  FALSE
)

purrr::pwalk(repos, \(...) {
  .x <- list(...)
  contributions[[.x$repo]] <<- total_contributions_summary(org, .x$repo, "yogat3ch", exclude_main = .x$exclude_main, since = "2022-01-01")
})


contributions_by_repo(contributions)
