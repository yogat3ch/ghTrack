has_more_pages <- function(gh_response) {
  gh:::gh_has_next(gh_response)
}

get_all_pages <- function(gh_response) {
  get_more_pages <- has_more_pages(gh_response)
  if (get_more_pages) {
    pages <- list(
      gh_response
    )
    i <- 2
    while (get_more_pages) {
      gh_response <- gh::gh_next(gh_response)
      rl <- gh::gh_rate_limit(gh_response)
      if (rl$remaining == 0) {
        UU::gbort("Rate limit reached. Resume in one hour.")
      }
      pages[[i]] <- gh_response
      i <- i + 1
      get_more_pages <- has_more_pages(gh_response)
    }
    out <- purrr::list_flatten(pages)
  } else
    out <- gh_response
  return(out)
}

