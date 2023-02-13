#' List Organization repositories
#'
#' @inheritParams get_pulls
#' @param type \code{chr} Specifies the types of repositories you want returned. If your organization is associated with an enterprise account using GitHub Enterprise Cloud or GitHub Enterprise Server 2.20+, type can also be internal. However, the internal value is not yet supported when a GitHub App calls this API with an installation access token.
#' Can be one of: `all`, `public`, `private`, `forks`, `sources`, `member`, `internal`
#'
#' @return \code{list} of repositories
#' @export
#'

get_list_all_organization_repositories <-
  function(org,
           type = NULL,
           sort = NULL,
           direction = NULL,
           per_page = 100,
           page = NULL) {
    resp <- get_all_pages(gh::gh(glue::glue("GET /orgs/{org}/repos"),
                                 type = type,
                                 sort = sort,
                                 direction = direction,
                                 per_page = per_page,
                                 page = page))
}

active_repos <- rlang::set_names(c(
  "dmdu",
  "RiverViz",
  "walton_status_tracker",
  "shinyVirga",
  "virgaUtils",
  "SUPERCAT",
  "crssDB",
  "BOR",
  "crcDatabase",
  "grand_canyon_viz",
  "crosstalk",
  "edf-storymap-hub",
  "crss_database",
  "crss_kedro",
  "storymap_viz_temp",
  "storymap_viz_fire_map",
  "storymap_viz_precip",
  "storymap_viz_swe",
  "storymap_viz_pinyon",
  "storymap_viz_powell_inflow",
  "virgaVault",
  "powell_inflow_map",
  "vlcrss",
  "edf-storymap",
  "bs4Dash",
  "crsspr_viz",
  "bias_crmms",
  "asu_hydrologies",
  "crssETL",
  "shiny_html_template_example",
  "ten_strat_funding_app",
  "test_golem",
  "rrg_econ_exposure_app"
)
)
