#' @import httr jsonlite memoise dplyr tibble purrr
metrc_auth <- memoise::memoise(function(software_key = Sys.getenv("metrc_software_key"), 
                       user_key = Sys.getenv("metrc_user_key")) {
  httr::authenticate(software_key, user_key)
})

BASE_URL <- memoise::memoise(function(state = Sys.getenv("metrc_state"), demo = Sys.getenv("metrc_demo")) {
  state <- match.arg(state, c("CO", "OR", "MD", "AK", "MI", "CA", "OH"))
  
  sprintf(paste0("https://", if (as.logical(demo)) {"sandbox-"}, "api-%s.metrc.com/"), tolower(state))
  
})

