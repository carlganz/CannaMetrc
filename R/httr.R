#' @import httr jsonlite
metrc_auth <- function(software_key = Sys.getenv("metrc_software_key"), 
                       user_key = Sys.getenv("metrc_user_key")) {
  httr::authenticate(software_key, user_key)
}

BASE_URL <- ""

