#' Get Facilities Data
#' 
#' Gives information about all facilities user can access.
#' 
#' @return tibble Containing information about each facility. License is a list-col with info about the license.
#' 
#' @export
#' @note See url{https://api-co.metrc.com/Documentation/#Facilities.get_facilities_v1}

metrc_get_facilities <- function() {
  url <- modify_url(
    BASE_URL(), path = "facilities/v1"
  )
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>% {
    tibble(
      HireDate = map_chr(., "HireDate"),
      HomePage = map_chr(., "HomePage"),
      IsOwner = map_lgl(., "IsOwner"),
      IsManager = map_lgl(., "IsManager"),
      Name = map_chr(., "Name"),
      Alias = map_chr(., "Alias"),
      DisplayName = map_chr(., "DisplayName"),
      License = map(., "License") %>% map(as_tibble)
    )
  }
}