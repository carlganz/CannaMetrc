#' Get Facilities Data
#' @export
#' @note See url{https://api-co.metrc.com/Documentation/#Facilities.get_facilities_v1}

metrc_get_facilites <- function() {
  url <- modify_url(
    BASE_URL, path = "facilities/v1"
  )
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}