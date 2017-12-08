#' Get Units of Measurement
#' @note See \url{https://api-co.metrc.com/Documentation/#UnitsOfMeasure.get_unitsofmeasure_v1_active}
#' @export

metrc_get_units <- function() {
  url <- modify_url(
    BASE_URL(), path = "unitsofmeasure/v1/active"
  )
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>% 
    bind_rows()
  
}