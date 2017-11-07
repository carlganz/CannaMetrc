#' Get Employee Data
#' 
#' This provides information about all employees at a specified facility.
#' 
#' @return data.frame Containing information about the employess of a facility
#' 
#' @param license_number Employee license number
#' @note See \url{https://api-co.metrc.com/Documentation/#Employees.get_employees_v1}
#' @export
metrc_get_employees <- function(license_number) {
  url <- modify_url(
    BASE_URL, path = "employees/v1",
    query = list(
      licenseNumber = license_number
    )
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