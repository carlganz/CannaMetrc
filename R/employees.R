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
  
  metrc_call("GET", "employees/v1", licenseNumber = license_number)
  
}