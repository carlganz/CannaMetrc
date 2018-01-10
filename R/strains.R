#' Get Strain
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.get_strains_v1_{id}}
metrc_get_strain <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", "strains/v1", id = id) %>%
    dropNullsOrEmpty() %>% as_tibble()
}

#' Get Active Strains
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.get_strains_v1_active}
metrc_get_strains_active <- function(license_number) {
  metrc_call("GET", "strains/v1/active", license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Post Strains
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.post_strains_v1_create}
metrc_post_strains <- function(license_number, name, testing_status, thc_level,
                               cbd_level, indica_percentage, sativa_percentage) {
  metrc_call("POST", "strains/v1/create", license_number = license_number, body = data.frame(
    Name = name, TestingStatus = testing_status, ThcLevel = thc_level,
    CbdLevel = cbd_level, IndicaPercentage = indica_percentage, 
    SativaPercentage = sativa_percentage
  ))
}

#' Post Update Strains
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.post_strains_v1_update}
metrc_post_strains_update <- function(license_number, id, name, testing_status, thc_level,
                               cbd_level, indica_percentage, sativa_percentage) {
  metrc_call("POST", "strains/v1/update", license_number = license_number, body = data.frame(
    Id = id, Name = name, TestingStatus = testing_status, ThcLevel = thc_level,
    CbdLevel = cbd_level, IndicaPercentage = indica_percentage, 
    SativaPercentage = sativa_percentage
  ))
}

#' Delete Strain
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.delete_strains_v1_{id}}
metrc_delete_strain <- function(license_number, id) {
  stopifnot(is.integer(id))
  metrc_call("DELETE", "strains/v1", id = id, license_number = license_number)
  
}
