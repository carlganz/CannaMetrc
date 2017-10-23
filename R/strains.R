#' Get Strain
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.get_strains_v1_{id}}
metrc_get_strain <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL, path = paste0("strains/v1/",id))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Active Strains
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.get_strains_v1_active}
metrc_get_strains_active <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "strains/v1/active",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Strains
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.post_strains_v1_create}
metrc_post_strains <- function(license_number, name, testing_status, thc_level,
                               cbd_level, indica_percentage, sativa_percentage) {
  url <- modify_url(BASE_URL, path = "strains/v1/create",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(url, metrc_auth(), body = data.frame(
    Name = name, TestingStatus = testing_status, ThcLevel = thc_level,
    CbdLevel = cbd_level, IndicaPercentage = indica_percentage, 
    SativaPercentage = sativa_percentage
  ))
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Update Strains
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.post_strains_v1_update}
metrc_post_strains_update <- function(license_number, id, name, testing_status, thc_level,
                               cbd_level, indica_percentage, sativa_percentage) {
  url <- modify_url(BASE_URL, path = "strains/v1/update",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(url, metrc_auth(), body = data.frame(
    Id = id, Name = name, TestingStatus = testing_status, ThcLevel = thc_level,
    CbdLevel = cbd_level, IndicaPercentage = indica_percentage, 
    SativaPercentage = sativa_percentage
  ))
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Delete Strain
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Strains.delete_strains_v1_{id}}
metrc_delete_strain <- function(license_number, id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL, path = paste0("strains/v1/", id),
                    query = list(licenseNumber = license_number))
  
  resp <- DELETE(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}
