#' Get Patient Info
#' @export
#' @param id ID of patient
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.get_patients_v1_{id}}
metrc_get_patient <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(
    BASE_URL, path = paste0("patients/v1/", id)
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

#' Get Active Patients
#' @export
#' @param license_number Facility license number
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.get_patients_v1_active}
metrc_get_patients_active <- function(license_number) {
  
  url <- modify_url(
    BASE_URL, path = "patients/v1/active",
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

#' Get Patient Status
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.get_patients_v1_status_{patientLicenseNumber}}
metrc_get_patient_status <- function(license_number, patient_license_number) {
  url <- modify_url(
    BASE_URL, path = paste0("patients/v1/status/", patient_license_number),
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

#' Post New Patient
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.post_patients_v1_add}
metrc_post_patients <- function(license_number, patient_license_number,
                                   license_effective_start_date,
                                   license_effective_end_date,
                                   recommended_plants,
                                   recommended_smokable_quantity,
                                   actual_date) {
  url <- modify_url(
    BASE_URL, path = "patients/v1/add",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), body = data.frame(
    LicenseNumber = license_number,
    LicenseEffectiveStartDate = license_effective_start_date,
    LicenseEffectiveEndDate = license_effective_end_date,
    RecommendedPlants = recommended_plants,
    RecommendedSmokableQuantity = recommended_smokable_quantity,
    ActualDate = actual_date
  ))
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Update Patient
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.post_patients_v1_update}
metrc_post_patients_update <- function(license_number, patient_license_number,
                                   new_license_number,
                                   license_effective_start_date,
                                   license_effective_end_date,
                                   recommended_plants,
                                   recommended_smokable_quantity,
                                   actual_date) {
  url <- modify_url(
    BASE_URL, path = "patients/v1/update",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), body = data.frame(
    LicenseNumber = license_number,
    NewLicenseNumber = new_license_number,
    LicenseEffectiveStartDate = license_effective_start_date,
    LicenseEffectiveEndDate = license_effective_end_date,
    RecommendedPlants = recommended_plants,
    RecommendedSmokableQuantity = recommended_smokable_quantity,
    ActualDate = actual_date
  ))
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Delete Patient
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.delete_patients_v1_{id}}
metrc_post_patient_delete <- function(license_number, id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(
    BASE_URL, path = paste0("patients/v1/", id),
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- DELETE(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

