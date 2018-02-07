#' Get Patient Info
#' @export
#' @param id ID of patient
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.get_patients_v1_{id}}
metrc_get_patient <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", "patients/v1", id = id)
}

#' Get Active Patients
#' @export
#' @param license_number Facility license number
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.get_patients_v1_active}
metrc_get_patients_active <- function(license_number) {
  metrc_call("GET", "patients/v1/active", license_number = license_number) %>%
    bind_rows()
}

#' Get Patient Status
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.get_patients_v1_status_{patientLicenseNumber}}
metrc_get_patient_status <- function(license_number, patient_license_number) {
  metrc_call("GET", "patients/v1/status", id = patient_license_number, license_number = license_number)
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
  metrc_call("POST", "patients/v1/add", license_number = license_number, body = data.frame(
    LicenseNumber = patient_license_number,
    LicenseEffectiveStartDate = license_effective_start_date,
    LicenseEffectiveEndDate = license_effective_end_date,
    RecommendedPlants = recommended_plants,
    RecommendedSmokableQuantity = recommended_smokable_quantity,
    ActualDate = actual_date
  ))
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
  metrc_call("POST", "patients/v1/update", license_number = license_number, body = data.frame(
    LicenseNumber = patient_license_number,
    NewLicenseNumber = new_license_number,
    LicenseEffectiveStartDate = license_effective_start_date,
    LicenseEffectiveEndDate = license_effective_end_date,
    RecommendedPlants = recommended_plants,
    RecommendedSmokableQuantity = recommended_smokable_quantity,
    ActualDate = actual_date
  ))
}

#' Delete Patient
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Patients.delete_patients_v1_{id}}
metrc_delete_patient <- function(license_number, id) {
  stopifnot(is.integer(id))
  metrc_call("DELETE", "patients/v1", id = id)
}

