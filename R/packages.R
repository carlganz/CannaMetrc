#' Get Package Info
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_{id}}
#' @param id ID of package

metrc_get_package <- function(id) {
  metrc_call("GET", "packages/v1", id = id) %>%
    map(dropNullsOrEmpty) %>% as_tibble()
}

#' Get Active Packages
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_active}
#' @export
#' @param license_number Facility license number

metrc_get_packages_active <- function(license_number) {
  metrc_call("GET", "packages/v1/active", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Onhold Packages
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_onhold}
#' @export
#' @param license_number Facility license number

metrc_get_packages_onhold <- function(license_number) {
  metrc_call("GET", "packages/v1/onhold", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Inactive Packages
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_inactive}
#' @export
#' @param license_number Facility license number

metrc_get_packages_inactive <- function(license_number) {
  metrc_call("GET", "packages/v1/inactive", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Package Types
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_types}
#' @export

metrc_get_packages_types <- function() {
  metrc_call("GET", "packages/v1/types") %>%
    unlist()
}

#' Get Package Adjustment Reasons
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_adjust_reasons}
metrc_get_adjust_reasons <- function() {
  metrc_call("GET", "packages/v1/adjust/reasons") %>%
    unlist() %>% as.vector()
}

#' Post New Package
#' @export
#' @param ingredients list-col 
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_create}

metrc_post_packages_create <- function(license_number,
                                   tag,
                                   item,
                                   quantity,
                                   unit_of_measure,
                                   is_production_batch,
                                   production_batch_number,
                                   product_requires_remediation,
                                   actual_date,
                                   ingredients) {
  metrc_call("POST", "packages/v1/create", license_number = license_number, body = list(list(
    Tag = tag,
    Item = item,
    Quantity = quantity,
    UnitOfMeasure = unit_of_measure,
    IsProductionBatch = is_production_batch,
    ProductionBatchNumber = production_batch_number,
    ProductRequiresRemediation = product_requires_remediation,
    ActualDate = actual_date,
    Ingredients = list(ingredients)
  )))
}


#' Post New Package Testing
#' @export
#' @param ingredients list-col 
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_create_testing}

metrc_post_packages_testing <- function(license_number,
                                   tag,
                                   item,
                                   quantity,
                                   unit_of_measure,
                                   is_production_batch,
                                   production_batch_number,
                                   product_requires_remediation,
                                   actual_date,
                                   ingredients) {
  metrc_call("POST", "packages/v1/create/testing", license_number = license_number, body = list(list(
    Tag = tag,
    Item = item,
    Quantity = quantity,
    UnitOfMeasure = unit_of_measure,
    IsProductionBatch = is_production_batch,
    ProductionBatchNumber = production_batch_number,
    ProductRequiresRemediation = product_requires_remediation,
    ActualDate = actual_date,
    Ingredients = list(ingredients)
  )))
  
}

#' Post New Package Plantings
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_create_plantings}

metrc_post_packages_plantings <- function(license_number,
                                   package_label,
                                   package_adjustment_amount,
                                   package_adjustment_amount_unit_of_measure_name,
                                   plant_batch_name,
                                   plant_batch_type,
                                   plant_count,
                                   strain_name,
                                   planted_date) {
  metrc_call("POST", "packages/v1/create/plantings", license_number = license_number, body = list(
    PackageLabel = package_label,
    PackageAdjustmentAmount = package_adjustment_amount,
    PackageAdjustmentAmountUnitOfMeasureName = package_adjustment_amount_unit_of_measure_name,
    PlantBatchName = plant_batch_name,
    PlantBatchType = plant_batch_type,
    PlantCount = plant_count,
    StrainName = strain_name,
    PlantedDate = planted_date
  ))
}


#' Post Change Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_change_item}

metrc_post_packages_change <- function(license_number,
                                             label, item) {
  metrc_call("POST", "packages/v1/change/item", license_number = license_number, body = data.frame(
    Label = label,
    Item = item
  ))
}

#' Post Adjust Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_adjust}
metrc_post_packages_adjust <- function(license_number, label, quantity, 
                                   unit_of_measure, adjustment_reason,
                                   adjustment_date, reason_note) {
  metrc_call("POST", "packages/v1/adjust", license_number = license_number, body = data.frame(
    Label = label,
    Quantity = quantity, 
    UnitOfMeasure = unit_of_measure, 
    AdjustmentReason = adjustment_reason,
    AdjustmentDate = adjustment_date, 
    ReasonNote = reason_note
  ))
  
}

#' Post Finish Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_finish}
metrc_post_packages_finish <- function(license_number, label, actual_date) {
  metrc_call("POST", "packages/v1/finish", license_number = license_number, body = data.frame(
    Label = label,
    ActualDate = actual_date
  ))
  
}

#' Post Unfinish Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_unfinish}
metrc_post_packages_unfinish <- function(license_number, label) {
  metrc_call("POST", "packages/v1/unfinish", license_number = license_number, body = data.frame(Label = label))
  
}

#' Post Remediate Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_remediate}
metrc_post_packages_remediate <- function(license_number, package_label, remediation_method_name,
                                         remediation_date, remediation_steps) {
  metrc_call("POST", "packages/v1/remediate", license_number = license_number, body = data.frame(
    PackageLabel = package_label, 
    RemediationMethodName = remediation_method_name,
    RemediationDate = remediation_date, 
    RemediationSteps = remediation_steps
  ))
}