#' Get Individual Harvest Data
#' @export
#' @param id Plant ID
#' @note See url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_{id}}

metrc_get_harvest <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", "harvests/v1", id = id) %>% 
    map(dropNulls) %>% bind_rows()
  
}

#' Get Active Harvests of Facility
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_active}
#' @param license_number Facility license number

metrc_get_harvests_active <- function(license_number) {
  metrc_call("GET", "harvests/v1/active", license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
  
}

#' Get On-Hold Harvests of Facility
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_onhold}
#' @param license_number Facility license number

metrc_get_harvests_onhold <- function(license_number) {
  metrc_call("GET", "harvests/v1/onhold", license_number = license_number) %>% 
    map(dropNulls) %>% bind_rows()
 
}

#' Get Inactive Harvests of Facility
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_inactive}
#' @param license_number Facility license number

metrc_get_harvests_inactive <- function(license_number) {
  metrc_call("GET", "harvests/v1/inactive", license_number = license_number) %>% 
    map(dropNulls) %>% bind_rows()

}

#' Post New Harvest
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.post_harvests_v1_createpackages}
#' @param license_number Facility license number

metrc_post_harvests_createpackage <- function(license_number, 
                               harvest,
                               item,
                               weight,
                               unit_of_weight,
                               tag,
                               is_production_batch,
                               production_batch_number,
                               product_requires_remediation,
                               remediate_product,
                               remediation_method_id,
                               remediation_date,
                               remediation_steps,
                               actual_date) {
  
  metrc_call("POST", "harvests/v1/createpackages", license_number = license_number, body = data.frame(
    Harvest = harvest,
    Item = item,
    Weight = weight,
    UnitOfWeight = unit_of_weight,
    Tag =tag,
    IsProductionBatch = is_production_batch,
    ProductionBatchNumber = production_batch_number,
    ProductRequiresRemediation = product_requires_remediation,
    RemediateProduct = remediate_product,
    RemediationMethodId = remediation_method_id,
    RemediationDate = remediation_date,
    RemediationSteps = remediation_steps,
    ActualDate = actual_date
  ))
}

#' Post Harvest Waste
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.post_harvests_v1_removewaste}
#' @param license_number Facility license number

metrc_post_harvests_removewaste <- function(license_number, 
                               id,
                               unit_of_weight,
                               waste_weight,
                               actual_date) {
  
  metrc_call("POST", "harvests/v1/removewaste", license_number= license_number, body = data.frame(
    Id = id,
    UnitOfWeight = unit_of_weight,
    WasteWeight = waste_weight,
    ActualDate = actual_date
  ))
  
}

#' Post Finish Harvest
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.post_harvests_v1_finish}
#' @param license_number Facility license number

metrc_post_harvests_finish <- function(license_number, 
                                     id,
                                     actual_date) {
  metrc_call("POST", "harvests/v1/finish", license_number = license_number, body = data.frame(
    Id = id,
    ActualDate = actual_date
  ))
 
}

#' Post Unfinish Harvest
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.post_harvests_v1_unfinish}
#' @param license_number Facility license number

metrc_post_harvests_unfinish <- function(license_number, id) {
  metrc_call("POST", "harvests/v1/unfinish", license_number = license_number, body = data.frame(Id = id))
  
}


