#' Get Individual Harvest Data
#' @export
#' @param id Plant ID
#' @note See url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_{id}}

metrc_get_harvest <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(
    BASE_URL(), path = paste0("harvests/v1/", id)
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

#' Get Active Harvests of Facility
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_active}
#' @param license_number Facility license number

metrc_get_harvests_active <- function(license_number) {
  
  url <- modify_url(
    BASE_URL(), path = "harvests/v1/active",
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

#' Get On-Hold Harvests of Facility
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_onhold}
#' @param license_number Facility license number

metrc_get_harvests_onhold <- function(license_number) {
  
  url <- modify_url(
    BASE_URL(), path = "harvests/v1/onhold",
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

#' Get Inactive Harvests of Facility
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.get_harvests_v1_inactive}
#' @param license_number Facility license number

metrc_get_harvests_inactive <- function(license_number) {
  
  url <- modify_url(
    BASE_URL(), path = "harvests/v1/inactive",
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

#' Post New Harvest
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
  
  url <- modify_url(
    BASE_URL(), path = "harvests/v1/createpackages",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               body = data.frame(
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
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Harvest Waste
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.post_harvests_v1_removewaste}
#' @param license_number Facility license number

metrc_post_harvests_removewaste <- function(license_number, 
                               id,
                               unit_of_weight,
                               waste_weight,
                               actual_date) {
  
  url <- modify_url(
    BASE_URL(), path = "harvests/v1/removewaste",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               body = data.frame(
                 Id = id,
                 UnitOfWeight = unit_of_weight,
                 WasteWeight = waste_weight,
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

#' Post Finish Harvest
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.post_harvests_v1_finish}
#' @param license_number Facility license number

metrc_post_harvests_finish <- function(license_number, 
                                     id,
                                     actual_date) {
  
  url <- modify_url(
    BASE_URL(), path = "harvests/v1/finish",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               body = data.frame(
                 Id = id,
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

#' Post Unfinish Harvest
#' @note See \url{https://api-co.metrc.com/Documentation/#Harvests.post_harvests_v1_unfinish}
#' @param license_number Facility license number

metrc_post_harvests_unfinish <- function(license_number, id) {
  
  url <- modify_url(
    BASE_URL(), path = "harvests/v1/unfinish",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               body = list(
                 Id = id
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


