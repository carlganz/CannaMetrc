#' Get Package Info
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_{id}}
#' @param id ID of package

metrc_get_package <- function(id) {
  
  url <- modify_url(
    BASE_URL(), path = paste0("packages/v1/", id)
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
    map(shiny:::dropNullsOrEmpty) %>% as_tibble()
}

#' Get Active Packages
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_active}
#' @export
#' @param license_number Facility license number

metrc_get_packages_active <- function(license_number) {
  
  url <- modify_url(
    BASE_URL(), path = "packages/v1/active",
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
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
    map(shiny:::dropNullsOrEmpty) %>% bind_rows()
}

#' Get Onhold Packages
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_onhold}
#' @export
#' @param license_number Facility license number

metrc_get_packages_onhold <- function(license_number) {
  
  url <- modify_url(
    BASE_URL(), path = "packages/v1/onhold",
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
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
    map(shiny:::dropNullsOrEmpty) %>% bind_rows()
}

#' Get Inactive Packages
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_inactive}
#' @export
#' @param license_number Facility license number

metrc_get_packages_inactive <- function(license_number) {
  
  url <- modify_url(
    BASE_URL(), path = "packages/v1/inactive",
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
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
    map(shiny:::dropNullsOrEmpty) %>% bind_rows()
}

#' Get Package Types
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.get_packages_v1_types}
#' @export

metrc_get_packages_types <- function() {
  
  url <- modify_url(
    BASE_URL(), path = "packages/v1/types"
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
    unlist()
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
  url <- modify_url(
    BASE_URL(), path = "packages/v1/create",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), encode = "json",
               body = list(list(
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
  
  if (http_error(resp)) {
    print(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE))
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
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
  url <- modify_url(
    BASE_URL(), path = "packages/v1/create/testing",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), encode = "json",
               body = list(list(
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
  
  if (http_error(resp)) {
    print(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE))
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
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
  url <- modify_url(
    BASE_URL(), path = "packages/v1/create/plantings",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(),  encode = "json",
               body = list(
                 PackageLabel = package_label,
                 PackageAdjustmentAmount = package_adjustment_amount,
                 PackageAdjustmentAmountUnitOfMeasureName = package_adjustment_amount_unit_of_measure_name,
                 PlantBatchName = plant_batch_name,
                 PlantBatchType = plant_batch_type,
                 PlantCount = plant_count,
                 StrainName = strain_name,
                 PlantedDate = planted_date
               ))
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}


#' Post Change Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_change_item}

metrc_post_packages_change <- function(license_number,
                                             label, item) {
  url <- modify_url(
    BASE_URL(), path = "packages/v1/change/item",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(),  encode = "json",
               body = data.frame(
                 Label = label,
                 Item = item
               ))
  
  if (http_error(resp)) {
    print(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE))
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}

#' Post Adjust Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_adjust}
metrc_post_packages_adjust <- function(license_number, label, quantity, 
                                   unit_of_measure, adjustment_reason,
                                   adjustment_date, reason_note) {
  url <- modify_url(
    BASE_URL(), path = "packages/v1/adjust",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               encode = "json",
               body = data.frame(
                 Label = label,
                 Quantity = quantity, 
                 UnitOfMeasure = unit_of_measure, 
                 AdjustmentReason = adjustment_reason,
                 AdjustmentDate = adjustment_date, 
                 ReasonNote = reason_note
               ))
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}

#' Post Finish Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_finish}
metrc_post_packages_finish <- function(license_number, label, actual_date) {
  url <- modify_url(
    BASE_URL(), path = "packages/v1/finish",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               encode= "json",
               body = data.frame(
                 Label = label,
                 ActualDate = actual_date
               ))

  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}

#' Post Unfinish Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_unfinish}
metrc_post_packages_unfinish <- function(license_number, label) {
  url <- modify_url(
    BASE_URL(), path = "packages/v1/unfinish",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), encode="json",
               body = data.frame(
                 Label = label
               ))
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}

#' Post Remediate Package
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Packages.post_packages_v1_remediate}
metrc_post_packages_remediate <- function(license_number, package_label, remediation_method_name,
                                         remediation_date, remediation_steps) {
  url <- modify_url(
    BASE_URL(), path = "packages/v1/unfinish",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(),  encode = "json",
               body = data.frame(
                 PackageLabel = package_label, 
                 RemediationMethodName = remediation_method_name,
                 RemediationDate = remediation_date, 
                 RemediationSteps = remediation_steps
               ))
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}