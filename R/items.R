#' Get Item Data
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.get_items_v1_{id}}
#' @param id Item ID

metrc_get_item <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(
    BASE_URL, path = paste0("items/v1/", id)
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

#' Get Active Items
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.get_items_v1_active}
#' @param license_number Facility license number

metrc_get_items_active <- function(license_number) {
  
  url <- modify_url(
    BASE_URL, path = "items/v1/active",
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

#' Get Items Categories
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.get_items_v1_categories}

metrc_get_items_categories <- function() {
  
  url <- modify_url(
    BASE_URL, path = "items/v1/categories"
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

#' Post New Items
#' @export
#' @param license_number Facility license number
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.post_items_v1_create}

metrc_post_items_create <- function(license_number,
                                 item_category,
                                 name,
                                 unit_of_measurement,
                                 strain,
                                 unit_thc_content_unit_of_measure,
                                 unit_weight,
                                 unit_weight_unit_of_measure) {
  url <- modify_url(
    BASE_URL, path = "items/v1/create",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               body = data.frame(
                 ItemCategory = item_category,
                 Name = name,
                 UnitOfMeasurement = unit_of_measurement,
                 Strain = strain,
                 UnitThcContentUnitOfMeasure = unit_thc_content_unit_of_measure,
                 UnitWeight = unit_weight,
                 UnitWeightUnitOfMeasure = unit_weight_unit_of_measure
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

#' Post Update Items
#' @export
#' @param license_number Facility license number
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.post_items_v1_update}

metrc_post_items_update <- function(license_number,
                                    id,
                                 item_category,
                                 name,
                                 unit_of_measurement,
                                 strain,
                                 unit_thc_content_unit_of_measure,
                                 unit_weight,
                                 unit_weight_unit_of_measure) {
  url <- modify_url(
    BASE_URL, path = "items/v1/update",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), 
               body = data.frame(
                 Id = id,
                 ItemCategory = item_category,
                 Name = name,
                 UnitOfMeasurement = unit_of_measurement,
                 Strain = strain,
                 UnitThcContentUnitOfMeasure = unit_thc_content_unit_of_measure,
                 UnitWeight = unit_weight,
                 UnitWeightUnitOfMeasure = unit_weight_unit_of_measure
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

#' Delete Items
#' @export
#' @param license_number Facility license number
#' @param id Item ID
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.delete_items_v1_{id}}

metrc_delete_item <- function(license_number,
                                    id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(
    BASE_URL, path = paste0("items/v1/", id),
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

