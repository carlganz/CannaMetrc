#' Get Item Data
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.get_items_v1_{id}}
#' @param id Item ID

metrc_get_item <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", "items/v1", id = id) %>% 
    map(dropNullsOrEmpty) %>% as_tibble()
}

#' Get Active Items
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.get_items_v1_active}
#' @param license_number Facility license number

metrc_get_items_active <- function(license_number) {
  metrc_call("GET", "items/v1/active", license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Items Categories
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.get_items_v1_categories}

metrc_get_items_categories <- function() {
  metrc_call("GET", "items/v1/categories") %>%
    bind_rows()
  
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
                                 unit_thc_content,
                                 unit_thc_content_unit_of_measure,
                                 unit_weight,
                                 unit_weight_unit_of_measure) {
  metrc_call("POST", "items/v1/create", license_number = license_number, body = data.frame(
    ItemCategory = item_category,
    Name = name,
    UnitOfMeasure = unit_of_measurement,
    Strain = strain,
    UnitThcContent = unit_thc_content,
    UnitThcContentUnitOfMeasure = unit_thc_content_unit_of_measure,
    UnitWeight = unit_weight,
    UnitWeightUnitOfMeasure = unit_weight_unit_of_measure
  ))
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
                                 unit_thc_content,
                                 unit_thc_content_unit_of_measure,
                                 unit_weight,
                                 unit_weight_unit_of_measure) {
  metrc_call("POST", "items/v1/update", license_number = license_number, body = data.frame(
    Id = id,
    ItemCategory = item_category,
    Name = name,
    UnitOfMeasure = unit_of_measurement,
    Strain = strain,
    UnitThcContent = unit_thc_content,
    UnitThcContentUnitOfMeasure = unit_thc_content_unit_of_measure,
    UnitWeight = unit_weight,
    UnitWeightUnitOfMeasure = unit_weight_unit_of_measure
  ))
  
}

#' Delete Items
#' @export
#' @param license_number Facility license number
#' @param id Item ID
#' @note See \url{https://api-co.metrc.com/Documentation/#Items.delete_items_v1_{id}}

metrc_delete_item <- function(license_number, id) {
  stopifnot(is.integer(id))

  metrc_call("DELETE", "items/v1", id = id, license_number = license_number)
  
}

