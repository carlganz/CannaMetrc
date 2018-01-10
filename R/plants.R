#' Get Plant Info
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_{id}}
metrc_get_plant <- function(id) {
  metrc_call("GET", "plants/v1", id = id) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Vegetative Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_vegetative}
metrc_get_plants_vegetative <- function(license_number) {
  metrc_call("GET", "plants/v1/vegetative", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Flowering Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_vegetative}
metrc_get_plants_flowering <- function(license_number) {
  metrc_call("GET", "plants/v1/flowering", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Onhold Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_onhold}
metrc_get_plants_onhold <- function(license_number) {
  metrc_call("GET", "plants/v1/onhold", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Inactive Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_inactive}
metrc_get_plants_inactive <- function(license_number) {
  metrc_call("GET", "plants/v1/inactive", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Post Move Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_moveplants}
metrc_post_plants_move_plants <- function(license_number,
                                     id, label, room, actual_date) {
  metrc_call("POST", "plants/v1/moveplants", license_number = license_number, body = data.frame(
    Id = id, Label = label, Room = room, ActualDate = actual_date
  ))
  
}

#' Post Move Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_moveplants}
metrc_post_plants_change_growth_phase <- function(license_number,
                                      id, label, new_tag, growth_phase,
                                      new_room, growth_date) {
  metrc_call("POST", "plants/v1/changegrowthphases", license_number = license_number, body = data.frame(
    Id = id, Label = label, NewTag = new_tag, GrowthPhase = growth_phase,
    NewRoom = new_room, GrowthDate = growth_date
  ))
  
}

#' Post Delete Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_destroyplants}
metrc_post_plants_destroy <- function(license_number,
                                                 id, label, reason_note, actual_date) {
  metrc_call("POST", "plants/v1/destroyplants", license_number = license_number, body = data.frame(
    Id = id, Label = label, ReasonNote = reason_note, ActualDate = actual_date
  ))
  
}

#' Post New Plant Plantings
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_create_plantings}
metrc_post_plants_create_plantings <- function(license_number, plant_label,
                                           plant_batch_name, plant_batch_type,
                                           plant_count, strain_name, actual_date) {
  metrc_call("POST", "plants/v1/create/plantings", license_number= license_number, body = data.frame(
    PlantLabel = plant_label,
    PlantBatchName = plant_batch_name, 
    PlantBatchType = plant_batch_type,
    PlantCount = plant_count, 
    StrainName = strain_name, 
    ActualDate = actual_date
  ))
}

#' Post Manicure Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_manicureplants}
metrc_post_plants_manicure <- function(license_number, plant,
                                       weight, unit_of_weight, drying_room,
                                       harvest_name, actual_date) {
  metrc_call("POST", "plants/v1/manicureplants", license_number = license_number, body = data.frame(
    Plant = plant,
    Weight = weight, UnitOfWeight = unit_of_weight,
    DryingRoom = drying_room,
    HarvestName = harvest_name, ActualDate = actual_date
  ))
  
}

#' Post Harvest Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_harvestplants}
metrc_post_plants_harvest <- function(license_number, plant,
                                       weight, unit_of_weight, drying_room,
                                       harvest_name, actual_date) {
  metrc_call("POST", "plants/v1/harvestplants", license_number = license_number, body = data.frame(
    Plant = plant,
    Weight = weight, UnitOfWeight = unit_of_weight,
    DryingRoom = drying_room,
    HarvestName = harvest_name, ActualDate = actual_date
  ))
  
}