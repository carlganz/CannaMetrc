#' Get Plant Batches
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_{id}}
metrc_get_plant_batch <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", "plantbatches/v1", id = id) %>%
    map(dropNullsOrEmpty) %>% as.data.frame()
}

#' Get Active Plant Batches
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_active}
metrc_get_plant_batchs_active <- function(license_number) {
  metrc_call("GET", "plantbatches/v1/active", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Inactive Plant Batches
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_inactive}
metrc_get_plant_batchs_inactive <- function(license_number) {
  metrc_call("GET", "plantbatches/v1/inactive", license_number = license_number) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Plant Batch Types
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_types}
metrc_get_plant_batchs_types <- function() {
  metrc_call("GET", "plantbatches/v1/types") %>%
    unlist()
}

#' Post Create Plantings
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.post_plantbatches_v1_createplantings}
metrc_post_plant_batchs_create_plantings <- function(license_number,
                                     name,
                                     type,
                                     count,
                                     strain,
                                     actual_date) {
  metrc_call("POST", "plantbatches/v1/createplantings", license_number = license_number, body = data.frame(
    Name = name,
    Type = type,
    Count = count,
    Strain = strain,
    ActualDate = actual_date
  ))
  
}

#' Post Create Packages
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.post_plantbatches_v1_createpackages}
metrc_post_plant_batches_create_packages <- function(license_number,
                                             id,
                                             item,
                                             tag,
                                             count,
                                             actual_date) {
  metrc_call("POST", "plantbatches/v1/createpackages", license_number = license_number, body = data.frame(
    Id = id,
    Item = item,
    Tag = tag,
    Count = count,
    ActualDate = actual_date
  ))
}

#' Post Change Plant Growth Phase
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.post_plantbatches_v1_changegrowthphase}
metrc_post_plant_batches_change_growth_phase <- function(license_number,
                                             id,
                                             count,
                                             starting_tag,
                                             growth_phase,
                                             new_room,
                                             growth_date) {
  metrc_call("POST", "plantbatches/v1/changegrowthphase", license_number = license_number, body = data.frame(
    Id = id,
    Count = count,
    StartingTag = starting_tag,
    GrowthPhase = growth_phase,
    NewRoom = new_room,
    GrowthDate = growth_date
  ))
  
}

#' Post Destroy Plant
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.post_plantbatches_v1_destroy}
metrc_post_plant_batches_destroy <- function(license_number,
                                     id,
                                     count,
                                     reason_note,
                                     actual_date) {
  metrc_call("POST", "plantbatches/v1/destroy", license_number = license_number, body = data.frame(
    Id = id,
    Count = count,
    ReasonNote = reason_note,
    ActualDate = actual_date
  ))
  
}