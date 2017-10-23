#' Get Plant Info
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_{id}}
metrc_get_plant <- function(id) {
  
  url <- modify_url(BASE_URL, path = paste0("plants/v1/", id))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Vegetative Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_vegetative}
metrc_get_plants_vegetative <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/vegetative",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Flowering Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_vegetative}
metrc_get_plants_flowering <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/flowering",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Onhold Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_onhold}
metrc_get_plants_onhold <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/onhold",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Inactive Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.get_plants_v1_inactive}
metrc_get_plants_inactive <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/inactive",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Move Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_moveplants}
metrc_post_plants_move_plants <- function(license_number,
                                     id, label, room, actual_date) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/moveplants",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Id = id, Label = label, Room = room, ActualDate = actual_date
    )
  )
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Move Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_moveplants}
metrc_post_plants_change_growth_phase <- function(license_number,
                                      id, label, new_tag, growth_phase,
                                      new_room, growth_date) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/changegrowthphases",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Id = id, Label = label, NewTag = new_tag, GrowthPhase = growth_phase,
      NewRoom = new_room, GrowthDate = growth_date
    )
  )
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Delete Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_destroyplants}
metrc_post_plants_destroy <- function(license_number,
                                                 id, label, reason_note, actual_date) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/destroyplants",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Id = id, Label = label, ReasonNote = reason_note, ActualDate = actual_date
    )
  )
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post New Plant Plantings
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_create_plantings}
metrc_post_plants_create_plantings <- function(license_number, plant_label,
                                           plant_batch_name, plant_batch_type,
                                           plant_count, strain_name, actual_date) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/create/plantings",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      PlantLabel = plant_label,
      PlantBatchName = plant_batch_name, 
      PlantBatchType = plant_batch_type,
      PlantCount = plant_count, 
      StrainName = strain_name, 
      ActualDate = actual_date
    )
  )
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Manicure Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_manicureplants}
metrc_post_plants_manicure <- function(license_number, plant,
                                       weight, unit_of_weight, drying_room,
                                       harvest_name, actual_date) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/manicureplants",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Plant = plant,
      Weight = weight, UnitOfWeight = unit_of_weight,
      DryingRoom = drying_room,
      HarvestName = harvest_name, ActualDate = actual_date
    )
  )
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Post Harvest Plants
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Plants.post_plants_v1_harvestplants}
metrc_post_plants_harvest <- function(license_number, plant,
                                       weight, unit_of_weight, drying_room,
                                       harvest_name, actual_date) {
  url <- modify_url(BASE_URL,
                    path = "plants/v1/harvestplants",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Plant = plant,
      Weight = weight, UnitOfWeight = unit_of_weight,
      DryingRoom = drying_room,
      HarvestName = harvest_name, ActualDate = actual_date
    )
  )
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}