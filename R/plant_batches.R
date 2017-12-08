#' Get Plant Batches
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_{id}}
metrc_get_plant_batch <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL(), path = paste0("plantbatches/v1/", id))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
    map(shiny:::dropNullsOrEmpty) %>% as.data.frame()
}

#' Get Active Plant Batches
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_active}
metrc_get_plant_batchs_active <- function(license_number) {
  url <- modify_url(BASE_URL(),
                    path = "plantbatches/v1/active",
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
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
    map(shiny:::dropNullsOrEmpty) %>% bind_rows()
}

#' Get Inactive Plant Batches
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_inactive}
metrc_get_plant_batchs_inactive <- function(license_number) {
  url <- modify_url(BASE_URL(),
                    path = "plantbatches/v1/inactive",
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
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
    map(shiny:::dropNullsOrEmpty) %>% bind_rows()
}

#' Get Plant Batch Types
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.get_plantbatches_v1_types}
metrc_get_plant_batchs_types <- function() {
  url <- modify_url(BASE_URL(), path = "plantbatches/v1/types")
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
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
  url <- modify_url(BASE_URL(),
                    path = "plantbatches/v1/createplantings",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url, encode = "json",
    metrc_auth(),
    body = data.frame(
      Name = name,
      Type = type,
      Count = count,
      Strain = strain,
      ActualDate = actual_date
    ), verbose()
  )
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }  
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
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
  url <- modify_url(BASE_URL(),
                    path = "plantbatches/v1/createpackages",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Id = id,
      Item = item,
      Tag = tag,
      Count = count,
      ActualDate = actual_date
    )
  )
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
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
  url <- modify_url(BASE_URL(),
                    path = "plantbatches/v1/changegrowthphase",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Id = id,
      Count = count,
      StartingTag = starting_tag,
      GrowthPhase = growth_phase,
      NewRoom = new_room,
      GrowthDate = growth_date
    )
  )
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}

#' Post Destroy Plant
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#PlantBatches.post_plantbatches_v1_destroy}
metrc_post_plant_batches_destroy <- function(license_number,
                                     id,
                                     count,
                                     reason_note,
                                     actual_date) {
  url <- modify_url(BASE_URL(),
                    path = "plantbatches/v1/destroy",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(
    url,
    metrc_auth(),
    body = data.frame(
      Id = id,
      Count = count,
      ReasonNote = reason_note,
      ActualDate = actual_date
    )
  )
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}