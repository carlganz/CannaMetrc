#' Get Room Info
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.get_rooms_v1_{id}}
metrc_get_room <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL, path = paste0("rooms/v1/", id))
  
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

#' Get Active Rooms
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.get_rooms_v1_active}
metrc_get_rooms_active <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "rooms/v1/active",
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

#' Post New Room
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.post_rooms_v1_create}
metrc_post_rooms <- function(license_number, name) {
  url <- modify_url(BASE_URL,
                    path = "rooms/v1/create",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(url, metrc_auth(), body = list(
    Name = name
  ))
  
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

#' Post Update Room
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.post_rooms_v1_update}
metrc_post_rooms_update <- function(license_number, id, name) {
  url <- modify_url(BASE_URL,
                    path = "rooms/v1/update",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(url, metrc_auth(), body = data.frame(
    Id = id,
    Name = name
  ))
  
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

#' Delete room
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.delete_rooms_v1_{id}}
metrc_delete_room <- function(license_number, id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL, path = paste0("rooms/v1/", id),
                    query = list(licenseNumber = license_number))
  
  resp <- DELETE(url, metrc_auth())
  
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