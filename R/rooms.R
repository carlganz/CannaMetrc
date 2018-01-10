#' Get Room Info
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.get_rooms_v1_{id}}
metrc_get_room <- function(id) {
  stopifnot(is.integer(id))

  metrc_call("GET", "rooms/v1", id = id) %>%
    as_tibble()
}

#' Get Active Rooms
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.get_rooms_v1_active}
metrc_get_rooms_active <- function(license_number) {
  
  metrc_call("GET", "rooms/v1/active", license_number = license_number) %>%
    bind_rows()
}

#' Post New Room
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.post_rooms_v1_create}
metrc_post_rooms <- function(license_number, name) {
  
  metrc_call("POST", "rooms/v1/create", license_number = license_number, body = data.frame(
    Name = name
  ))
}

#' Post Update Room
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.post_rooms_v1_update}
metrc_post_rooms_update <- function(license_number, id, name) {
  
  metrc_call("POST", "rooms/v1/update", license_number = license_number, body = data.frame(
    Id = id,
    Name = name
  ))
  
}

#' Delete room
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Rooms.delete_rooms_v1_{id}}
metrc_delete_room <- function(license_number, id) {
  stopifnot(is.integer(id))
  metrc_call("DELETE", "rooms/v1", id = id, license_number = license_number)
  
}