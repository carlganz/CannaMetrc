#' @import httr jsonlite memoise dplyr tibble purrr
metrc_auth <- memoise::memoise(function(software_key = Sys.getenv("metrc_software_key"), 
                       user_key = Sys.getenv("metrc_user_key")) {
  httr::authenticate(software_key, user_key)
})

BASE_URL <- memoise::memoise(function(state = Sys.getenv("metrc_state"), demo = Sys.getenv("metrc_demo")) {
  state <- match.arg(state, c("CA", "CO", "OR", "MD", "AK", "MI", "CA", "OH"))
  
  sprintf(paste0("https://", if (as.logical(demo)) {"sandbox-"}, "api-%s.metrc.com/"), tolower(state))
  
})

#' @import shiny
dropNulls <- getFromNamespace("dropNulls", "shiny")
dropNullsOrEmpty <- getFromNamespace("dropNullsOrEmpty", "shiny")

metrc_call <- function(type = c("GET", "POST", "PUT", "DELETE"), endpoint = c(), id = NULL, license_number = NULL, body = NULL) {
  url <- modify_url(BASE_URL(), path = paste0(endpoint, if (!is.null(id)) paste0("/", id)), query = list(
    licenseNumber = license_number
  ))
  
  type <- match.arg(type)
  # endpoint <- match.arg(endpoint)
  
  resp <- switch(type,
                 GET = GET(url, metrc_auth(), httr::accept_json(), user_agent("CannaData")),
                 POST = POST(url, metrc_auth(), body = body, encode = "json", httr::accept_json(), user_agent("CannaData"), httr::content_type_json()),
                 PUT = POST(url, metrc_auth(), body = body, encode = "json", httr::accept_json(), user_agent("CannaData"), httr::content_type_json()),
                 DELETE = DELETE(url, metrc_auth(), httr::accept_json(), user_agent("CannaData"))
  )
  if (type == "GET") {
  if (!(http_type(resp) %in% c("application/json", "text/json"))) {
    print(http_status(resp)$message)
    stop("Metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    print(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE))
    stop(paste("Metrc API errored:", 
               http_status(resp)$message, sep = "\n"), call. = FALSE)
  }
  } else {
    if (http_error(resp)) {
      print(paste("Metrc API errored:", 
                 http_status(resp)$message, sep = "\n"))
      print(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE))
      stop(paste("Metrc API errored:", 
                 http_status(resp)$message, sep = "\n"), call. = FALSE)
    } else {
      return(TRUE)
    }
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  
}
