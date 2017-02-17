#' Retrieves activity names from Suma server
#'
#' This function retrieves a table of activity names and ID numbers, suitable for use as a key for suma_decode_activities function.
#' @param url The url of the Suma query server. If your client is at "yourlibrary.edu/suma/client" this is usually "yourlibrary.edu/sumaserver"
#' @export
#' @examples
#' sumaKey <- suma_get_activities()

suma_get_activities <- function(url = "https://library.dartmouth.edu/sumaserver/") {
  acts <- jsonlite::fromJSON(paste0(url, "query/counts?format=ALC"))
  activities <- data.table::rbindlist(acts$dictionary$activities) %>% unique() %>% dplyr::select(actId = id, actName = title) %>% dplyr::arrange(actId)
  return(activities)
}

#' Retrieves location names from Suma server
#'
#' This function retrieves a table of location names and ID numbers.
#' @param url The url of the Suma query server. If your client is at "yourlibrary.edu/suma/client" this is usually "yourlibrary.edu/sumaserver"
#' @export
#' @examples
#' locs <- suma_get_locations()

suma_get_locations <- function(url = "https://library.dartmouth.edu/sumaserver/") {
  locs <- jsonlite::fromJSON(paste0(url, "query/counts?format=ALC"))
  locations <- data.table::rbindlist(locs$dictionary$locations) %>% unique() %>% dplyr::select(locId = id, locName = title, description) %>% dplyr::arrange(locId)
  return(locations)
}

#' Retrieves initiative names from Suma server
#'
#' This function retrieves a table of initiative names and ID numbers, useful for knowing which intiative ID to use when calling suma_from_api.
#' @param url The url of the Suma query server. If your client is at "yourlibrary.edu/suma/client" this is usually "yourlibrary.edu/sumaserver"
#' @export
#' @examples
#' initiatives <- suma_get_initiatives()

suma_get_initiatives <- function(url = "https://library.dartmouth.edu/sumaserver/") {
  df <- jsonlite::fromJSON(paste0(url, "query/counts?format=ALC"))
  initiatives <- df %>% dplyr::select(initiativeId = id, initiativeName = title, description) %>% dplyr::arrange(initiativeId)
  return(initiatives)
}

#' Decode activities from numbers into activity names
#'
#' This function separates multiple activities attached to single Suma counts, compares them to a key table, and assigns activity names as appropriate.
#' @param df Data frame containing the initial Suma data
#' @param key Data frame containing the key table
#' @export
#' @examples
#' df <- suma_from_api()
#' sumaKey <- suma_get_activities()
#' dfDecoded <- suma_decode_activities(df, sumaKey)

suma_decode_activities <- function(df, key) {
  df %>%
    dplyr::left_join(key)
}


