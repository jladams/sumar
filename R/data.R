#' Gather data from the Suma API
#'
#' This function hits the Suma API for the selected initiative ID during the given date range.
#' @importFrom magrittr %>%
#' @param url The base url, defaults to Dartmouth's https://library.dartmouth.edu/suma/
#' @param initiativeId The desired initiative to retrieve data for, defaults to 1
#' @param startDate Desired start date, defaults to beginning of current month
#' @param endDate Desired end date, defaults to today's date
#' @export
#' @examples
#' df <- suma_from_api("https://library.dartmouth.edu/suma/", 1, "2016-04-01", "2016-08-01")

suma_from_api <- function(url = "https://library.dartmouth.edu/suma/", initiativeId = 1, startDate = cut(Sys.Date(), "month"), endDate = Sys.Date()) {
  jsonlite::fromJSON(paste0(url, "analysis/reports/lib/php/rawDataResults.php?id=", initiativeId, "&sdate=", lubridate::ymd(startDate), "&edate=", lubridate::ymd(endDate)), flatten=TRUE) %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    tidyr::unnest()
}

#' Decode activities from numbers into activity names
#'
#' This function separates multiple activities attached to single Suma counts, compares them to a key table, and assigns activity names as appropriate.
#' @param df Data frame containing the initial Suma data
#' @param key Data frame containing the key table
#' @export
#' @examples
#' df <- suma_decode_activities(df, sumaKey)

suma_decode_activities <- function(df, key) {
  df %>%
    tidyr::unnest() %>%
    dplyr::left_join(key)
}

#' Determine the highest actual count from a single session
#'
#' This function determines the maximum count in a single session, with the option to group results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_max_count <- function(df, groupBy = NULL, filterBy = NULL){
  ifelse(is.null(groupBy) & is.null(filterBy),
    suma_max_nulls(df),
    ifelse(is.null(groupBy) & !is.null(filterBy),
      suma_max_filter(df, filterBy),
      ifelse(!is.null(groupBy) & is.null(filterBy),
        suma_max_group(df),
        suma_max_full(df)
      )
    )
  )
}

#' Helper function to suma_max_count
suma_max_nulls <- function(df) {
  df %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    dplyr::group_by(hour=lubridate::hour(time), sessionId) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(value=max(value))
}

#' Helper function to suma_max_count
suma_max_filter <- function(df, filterBy) {
  df %>%
    dplyr::filter_(filterBy) %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    dplyr::group_by(hour=lubridate::hour(time), sessionId) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(value=max(value))
}
