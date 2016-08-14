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
#' df <- suma_from_api()
#' sumaKey <- data("sumaKey")
#' dfDecoded <- suma_decode_activities(df, sumaKey)

suma_decode_activities <- function(df, key) {
  df %>%
    dplyr::left_join(key)
}

#' Determine the highest actual count across sessions
#'
#' This function determines the maximum count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupLocation Optional argument to group data by location
#' @param groupHour Optional argument to group data by hour of day
#' @param groupWeekday Optional argument to group data by day of the week
#' @param groupMonth Optional argument to group data by month
#' @param groupActs Optional argument to group data by activity
#' @param filterBy Optional argument to filter results

suma_max_count <- function(df, filterBy = NULL, groupBy = NULL){
  df <- suma_math_conditions(df, groupBy, filterBy, op = max)
  return(df)
}

#' Determine the lowest actual count across sessions
#'
#' This function determines the minimum count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_min_count <- function(df, groupBy = NULL, filterBy = NULL){
  df <- suma_math_conditions(df, groupBy, filterBy, op=min)
  return(df)
}

#' Determine the mean count across sessions
#'
#' This function determines the mean count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_mean_count <- function(df, groupBy = NULL, filterBy = NULL){
  df <- suma_math_conditions(df, groupBy, filterBy, op=mean)
  return(df)
}

#' Determine the median count across sessions
#'
#' This function determines the median count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_median_count <- function(df, groupBy = NULL, filterBy = NULL){
  df <- suma_math_conditions(df, groupBy, filterBy, op=median)
  return(df)
}

#' Helper function to suma_MATH_count
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results
#' @param op Used to specify which operation will be used (min, max, mean, etc.)
suma_math_conditions <- function(df, groupBy = NULL, filterBy = NULL, op) {
  ifelse(is.null(groupBy) & is.null(filterBy),
         df <- suma_math_nulls(df, op),
         ifelse(is.null(groupBy) & !is.null(filterBy),
                df <- suma_math_filter(df, filterBy, op),
                ifelse(!is.null(groupBy) & is.null(filterBy),
                       df <- suma_math_group(df, groupBy, op),
                       df <- suma_math_full(df, groupBy, filterBy, op)
                )
         )
  )
  return(df)
}

#' Helper function, checks whether to group results of Suma math functions and returns vector for use later
#' @inheritParams suma_max_count
suma_group_check <- function(groupLocation = FALSE, groupHour = FALSE, groupWeekday = FALSE, groupMonth = FALSE, groupActs = FALSE){
  groupBy <- vector(mode="character", length = 0)
  if(isTRUE(groupLocation)){
    groupBy <- c(groupBy, "location")
  }
  if(isTRUE(groupHour)){
    groupBy <- c(groupBy, "lubridate::hour(sessionStart)")
  }
  if(isTRUE(groupWeekday)){
    groupBy <- c(groupBy, "lubridate::wday(sessionStart)")
  }
  if(isTRUE(groupMonth)){
    groupBy <- c(groupBy, "lubridate::month(sessionStart)")
  }
  if(isTRUE(groupActs)){
    groupBy <- c(groupBy, "activities")
  }
  return(groupBy)
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_nulls <- function(df, op) {
  df %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    dplyr::group_by(sessionId) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(value=op(value))
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_filter <- function(df, filterBy, op) {
  df <- df %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    dplyr::filter_(filterBy) %>%
    dplyr::group_by(sessionId) %>%
    dplyr::summarize(value = n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(value = op(value))
  return(df)
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_group <- function(df, groupBy, op) {
  tmp <- c(groupBy, "sessionId")
  df <- df %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    dplyr::group_by_(.dots = tmp) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = groupBy) %>%
    dplyr::summarize(value = op(value))
  return(df)
}
