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

suma_max_count <- function(df, filterBy = NULL, groupLocation = FALSE, groupHour = FALSE, groupWeekday = FALSE, groupMonth = FALSE, groupActs = FALSE, op){
  groupBy <- suma_group_check(groupLocation, groupHour, groupWeekday, groupMonth, groupActs)
  suma_math_conditions(df, groupBy, filterBy, op=max)
}

#' Determine the lowest actual count across sessions
#'
#' This function determines the minimum count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_min_count <- function(df, groupBy = NULL, filterBy = NULL){
 suma_math_conditions(df, groupBy, filterBy, op=min)
}

#' Determine the mean count from a single session
#'
#' This function determines the mean count across, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_mean_count <- function(df, groupBy = NULL, filterBy = NULL){
  suma_math_conditions(df, groupBy, filterBy, op=mean)
}

#' Helper function to suma_MATH_count
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results
#' @param op Used to specify which operation will be used (min, max, mean, etc.)
suma_math_conditions <- function(df, groupBy, filterBy = NULL, op) {
  ifelse(length(groupBy) == 0 & is.null(filterBy),
         suma_math_nulls(df, op),
         ifelse(length(groupBy) == 0 & !is.null(filterBy),
                suma_math_filter(df, filterBy, op),
                ifelse(length(groupBy) != 0 & is.null(filterBy),
                       suma_math_group(df, groupBy, op),
                       suma_math_full(df, groupBy, filterBy, op)
                )
         )
  )
}

#' Helper function, checks whether to group results of Suma math functions and returns vector for use later
#' @inheritParams suma_max_count
suma_group_check <- function(groupLocation = FALSE, groupHour = FALSE, groupWeekday = FALSE, groupMonth = FALSE, groupActs = FALSE){
  groupBy <- vector(mode="character", length = 0)
  if(isTRUE(groupLocation)){
    groupBy <- c(groupBy, "location")
  }
  if(isTRUE(groupHour)){
    groupBy <- c(groupBy, "lubridate::hour(time)")
  }
  if(isTRUE(groupWeekday)){
    groupBy <- c(groupBy, "lubridate::wday(time)")
  }
  if(isTRUE(groupMonth)){
    groupBy <- c(groupBy, "lubridate::month(time)")
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
    dplyr::group_by(hour=lubridate::hour(time), sessionId) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(value=op(value))
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_filter <- function(df, filterBy, op) {
  df %>%
    dplyr::filter_(filterBy) %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    dplyr::group_by(hour=lubridate::hour(time), sessionId) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(value=op(value))
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_group <- function(df, groupBy, op) {
  df <- df %>%
    dplyr::distinct(countId, .keep_all = TRUE) %>%
    dplyr::group_by_(.dots = groupBy) %>%
    dplyr::summarise_(value=~n)
  return(df)
}
