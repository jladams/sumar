#' Gather data from the Suma API
#'
#' This function hits the Suma API for the selected initiative ID during the given date range.
#' @importFrom magrittr %>%
#' @param url The base url, defaults to Dartmouth's https://library.dartmouth.edu/suma/
#' @param initiativeId The desired initiative to retrieve data for, defaults to 1
#' @param startDate Desired start date, defaults to beginning of current month
#' @param endDate Desired end date, defaults to today's date
#' @param sepDates Creates columns for easy grouping and sorting of date and time from sessionStart
#' @param unnest If TRUE, separates counts with multiple activities into one row per activity
#' @export
#' @examples
#' df <- suma_from_api("https://library.dartmouth.edu/sumaserver/", 1, "2016-04-01", "2016-08-01")

suma_from_api <- function(url = "https://library.dartmouth.edu/sumaserver/", initiativeId = 1, startDate = cut(Sys.Date(), "month"), endDate = Sys.Date(), sepDates = TRUE, unnest = TRUE) {
  df <- jsonlite::fromJSON(paste0(url,
                                  "query/sessions?id=", initiativeId,
                                  "&sdate=", gsub("-","",lubridate::ymd(startDate)),
                                  "&edate=", gsub("-","",lubridate::ymd(endDate))),
                           flatten=TRUE)

  locations <- df$initiative$dictionary$locations %>%
    unique() %>%
    dplyr::select(locId = id, location = title)

  actGroups <- df$initiative$dictionary$activityGroups %>%
    unique() %>%
    dplyr::select(groupId = id, actGroup = title)

  activities <- df$initiative$dictionary$activities %>%
    unique() %>%
    dplyr::select(actId = id, activity = title, groupId = activityGroup) %>%
    dplyr::left_join(actGroups, by = "groupId")

  df <- df$initiative$sessions %>%
    dplyr::rename(sessionId = id) %>%
    tidyr::unnest() %>%
    tidyr::unnest() %>%
    dplyr::rename(locId = location, actId = activities) %>%
    dplyr::left_join(locations, by = "locId") %>%
    dplyr::left_join(activities, by = "actId") %>%
    dplyr::select(sessionId, sessionStart = start, sessionEnd = end, time, countId = id, location, count = number, activities = activity, actGroup)

  if(unnest == FALSE){
    df <- df %>%
      dplyr::group_by(sessionId, sessionStart, sessionEnd, time, count, location, countId) %>%
      dplyr::summarise(activities = toString(unique(activities)))
  }
  if(sepDates){
    df$year <- lubridate::year(df$sessionStart)
    df$month <- lubridate::month(df$sessionStart)
    df$day <- lubridate::day(df$sessionStart)
    df$wday <- lubridate::wday(df$sessionStart, label = TRUE)
    df$hour <- lubridate::hour(df$sessionStart)
  }
  return(df)
}

#' Determine the highest actual count across sessions
#'
#' This function determines the maximum count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, wday, time
#' @param filterBy Optional argument to filter results

suma_max_count <- function(df, filterBy = NULL, groupBy = NULL){
  df <- suma_math_conditions(df, filterBy, groupBy, op = max)
  return(df)
}

#' Determine the lowest actual count across sessions
#'
#' This function determines the minimum count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_min_count <- function(df, filterBy = NULL, groupBy = NULL){
  df <- suma_math_conditions(df, filterBy, groupBy, op=min)
  return(df)
}

#' Determine the mean count across sessions
#'
#' This function determines the mean count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_mean_count <- function(df, filterBy = NULL, groupBy = NULL){
  df <- suma_math_conditions(df, filterBy, groupBy, op=mean)
  return(df)
}

#' Determine the median count across sessions
#'
#' This function determines the median count across sessions, with the option to group or filter results
#' @export
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results

suma_median_count <- function(df, filterBy = NULL, groupBy = NULL ){
  df <- suma_math_conditions(df, filterBy, groupBy, op=median)
  return(df)
}

#' Helper function to suma_MATH_count
#' @param df Data frame containing Suma Data
#' @param groupBy Optional argument to group data, such as term, weekday, time
#' @param filterBy Optional argument to filter results
#' @param op Used to specify which operation will be used (min, max, mean, etc.)
suma_math_conditions <- function(df, filterBy = NULL, groupBy = NULL, op) {
  ifelse(is.null(groupBy) & is.null(filterBy),
         df <- suma_math_nulls(df, op),
         ifelse(is.null(groupBy) & !is.null(filterBy),
                df <- suma_math_filter(df, filterBy, op),
                ifelse(!is.null(groupBy) & is.null(filterBy),
                       df <- suma_math_group(df, groupBy, op),
                       df <- suma_math_full(df, filterBy, groupBy, op)
                )
         )
  )
  return(df)
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_nulls <- function(df, op) {
  df %>%
    dplyr::group_by(sessionId) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(value=op(value))
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_filter <- function(df, filterBy, op) {
  df <- df %>%
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
    dplyr::group_by_(.dots = tmp) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = groupBy) %>%
    dplyr::summarize(value = op(value))
  return(df)
}

#' Helper function to suma_MATH_count
#' @inheritParams suma_math_conditions
suma_math_full <- function(df, filterBy, groupBy, op) {
  tmp <- c(groupBy, "sessionId")
  df <- df %>%
    dplyr::filter_(filterBy) %>%
    dplyr::group_by_(.dots = tmp) %>%
    dplyr::summarize(value=n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = groupBy) %>%
    dplyr::summarize(value = op(value))
  return(df)
}
