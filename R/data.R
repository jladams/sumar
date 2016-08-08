suma_from_api <- function(url, initiativeId, startDate, endDate) {
  jsonlite::fromJSON(paste0(url, "analysis/reports/lib/php/rawDataResults.php?id=", initiativeId, "&sdate=", lubridate::ymd(startDate), "&edate=", lubridate::ymd(endDate)), flatten=TRUE) %>%
    dplyr::mutate(term=getTerms(time)) %>%
    dplyr::distinct(countId, .keep_all = TRUE)
}

suma_decode_activities <- function(df, key) {
  dplyr::left_join(
    tidyr::unnest(df),
  key)
}
