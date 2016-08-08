suma_from_api <- function(url = "https://library.dartmouth.edu/suma/", initiativeId = 1, startDate = cut(Sys.Date(), "month"), endDate = Sys.Date()) {
  jsonlite::fromJSON(paste0(url, "analysis/reports/lib/php/rawDataResults.php?id=", initiativeId, "&sdate=", lubridate::ymd(startDate), "&edate=", lubridate::ymd(endDate)), flatten=TRUE) %>%
    dplyr::mutate(term=getTerms(time)) %>%
    dplyr::distinct(countId, .keep_all = TRUE)
}

suma_decode_activities <- function(df, key) {
  dplyr::left_join(
    tidyr::unnest(df),
  key)
}
