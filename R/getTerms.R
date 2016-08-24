#' Compares a date to the terms of the Dartmouth academic calendar from 2009 - 2017
#'
#' Takes a date or datetime and compares it do the dates of the Dartmouth academic calendar, returns a Term name.
#' @export
#' @param date_time A valid date or datetime, i.e., 2016-08-01
#' @examples
#' getTerms("2016-05-17")
#'
#' df <- suma_from_api()
#' df$terms <- getTerms(df$time)
suma_get_terms <- function(date_time){
  ifelse(
    as.Date(date_time) %in% lubridate::ymd(20090625):lubridate::ymd(20090901) |
      as.Date(date_time) %in% lubridate::ymd(20100624):lubridate::ymd(20100831) |
      as.Date(date_time) %in% lubridate::ymd(20110623):lubridate::ymd(20110830) |
      as.Date(date_time) %in% lubridate::ymd(20120621):lubridate::ymd(20120828) |
      as.Date(date_time) %in% lubridate::ymd(20130620):lubridate::ymd(20130827) |
      as.Date(date_time) %in% lubridate::ymd(20140619):lubridate::ymd(20140826) |
      as.Date(date_time) %in% lubridate::ymd(20150625):lubridate::ymd(20150901) |
      as.Date(date_time) %in% lubridate::ymd(20160623):lubridate::ymd(20160830),
    "Summer",
    ifelse(
      as.Date(date_time) %in% lubridate::ymd(20090923):lubridate::ymd(20091209) |
        as.Date(date_time) %in% lubridate::ymd(20100922):lubridate::ymd(20101208) |
        as.Date(date_time) %in% lubridate::ymd(20110921):lubridate::ymd(20111207) |
        as.Date(date_time) %in% lubridate::ymd(20120910):lubridate::ymd(20121121) |
        as.Date(date_time) %in% lubridate::ymd(20130916):lubridate::ymd(20131127) |
        as.Date(date_time) %in% lubridate::ymd(20140915):lubridate::ymd(20141126) |
        as.Date(date_time) %in% lubridate::ymd(20150916):lubridate::ymd(20151125) |
        as.Date(date_time) %in% lubridate::ymd(20160912):lubridate::ymd(20161123),
      "Fall",
      ifelse(
        as.Date(date_time) %in% lubridate::ymd(20100104):lubridate::ymd(20100316) |
          as.Date(date_time) %in% lubridate::ymd(20110104):lubridate::ymd(20110316) |
          as.Date(date_time) %in% lubridate::ymd(20120104):lubridate::ymd(20120314) |
          as.Date(date_time) %in% lubridate::ymd(20130107):lubridate::ymd(20130325) |
          as.Date(date_time) %in% lubridate::ymd(20140106):lubridate::ymd(20140314) |
          as.Date(date_time) %in% lubridate::ymd(20150105):lubridate::ymd(20150317) |
          as.Date(date_time) %in% lubridate::ymd(20160104):lubridate::ymd(20160315) |
          as.Date(date_time) %in% lubridate::ymd(20170104):lubridate::ymd(20170315),
        "Winter",
        ifelse(
          as.Date(date_time) %in% lubridate::ymd(20100329):lubridate::ymd(20100613) |
            as.Date(date_time) %in% lubridate::ymd(20110328):lubridate::ymd(20110612) |
            as.Date(date_time) %in% lubridate::ymd(20120326):lubridate::ymd(20120610) |
            as.Date(date_time) %in% lubridate::ymd(20130325):lubridate::ymd(20130609) |
            as.Date(date_time) %in% lubridate::ymd(20140324):lubridate::ymd(20140608) |
            as.Date(date_time) %in% lubridate::ymd(20150330):lubridate::ymd(20150614) |
            as.Date(date_time) %in% lubridate::ymd(20160328):lubridate::ymd(20160612) |
            as.Date(date_time) %in% lubridate::ymd(20170327):lubridate::ymd(20170611),
          "Spring",
          "Intersession"
        )
      )
    )
  )
}
