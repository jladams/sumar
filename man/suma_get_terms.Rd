% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTerms.R
\name{suma_get_terms}
\alias{suma_get_terms}
\title{Compares a date to the terms of the Dartmouth academic calendar from 2009 - 2017}
\usage{
suma_get_terms(date_time)
}
\arguments{
\item{date_time}{A valid date or datetime, i.e., 2016-08-01}
}
\description{
Takes a date or datetime and compares it do the dates of the Dartmouth academic calendar, returns a Term name.
}
\examples{
suma_get_terms("2016-05-17")

df <- suma_from_api()
df$terms <- suma_get_terms(df$time)
}

