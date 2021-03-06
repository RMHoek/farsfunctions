% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize FARS data over years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A numeric vector containing the years of which the
FARS data is asked}
}
\value{
A table of number of accidents with fatalities per month (rows)
of the years asked for (columns)
}
\description{
This function summarises the number of accidents with fatalities per
month and year over the years asked for.
}
\note{
if a year is invalid, it is ignored. Much of the detail in the data
available in the FARS dataset is ignored, as only number of fatalities are
collected
}
\examples{
\dontrun{
fars_summarize_years(c(2013, 2014, 2015))}

}
