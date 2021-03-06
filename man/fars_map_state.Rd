% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plot a state mape with locations of fatal accidents by LAT/LON}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{A single number identifying the state (see Note)}

\item{year}{A single number representing the year from which data is asked}
}
\description{
This function creates a geographical map depicting the data on accidents
with fatalities in a selected state in a particular year.
}
\note{
While the function returns NULL, a graphical representation of the
state with fatal accidents is plotted by latitude and longitude

for state.num decoding table, consult p26 of the pdf document at
\url{https://crashstats.nhtsa.dot.gov/Api/Public/Publication/812315}
}
\examples{
\dontrun{
fars_map_state(6, 2015)}

}
