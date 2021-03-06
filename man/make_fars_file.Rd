% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_fars_file}
\alias{make_fars_file}
\title{write FARS file based on FARS dbf datafile}
\usage{
make_fars_file(year)
}
\arguments{
\item{year}{a numberic vector of length 1 representing the year of the dbf}
}
\value{
data.frame of the file that is written to the working directory.
}
\description{
Use the "accident.dbf" file from a known year obtained from the FTP site of
the NHTSA and placed in the working directory and convert it to a csv.bz2 file
with the correct filename to be used by the other fars functions.
}
\note{
the function assumes that the user has put an "accident.dbf" file from
the FARS FTP site of a given year in the working directory. If this file is
available, the function will stop and show a message. Also, if the year given
is not between 1975 and the year before the current year, the function will
stop and show a message.
}
\examples{
\dontrun{
make_fars_file(2012)}

}
