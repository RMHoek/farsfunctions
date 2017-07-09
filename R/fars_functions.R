## Building package Assignment by RM Hoek

#' Read a FARS file
#'
#' This Helper-function checks if the filename given exists and assumes the data
#' is in .csv.bz2 format, which it will format as data frame.
#'
#' @param filename A character string representing a filename
#' @return A data.frame with fars data from the file called 'filename'
#'
#' @note although the existance of the file (filename) is checked, it is
#' assumed that the file is in csv format, if not a warning is passed by
#' readr::read_csv.
#'
#' @note this package comes with three example files that can be accessed using
#' \code{system.file("extdata", "accident_2013.csv.bz2", package = "farsfunctions")}
#' \code{system.file("extdata", "accident_2014.csv.bz2", package = "farsfunctions")}
#' \code{system.file("extdata", "accident_2015.csv.bz2", package = "farsfunctions")}
#'
#' @examples \dontrun{
#' fars_read("accident_2015.csv.bz2")}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df

fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' Create FARS data filename by year
#'
#' This Helper-function constructs a valid FARS filename from a given year
#'
#' @param year The year which the datafile is asked for.
#' @return A character string with format: "accident_yyyy.csv.bz2", where
#' yyyy is the year parameter passed.
#'
#' @note there is no check whether the number is a valid year for the FARS data,
#' any typo or garbled number will result in a filename, correct or not.
#'
#' @examples \dontrun{
#' make_filename(2015)}

make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' write FARS file based on FARS dbf datafile
#'
#' Use the "accident.dbf" file from a known year obtained from the FTP site of
#' the NHTSA and placed in the working directory and convert it to a csv.bz2 file
#' with the correct filename to be used by the other fars functions.
#'
#' @param year a numberic vector of length 1 representing the year of the dbf
#' @return data.frame of the file that is written to the working directory.
#'
#' @note the function assumes that the user has put an "accident.dbf" file from
#' the FARS FTP site of a given year in the working directory. If this file is
#' available, the function will stop and show a message. Also, if the year given
#' is not between 1975 and the year before the current year, the function will
#' stop and show a message.
#'
#' @examples \dontrun{
#' make_fars_file(2012)}
#'
#' @importFrom lubridate year now
#' @importFrom foreign read.dbf
#' @importFrom readr write_csv
#'
#' @export

make_fars_file <- function(year){
    if(!file.exists("accident.dbf"))
        stop("There is no file called 'accident.dbf' in the working directory")
    year <- as.integer(year)
    if(year < 1975 | year >= lubridate::year(lubridate::now()))
        stop("The year specified is not a valid year for FARS data")
    temp <- suppressMessages(foreign::read.dbf("accident.dbf"))
    filename <- make_filename(year)
    readr::write_csv(temp, filename)
}

#' Read FARS data files of given years
#'
#' This function returns a list of data files per year, as specified by the
#' parameter years. The parameter is used to create filenames, using the
#' make_filename helper function. If an invalid year is passed, the return
#' value for that year is NULL
#'
#' @param years A numeric vector containing the years of which the
#' FARS data is asked
#' @return A list of data.frames or NULL if the year resulted in an invalid file
#'
#' @note if any of the numbers in years is not valid, the list will contain
#' "NULL" at that index, and a "invalid year" warning will be issued. Much of the
#' detail in the data available in the FARS dataset is ignored, as only MONTH
#' and year are selected.
#'
#' @examples \dontrun{
#' fars_read_years(2015)
#' fars_read_years(c(2013, 2014, 2015))}
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @export

fars_read_years <- function(years) {
    # to avoid 'no visible binding for global variable' note at R CMD check
    MONTH = NULL
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>%
                dplyr::select(MONTH, year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' Summarize FARS data over years
#'
#' This function summarises the number of accidents with fatalities per
#' month and year over the years asked for.
#'
#' @param years A numeric vector containing the years of which the
#' FARS data is asked
#' @return A table of number of accidents with fatalities per month (rows)
#' of the years asked for (columns)
#'
#' @note if a year is invalid, it is ignored. Much of the detail in the data
#' available in the FARS dataset is ignored, as only number of fatalities are
#' collected
#'
#' @examples \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))}
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    # to avoid 'no visible binding for global variable' note at R CMD check
    MONTH = n = NULL
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' Plot a state mape with locations of fatal accidents by LAT/LON
#'
#' This function creates a geographical map depicting the data on accidents
#' with fatalities in a selected state in a particular year.
#'
#' @param state.num A single number identifying the state (see Note)
#' @param year A single number representing the year from which data is asked
#' @return NULL
#'
#' @note While the function returns NULL, a graphical representation of the
#' state with fatal accidents is plotted by latitude and longitude
#'
#' @note for state.num decoding table, consult p26 of the pdf document at
#' \url{https://crashstats.nhtsa.dot.gov/Api/Public/Publication/812315}
#'
#' @examples \dontrun{
#' fars_map_state(6, 2015)}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export

fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)
    # to avoid 'no visible binding for global variable' note at R CMD check
    STATE = NULL
    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if(nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                  xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
