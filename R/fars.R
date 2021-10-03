#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#'
#'fars_read
#'
#'This function reads the data from a csv into a dataframe to then coerce it into a tibble.
#'
#'@param filename filename of the data in csv archive
#'
#'@return returns a tibble containing the data from the csv
#'
#'@examples
#'fars_read("data.csv")
#'
#'
#'\dontrun{
#' fars_complete_2013 <- fars:::fars_read("accident_2013.csv.bz2")
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#'
#'make_filename
#'
#'This funciton generates a String of the format "accident_{year}.csv.bz2"
#'
#'@param year year that must be included in the filename
#'
#'@return String of the form "accident_{year}.csv.bz2"
#'
#'@examples
#'\dontrun{
#' fars:::make_filename(2013)
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  p <- sprintf("accident_%d.csv.bz2", year)
  file.path("inst", "extdata",p)
}

#'
#'fars_read_years
#'
#'The function reads the data from every year in the vector input
#'
#'@param years vector of the years that correspond to the data
#'
#'@return a list of tibbles containing de data from each year.
#'
#'@examples
#'\dontrun{
#' fars2013 <- fars:::fars_read_years(2013)
#' fars2013 <- fars:::fars_read_years("2013")
#' fars1314 <- fars:::fars_read_years(c(2013, 2014))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file) %>%
        dplyr::mutate(year = year) %>%
        dplyr::select(MONTH, year)
      return (dat)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#'
#'fras_summarize_years
#'
#'Creates a  tibble with the amount of accidents per month in each year
#'
#'@param years vector with years of interest
#'
#'@return a tibble with the amount of accidents per month in each year
#'
#'@examples
#'\dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#'
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#'
#'faras_map_state
#'
#'Plots the accidents from a state in a specific year
#'
#'@param state.num number corresponding to the state of interest
#'@param year year of interest
#'
#'@return NULL
#'
#'@examples
#' \dontrun{
#' fars_map_state(13, 2014)
#' }
#'
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

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
