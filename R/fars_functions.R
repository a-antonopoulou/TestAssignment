#' Print "fars_read"
#'
#' This is a simple function that, reads a csv which name will be included as the solely argument in the function.
#' If the csv does not exist, the execution of the function will stop by throwing a relevant message.
#'
#' @param filename A character string specifying the name of the csv that will be imported in R
#'
#' @return This function returns the imported file in R in a tibble format (class "tbl_df")
#'
#' @importFrom("readr", "read_csv")
#' @importFrom("dplyr", "tbl_df")
#'
#' @examples
#' fars_read("accident_2013.csv")
#'
#' @note Please note that in this function uses the read_csv function from readr package and the tbl_df function from the dplyr package.
#' @note Additionally to above, you should have in mind that the files to be imported in R should be either placed in the working directory or the path to be included as part of the parameter (input) of the function.
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Print "make_filename"
#'
#' This is a function that, returns the name of the file related to the year specified as parameter.
#'
#' @param year The value of the year.
#'
#' @return This function returns the name "accident_%d.csv.bz2" but instead of %d will concatenate the value of year specified as input in the function.
#'
#' @examples make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Print "fars_read_years"
#'
#' This function creates 2 columns Month and Year related to the source file of the specified year.
#'
#' @param years The value of the year. The year can be set as character or numeric and will not break the execution of the code. Also you can use a vector of years.
#'
#' @return This function returns 2 columns Month and Year.
#'
#' @importFrom("dplyr", "mutate", "select")
#'
#' @examples
#' fars_read_years("2013")
#' fars_read_years(c(2013,2015))
#'
#' @note In this function, are used the mutate ad select functions from dplyr package. So please make sure to have upfront install and load the dplyr package otherwise you may face issues/errors during the execution.
#'
#' @export
fars_read_years <- function(years) {
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

#' Print "fars_summarize_years"
#'
#' This is a function that, summarizes the number of rows included per month in the source file and the specified year. The format of the results, is a table of tibble class, with one row per month and with at least two columns. One presenting the MONTH and one for each year used as parameter during the execution.
#'
#' @param years A vector including the years that will be included in the analysis. As above, it can be one year or more and can be passed as character or numeric.
#'
#' @return This function returns a table of tibble class, with one row per month and with at least two columns. One presenting the MONTH and one for each year used as parameter during the execution.It basically provides the number of fatal injuries suffered in motor vehicle traffic crashes by Month and year.
#'
#' @importFrom("dplyr", "bind_rows", "summarize")
#' @importFrom("tidyr", "spread")
#'
#' @examples fars_summarize_years(c("2013","2014"))
#'
#' @note Again for the proper execution of this function, we need to verify that the dplyr package is installed and loaded and also that the source files are located in the folder that is currently set as working directory.
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Print "fars_map_state"
#'
#' This is function that takes as input the state and the year and provides as output a map of the specified state demonstarting the exact location of fatal injuries suffered in motor vehicle traffic crashes (as dots).
#'
#' @param state.num The state number. Only one value can be passed as state.num (either numeric or character). If not a proper state.num is specified, an error message will be thrown.
#' @param year The value of the year. Only one value can be passed as year (either numeric or character).
#'
#' @return This function returns a plot of the incidences for the specified year and state. Specifically, it will return a map of the state specified including the incidences as dots.
#'
#' @importFrom("dplyr", "filter")
#' @importFrom("maps", "map")
#' @importFrom("graphics", "points")
#'
#' @examples fars_map_state(45,2013)
#'
#' @note Needed packages for this function are: dplyr, maps, graphics. Furthermore, if there are not incidences for the specified combination of year/state number, a relevant message will pop-up.
#'
#' @export
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
