---
title: "TestAssignment"
author: "Nancy"
date: "8/14/2020"
output: md_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE )
```

### TestAssignment Package - Overview

This is an R package which created as part of the last assignment for the coursera lesson "Building an R Package" which is a part of the "Mastering Software Development in R Specialization". 

This package includes the following functions:

+ fars_read
+ make_filename
+ fars_read_years
+ fars_summarize_years
+ fars_map_state
\
\

##### **Input Files**: 

As input for this assignment we are using data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes. 

A sample of the data are:

```{r sample_of_source_data, echo=FALSE, results=TRUE}

setwd("~/data")

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

source <- fars_read("accident_2013.csv")

head(source)

```
\

##### **Final Output**: 

The final output is given through the execution of the "fars_map_state". This function returns a plot of the incidences for the specified year and state. Specifically, it will return a map of the state specified including the incidences as dots.

Below you can find an example of the incidences for state **45** and year **2013**:

```{r sample_of_output, echo=FALSE, results=TRUE}

setwd("~/data")

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

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

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

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

fars_map_state(45,2013)


```


## Testing 

I am working in Windows environment so Travis could not proceed with the testing, this is why I tested it also with appveyor which is the most suitable option for this operating system. 


##### Travis Badge

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/a-antonopoulou/TestAssignment.svg?branch=master)](https://travis-ci.org/github/a-antonopoulou/TestAssignment/builds/718049257)
<!-- badges: end -->



##### appveyor Badge

 <!-- badges: start -->
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/a-antonopoulou/TestAssignment?branch=master&svg=true)](https://ci.appveyor.com/project/a-antonopoulou/TestAssignment)
  <!-- badges: end -->
  
