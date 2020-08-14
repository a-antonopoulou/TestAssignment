
library(testthat)

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

testthat::expect_that(make_filename(2015), equals("accident_2015.csv.bz2"))

