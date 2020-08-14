
library(testthat)

expect_that(make_filename(2015), equals("accident_2015.csv.bz2"))
gives_warning(fars_read_years(2017))
throws_error(fars_map_state(43,2015))
