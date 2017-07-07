## test file for fars_functions.R file

library(testthat)
f<-file(system.file("R", "fars_functions.R", package = "farsfunctions"), open = "r")
source(f)
close(f)

test_that("make_filename_string", {
    temp <- make_filename(2015)
    l <- nchar(temp)
    expect_equal(l, 21)
    expect_is(temp, "character")
})

test_that("fars_read_years_list", {
    expect_warning(fars_read_years(9999))
    temp <- fars_read_years(c(2013, 2014, 9999))
    n <- temp[[3]]
    expect_is(temp, "list")
    expect_identical(n, NULL)
})
