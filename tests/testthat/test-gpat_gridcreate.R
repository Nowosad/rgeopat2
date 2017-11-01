context("gpat_gridcreate")
test_that("grid is created", {

  header_filepath = system.file("rawdata/Augusta2011_grid100.hdr", package="rgeopat2")

  # options work as expected
  expect_equal(dim(gpat_gridcreate(header_filepath)), c(322, 1))
  expect_equal(dim(gpat_gridcreate(header_filepath, brick = TRUE)), c(84, 1))

  # bad inputs
  expect_error(gpat_gridcreate("New name"))
  expect_error(gpat_gridcreate(header_filepath, brick = "yes"))

})
