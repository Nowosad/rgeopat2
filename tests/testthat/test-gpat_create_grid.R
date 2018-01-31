context("gpat_create_grid")
test_that("grid is created", {

  header_filepath = system.file("rawdata/Augusta2011_grid100.hdr", package="rgeopat2")

  # options work as expected
  expect_equal(dim(gpat_create_grid(header_filepath)), c(322, 1))
  expect_equal(dim(gpat_create_grid(header_filepath, brick = TRUE)), c(84, 1))

  # bad inputs
  expect_error(gpat_create_grid("New name"))
  expect_error(gpat_create_grid(header_filepath, brick = "yes"))

  # shift cannot be different from size
  header_filepath2 = system.file("rawdata/Augusta2011_grid_size_shift.hdr", package="rgeopat2")
  expect_error(gpat_create_grid(header_filepath2))
})
