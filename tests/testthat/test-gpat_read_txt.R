context("gpat_read_txt")
test_that("read points", {

  my_points = system.file("rawdata/Augusta2011_points.txt", package="rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_points)), 4)

  # bad inputs
  expect_error(gpat_read_txt("my_invisible_file"))

})

test_that("read polygons", {

  my_polygon = system.file("rawdata/Augusta2011_polygon.txt", package="rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_polygon)), 19)

  # bad inputs
  expect_error(gpat_read_txt("my_invisible_file"))

})
