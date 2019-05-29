context("gpat_read_txt")

test_that("read global", {
  # bad inputs
  expect_error(gpat_read_txt("my_invisible_file"))
  expect_error(gpat_read_txt(my_lind, signature = "fake_signature"))

})

test_that("read points", {

  my_points = system.file("rawdata/Augusta2011_points.txt", package = "rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_points)), 4)

})

test_that("read polygons", {

  my_polygon = system.file("rawdata/Augusta2011_polygon.txt", package = "rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_polygon)), 19)

})

test_that("read lind", {

  my_lind = system.file("rawdata/Augusta2011_lind.txt", package = "rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_lind, signature = "lind")), 19)

})

test_that("read linds", {

  my_linds = system.file("rawdata/Augusta2011_polygon.txt", package = "rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_linds, signature = "linds")), 19)

})

test_that("read grid", {

  my_grid = system.file("rawdata/Augusta2011_grid100.txt", package = "rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_grid)), 322)

})

test_that("read gridlinds", {

  my_gridlinds = system.file("rawdata/Augusta2011_grid_linds.txt", package = "rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_gridlinds, signature = "linds")), 322)

})

test_that("read gridts", {

  my_gridts = system.file("rawdata/barent_ts_grd.txt", package = "rgeopat2")

  # data is intact
  expect_equal(nrow(gpat_read_txt(my_gridts, signature = "ts")), 1579)

})
