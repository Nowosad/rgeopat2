context("gpat_read_distmtx")
test_that("read distmtx", {

  my_distmtx = system.file("rawdata/Augusta2011_matrix_grid.csv", package="rgeopat2")

  # data is intact
  expect_equal(length(gpat_read_distmtx(my_distmtx)), 51681)

  # bad inputs
  expect_error(gpat_read_distmtx("my_invisible_file"))

})
