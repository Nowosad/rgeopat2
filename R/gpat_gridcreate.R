#' Grid polygon creator
#'
#' Creates a polygon of a geoPAT grid based on the grid header
#'
#' @param x A filepath to the geoPAT 2.0 grid header file
#' @param brick TRUE/FALSE; should a new grid polygon have a brick topology
#'
#' @return sfc_POLYGON
#'
#' @importFrom sf %>% st_polygon st_sfc st_set_crs st_sf
#' @importFrom stats aggregate
#'
#' @examples
#' header_filepath = system.file("rawdata/Augusta2011_grid100.hdr", package="rgeopat2")
#' my_grid = gpat_gridcreate(header_filepath)
#' my_grid_brick = gpat_gridcreate(header_filepath, brick = TRUE)
#'
#' plot(my_grid)
#' plot(my_grid_brick, add = TRUE, border = "red", lwd = 3)
#'
#' @export
gpat_gridcreate = function(x, brick = FALSE){
  header = gpat_header_parser(x)

  x1 = header$start_x
  y1 = header$start_y
  x2 = header$start_x + header$res_x * header$n_cols
  y2 = header$start_y + header$res_y * header$n_rows

  single_cell_creator = function(x1, y1, x2, y2){
    list(rbind(c(x1, y1), c(x1, y2), c(x2, y2), c(x2, y1), c(x1, y1)))
  }

  my_bb = single_cell_creator(x1, y1, x2, y2) %>%
    st_polygon() %>%
    st_sfc()

  if (header$proj_4 != ""){
    my_bb = my_bb %>%
      st_set_crs(value = header$proj_4)
  }

  my_grid = gpat_st_make_grid(my_bb,
                              n = c(header$n_cols, header$n_rows),
                              brick = brick)
  my_grid
}

#' Parse a header of a geoPAT 2.0 grid file
#'
#' Extracts basic information from a geoPAT 2.0 grid header file
#'
#' @param x A filepath to the geoPAT 2.0 grid header file
#'
#' @return data_frame
#'
#' @importFrom sf %>% st_crs
#' @importFrom stringr str_sub
#' @importFrom utils capture.output
gpat_header_parser = function(x){
  x = readLines(x)
  res_x = str_sub(x[5], start=6) %>% as.double()
  res_y = str_sub(x[9], start=6) %>% as.double()
  start_x = str_sub(x[4], start=6) %>% as.double()
  start_y = str_sub(x[7], start=6) %>% as.double()
  n_rows = str_sub(x[10], start=7) %>% as.integer()
  n_cols = str_sub(x[11], start=7) %>% as.integer()

  # proj_4 = tryCatch({st_crs(wkt = str_sub(x[12], start=7))$proj4string},
  #                   error = function(e) "")

  invisible(capture.output({proj_4 = tryCatch({st_crs(wkt = "")$proj4string},
                    error = function(e) "")}))

  data.frame(res_x = res_x, res_y = res_y,
           start_x = start_x, start_y = start_y,
           n_rows = n_rows, n_cols = n_cols,
           proj_4 = proj_4, stringsAsFactors = FALSE)
}

#' Grid polygon creator (without a header)
#'
#' Creates a polygon of a gpat grid based on a given parameters
#'
#' @param x An object of class sf or sfc
#' @param n An integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @param brick TRUE/FALSE; should a new grid polygon have a brick topology
#'
#' @references Based on the st_make_grid function from the sf package
#'
#' @return sfc_POLYGON
#'
#' @importFrom sf %>% st_bbox st_polygon st_sfc st_crs st_cast
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#'
#' my_grid = gpat_st_make_grid(nc) %>%
#'   st_as_sf(data.frame(id = 1:100), .)
#'
#' grid_centroids = st_centroid(my_grid) %>%
#'   st_coordinates(grid_centroids) %>%
#'   as_data_frame() %>%
#'   mutate(id = 1:100)
#'
#' ggplot() +
#'   geom_sf(data = my_grid) +
#'   geom_text(data = grid_centroids, aes(x = X, y = Y, label = id)) +
#'   theme_void()
#' }
gpat_st_make_grid = function(x,
                              n = c(10, 10),
                              brick = FALSE){

  offset = st_bbox(x)[c(1, 4)]
  bb = st_bbox(x)
  n = rep(n, length.out = 2)
  nx = n[1]
  ny = n[2]
  xc = seq(offset[1], bb[3], length.out = nx + 1)
  yc = seq(offset[2], bb[2], length.out = ny + 1)

  ret = vector("list", nx * ny)
  square = function(x1, y1, x2, y2){
    st_polygon(list(matrix(c(x1, x2, x2, x1, x1, y1, y1, y2, y2, y1), 5)))
  }
  for (i in 1:nx) {
    for (j in 1:ny) {
      ret[[(j - 1) * nx + i]] = square(xc[i], yc[j], xc[i + 1], yc[j + 1])
    }
  }

  my_grid = st_sf(st_sfc(ret, crs = st_crs(x)))

  if (brick){
    ids_y = rep(c(1, 1, 2, 2), length.out = ny)
    ids = numeric(length = nrow(my_grid))

    for (i in seq_len(ny)){
      ids_y_i = ids_y[i]
      if (ids_y_i == 1){
        ids[seq_len(nx) + (i - 1) * nx] = rep(c(1:12), each = 2, length.out = nx)
      } else if (ids_y_i == 2){
        ids[seq_len(nx) + (i - 1) * nx] = c(13, rep(c(14:24), each = 2, length.out = nx-1))
      }
    }
    my_grid = aggregate(my_grid, by = list(ids), mean) %>%
      st_cast(to = "POLYGON", warn = FALSE)
    my_grid$Group.1 = NULL
  }

  return(my_grid)
}
