#' Grid polygon creator
#'
#' Creates a polygon of a GeoPAT 2 grid based on the grid header
#'
#' @param x A filepath to the GeoPAT 2 grid header file
#' @param brick TRUE/FALSE; should a new grid polygon have a brick topology
#'
#' @return sf
#'
#' @importFrom sf %>% st_polygon st_sfc st_set_crs st_sf
#' @importFrom stats aggregate
#'
#' @examples
#' header_filepath = system.file("rawdata/Augusta2011_grid100.hdr", package="rgeopat2")
#' my_grid = gpat_create_grid(header_filepath)
#' my_grid_brick = gpat_create_grid(header_filepath, brick = TRUE)
#'
#' plot(my_grid)
#' plot(my_grid_brick, add = TRUE, border = "red", lwd = 3)
#'
#' @export
gpat_create_grid = function(x, brick = FALSE){
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

#' Parse a header of a GeoPAT 2 grid file
#'
#' Extracts basic information from a geoPAT 2 grid header file
#'
#' @param x A filepath to the GeoPAT 2 grid header file
#'
#' @return data_frame
#'
#' @importFrom sf %>% st_crs
#' @importFrom stringr str_sub str_split str_which
#' @importFrom utils capture.output
gpat_header_parser = function(x){
  x = readLines(x)
  res_x = str_sub(x[5], start=6) %>% as.double()
  res_y = str_sub(x[9], start=6) %>% as.double()
  start_x = str_sub(x[4], start=6) %>% as.double()
  start_y = str_sub(x[7], start=6) %>% as.double()
  n_rows = str_sub(x[10], start=7) %>% as.integer()
  n_cols = str_sub(x[11], start=7) %>% as.integer()
  proj_wkt = str_sub(x[12], start=7)

  # extract size and shift
  desc = str_split(x[13], "\\|", simplify = TRUE)
  size = as.numeric(desc[str_which(desc, "-z")+1])
  if (length(size) == 0){
    size = as.numeric(gsub("\\D", "", desc[str_which(desc, "--size=")]))
  }
  shift = as.numeric(desc[str_which(desc, "-f")+1])
  if (length(shift) == 0){
    shift = as.numeric(gsub("\\D", "", desc[str_which(desc, "--shift=")]))
  }
  # test if size != shift
  if (size != shift){
    stop("We don't support overlapping grids (where size != shift). Open a new issue on our github page if you want this option")
  }

  invisible(capture.output({proj_4 = tryCatch({st_crs(wkt = proj_wkt)$proj4string},
                                              error = function(e) "")}))

  data.frame(res_x = res_x, res_y = res_y,
             start_x = start_x, start_y = start_y,
             n_rows = n_rows, n_cols = n_cols,
             proj_4 = proj_4, stringsAsFactors = FALSE)
}

#' Grid polygon creator (without a header)
#'
#' Creates a polygon of a GeoPAT grid based on a given parameters
#'
#' @param x An object of class sf or sfc
#' @param n An integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @param brick TRUE/FALSE; should a new grid polygon have a brick topology
#'
#' @references Based on the st_make_grid function from the sf package
#'
#' @return sf
#'
#' @importFrom sf %>% st_bbox st_polygon st_sfc st_crs st_cast
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#'
#' my_grid = gpat_st_make_grid(nc)
#' my_grid$id = 1:100
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

  my_grid = st_sf(geometry = st_sfc(ret, crs = st_crs(x)))

  if (brick){
    ids_y = rep(c(1, 1, 2, 2), length.out = ny) # rows groups
    ids = numeric(length = nrow(my_grid)) # local ids

    spatial_ids = seq_len(nx %/% 2 + 1) # starting cell numbers in columns
    for (i in seq_len(ny)){
      ids_y_i = ids_y[i] # which row group
      if (ids_y_i == 1){
        ids[seq_len(nx) + (i - 1) * nx] = rep(spatial_ids, each = 2, length.out = nx) # add cell numbers
      } else if (ids_y_i == 2){
        if (ids_y[i-1] != ids_y[i]){ # if row group changed (yes)
          spatial_ids = spatial_ids + (nx %/% 2 + 1) # new cell numbers in columns
          ids[seq_len(nx) + (i - 1) * nx] = c(spatial_ids[1], rep(spatial_ids[-1], each = 2, length.out = nx-1)) # add cell numbers
        } else {
          ids[seq_len(nx) + (i - 1) * nx] = c(spatial_ids[1], rep(spatial_ids[-1], each = 2, length.out = nx-1)) # add cell numbers
          spatial_ids = spatial_ids + (nx %/% 2 + 1) # new cell numbers in columns
        }
      }
    }
    n = c(length(spatial_ids), max(ids) / length(spatial_ids))

    my_grid = aggregate(my_grid, by = list(ids), mean) %>%
      st_cast(to = "POLYGON", warn = FALSE) # aggregate by ids and convert to POLYGON
    my_grid$Group.1 = NULL
  }

  df_ids = create_ids(n[1], n[2])

  my_grid = st_sf(data.frame(my_grid, df_ids))

  return(my_grid)
}

create_ids = function(num_c, num_r){
  m = 1
  m_row = 1
  m_col = 1

  result = data.frame(col = integer(length = num_r * num_c),
                      row = integer(length = num_r * num_c))

  for (i in seq_len(num_r)){
    for (j in seq_len(num_c)){
      result[m, "col"] = m_col
      result[m, "row"] = m_row
      m = m + 1
      m_col = m_col + 1
    }
    m_col = 1
    m_row = m_row + 1
  }
  return(result)
}

