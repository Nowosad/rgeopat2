#' Read a geoPAT distance matrix
#'
#' Read a geoPAT distance matrix into R
#'
#' @param x A filepath to the geoPAT 2.0 distance matrix file
#'
#' @return dist
#'
#' @importFrom readr read_csv
#' @importFrom stats as.dist
#'
#' @examples
#' distmtx_filepath = system.file("rawdata/Augusta2011_matrix_grid.csv", package="rgeopat2")
#' my_distmtx = gpat_read_distmtx(distmtx_filepath)
#'
#' @export

gpat_read_distmtx = function(x){
  as.dist(suppressMessages(read_csv(x, progress = FALSE))[, -1])
}
