#' Read a geoPAT 2.0 text output
#'
#' Read a text output of the geoPAT 2.0 functions into R
#'
#' @param x A filepath to the geoPAT 2.0 text file
#'
#' @return data.frame
#'
#' @importFrom readr read_lines
#' @importFrom stringr str_extract
#' @importFrom stats as.dist
#' @importFrom utils read.table
#'
#' @examples
#' polygon_filepath = system.file("rawdata/Augusta2011_polygon.txt", package = "rgeopat2")
#' my_polygon = gpat_read_txt(polygon_filepath)
#'
#' points_filepath = system.file("rawdata/Augusta2011_points.txt", package = "rgeopat2")
#' my_points = gpat_read_txt(points_filepath)
#'
#' @export
gpat_read_txt = function(x){
  lines = read_lines(x, progress = FALSE)
  obj_name = str_extract(lines, '"([^"]*)"') %>% str_extract("\\(?[0-9,.]+\\)?")
  clean_lines = lines %>%
    gsub("(?<=\\[)(.*)(?=>)", "", ., perl = TRUE) %>%
    gsub("\\[> ", "", ., perl = TRUE)
  n_cols = strsplit(clean_lines[[1]], split = ",") %>%
    unlist() %>%
    length()
  p = read.table(text = clean_lines, sep = ",")
  p$name = as.numeric(obj_name)
  return(p)
}
