#' Read a geoPAT 2.0 text output
#'
#' Read a text output of the geoPAT 2.0 functions into R
#'
#' @param x A filepath to the geoPAT 2.0 text file
#'
#' @return data.frame
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_extract
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
  df = suppressMessages(read_delim(x, delim = ",", col_names = FALSE, progress = FALSE))
  obj_name = str_extract(df$X2, '"([^"]*)"') %>% str_extract("\\(?[0-9,.]+\\)?")
  clean_first_col = df$X2 %>%
    gsub("(?)(.*)(?=>)", "", ., perl = TRUE) %>%
    gsub("\\> ", "", ., perl = TRUE) %>%
    as.numeric() %>%
    data.frame(X1 = ., stringsAsFactors = FALSE)

  df = cbind(clean_first_col, df[- c(1, 2)])
  names(df) = paste0("X", seq_along(df))
  df$name = as.numeric(obj_name)
  return(df)
}
