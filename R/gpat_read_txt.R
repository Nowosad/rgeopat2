#' Read a geoPAT 2.0 text output
#'
#' Read a text output of the geoPAT 2.0 functions into R
#'
#' @param x A filepath to the geoPAT 2.0 text file
#' @param signature A signature used to create the geoPAT 2.0 text output (supported signatures: "lind", "linds")
#'
#' @return data.frame
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_detect str_extract str_replace_all str_split
#'
#' @examples
#' polygon_filepath = system.file("rawdata/Augusta2011_polygon.txt", package = "rgeopat2")
#' my_polygon = gpat_read_txt(polygon_filepath)
#'
#' points_filepath = system.file("rawdata/Augusta2011_points.txt", package = "rgeopat2")
#' my_points = gpat_read_txt(points_filepath)
#'
#' lind_filepath = system.file("rawdata/Augusta2011_lind.txt", package = "rgeopat2")
#' my_lind = gpat_read_txt(lind_filepath, signature = "lind")
#'
#' linds_filepath = system.file("rawdata/Augusta2011_linds.txt", package = "rgeopat2")
#' my_linds = gpat_read_txt(linds_filepath, signature = "linds")
#'
#' grid_filepath = system.file("rawdata/Augusta2011_grid100.txt", package = "rgeopat2")
#' my_grid = gpat_read_txt(grid_filepath)
#'
#' gridlinds_filepath = system.file("rawdata/Augusta2011_grid_linds.txt", package = "rgeopat2")
#' my_grid = gpat_read_txt(gridlinds_filepath, signature = "linds")
#'
#' @export
gpat_read_txt = function(x, signature = NULL){
  df = suppressMessages(read_delim(x, delim = ",", col_names = FALSE, progress = FALSE))
  obj_desc = str_extract(df$X2, '"([^"]*)"') %>% str_replace_all('\"', "")
  clean_first_col = df$X2 %>%
    gsub("(?)(.*)(?=>)", "", ., perl = TRUE) %>%
    gsub("\\> ", "", ., perl = TRUE) %>%
    as.numeric() %>%
    data.frame(X1 = ., stringsAsFactors = FALSE)
  df = cbind(clean_first_col, df[-c(1, 2)])
  if (is.null(signature)){
    names(df) = paste0("X", seq_along(df))
  } else if (signature == "lind"){
    n = (length(df) - length(landscape_level)) / length(class_level)
    names(df) = c(landscape_level, paste0(rep(class_level, each = n), "_", seq_len(n)))
  } else if (signature == "linds"){
    n = (length(df) - length(landscape_level))
    names(df) = c(landscape_level, paste0("pland", "_", seq_len(n)))
  }
  if (str_detect(obj_desc[1], "cat")){
    obj_name = str_extract(obj_desc, "\\(?[0-9,.]+\\)?") %>% as.numeric
    df$cat = obj_name
  } else if (str_detect(obj_desc[1], "loc")){
    obj_name = str_extract(obj_desc, "\\(?[0-9,.]+\\)?") %>% as.numeric
    df$loc = obj_name
  } else{
    obj_desc = str_split(obj_desc, "_", simplify = TRUE)
    col_n = obj_desc[, ncol(obj_desc) - 1]
    row_n = obj_desc[, ncol(obj_desc)]
    df$col = as.numeric(col_n) + 1
    df$row = as.numeric(row_n) + 1
  }
  return(df)
}

landscape_level = c("lpi", "ed", "area_mn", "area_am", "area_md", "area_ra", "area_sd",
                    "area_cv", "gyrate_mn", "gyrate_am", "gyrate_md", "gyrate_ra",
                    "gyrate_sd", "gyrate_cv", "pafrac", "para_mn", "para_am", "para_md",
                    "para_ra", "para_sd", "para_cv", "shape_mn", "shape_am", "shape_md",
                    "shape_ra", "shape_sd", "shape_cv", "frac_mn", "frac_am", "frac_md",
                    "frac_ra", "frac_sd", "frac_cv", "contig_mn", "contig_am", "contig_md",
                    "contig_ra", "contig_sd", "contig_cv", "contag", "iji", "pladj",
                    "ai", "lsi", "cohesion", "pd", "split", "division", "mesh", "prd",
                    "rpr", "shdi", "sidi", "msidi", "shei", "siei", "msiei")

class_level = c("pland", "lpi", "ed", "area_mn", "area_am", "area_md", "area_ra",
                "area_sd", "area_cv", "gyrate_mn", "gyrate_am", "gyrate_md",
                "gyrate_ra", "gyrate_sd", "gyrate_cv", "pafrac", "para_mn", "para_am",
                "para_md", "para_ra", "para_sd", "para_cv", "shape_mn", "shape_am",
                "shape_md", "shape_ra", "shape_sd", "shape_cv", "frac_mn", "frac_am",
                "frac_md", "frac_ra", "frac_sd", "frac_cv", "contig_mn", "contig_am",
                "contig_md", "contig_ra", "contig_sd", "contig_cv", "iji", "pladj",
                "ai", "lsi", "cohesion", "pd", "split", "division", "mesh")
