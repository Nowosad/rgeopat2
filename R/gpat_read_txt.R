#' Read a geoPAT 2.0 text output
#'
#' Read a text output of the geoPAT 2.0 functions into R
#'
#' @param x A filepath to the geoPAT 2.0 text file
#' @param signature A signature used to create a geoPAT 2.0 text output (supported signatures: "lind", "linds")
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
#' lind_filepath = system.file("rawdata/Augusta2011_lind.txt", package = "rgeopat2")
#' my_lind = gpat_read_txt(lind_filepath, signature = "lind")
#'
#' linds_filepath = system.file("rawdata/Augusta2011_linds.txt", package = "rgeopat2")
#' my_linds = gpat_read_txt(linds_filepath, signature = "linds")
#'
#' @export
gpat_read_txt = function(x, signature = NULL){
  df = suppressMessages(read_delim(x, delim = ",", col_names = FALSE, progress = FALSE))
  obj_name = str_extract(df$X2, '"([^"]*)"') %>% str_extract("\\(?[0-9,.]+\\)?")
  clean_first_col = df$X2 %>%
    gsub("(?)(.*)(?=>)", "", ., perl = TRUE) %>%
    gsub("\\> ", "", ., perl = TRUE) %>%
    as.numeric() %>%
    data.frame(X1 = ., stringsAsFactors = FALSE)
  df = cbind(clean_first_col, df[- c(1, 2)])
  if (is.null(signature)){
    names(df) = paste0("X", seq_along(df))
  } else if (signature == "lind"){
    n = (length(df) - length(landscape_level)) / length(class_level)
    names(df) = c(landscape_level, paste0(rep(class_level, each = n), "_", seq_len(n)))
  } else if (signature == "linds"){
    n = (length(df) - length(landscape_level)) / length(class_level)
    names(df) = c(landscape_level, paste0("pland", "_", seq_len(n)))
  } else {
    stop("This signature is not supported")
  }
  df$name = as.numeric(obj_name)
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
