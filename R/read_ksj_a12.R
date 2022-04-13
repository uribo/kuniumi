zip_a09 <- function(year, pref_code = NULL) {
  year <- rlang::arg_match(year,
                           values = c("2006", "2011", "2018"))
  pref_code <-
    sprintf("%02d", as.numeric(pref_code)) %>%
    jpndistrict:::prefcode_validate()
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/A09/A09-{yy}/A09-{yy}_{pref_code}_GML.zip", # nolint
             yy = substr(year, 3, 4))
}

read_ksj_a09 <- function(path = NULL,
                         .year = NULL, .pref_code = NULL, .download = FALSE) {
  if (is.null(path)) {
    dl_zip <-
      zip_a09(.year, .pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
  }
  path
}

zip_a12_url <- function(year, pref_code = NULL) {
  year <-
    as.character(year)
  year <- rlang::arg_match(year,
                           c("2006", "2011", "2015"))
  pref_code <-
    sprintf("%02d", as.numeric(pref_code)) %>%
    jpndistrict:::prefcode_validate()
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/A12/A12-{yy}/A12-{yy}_{pref_code}_GML.zip", # nolint
             yy = substr(year, 3, 4))
}

read_ksj_a12 <- function(path = NULL,
                         .year = NULL, .pref_code = NULL, .download = FALSE) {
  if (is.null(path)) {
    dl_zip <-
      zip_a12_url(.year, .pref_code)
    path <- download_ksj_zip(dl_zip, .download = .download)
  }
  st_read_crs4612(path)
}


# read_ksj_a12(.year = "2006", .pref_code = 8, .download = TRUE)

# d1 <- st_read("~/Documents/resources/国土数値情報/A12/2015/A12-15_08_GML/a001080020160205.shp")
# d2 <- st_read("~/Documents/resources/国土数値情報/A12/2015/A12-15_08_GML/a001080020160206.shp")
#
# mapview::mapview(d2) +
#   mapview::mapview(d1)

# seq.int(47) %>%
#   purrr::walk(
#     ~ read_ksj_a09(.year = "2011", .pref_code = .x, .download = TRUE)
#   )

# st_read("~/Documents/resources/国土数値情報/A09/2011/A12-11_08_GML/A12-11_08_GML/a001080020120306.shp")
