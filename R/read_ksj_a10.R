#' Kokudosuuchi A10 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_a10 <- function(path = NULL, .pref_code = NULL, .year = NULL, .download = FALSE) {
  if (is.null(path)) {
    path <-
      zip_a10_url(.pref_code, .year) %>%
      download_ksj_zip(.download = .download, source = "ksj")
  }
  path %>%
    purrr::map(
      ~ st_read_crs4612(.x,
                        options = c("ENCODING=CP932"))
    ) %>%
    purrr::reduce(rbind) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_transform(crs = 6668)
}

zip_a10_url <- function(area_code, year) {
  area_code <-
    check_area_code(area_code)
  year <-
    as.character(year)
  year <-
    rlang::arg_match(year,
                     c("2006", "2010", "2011", "2015"))

  if (area_code %in% c("00") & year %in% c("2006", "2011", "2015")) {
    rlang::abort("mismatch")
  }
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/{id}/{id}-{yy}/{id}-{yy}_{area_code}_GML.zip",
             id = "A10",
             yy = substr(year, 3, 4))
}
