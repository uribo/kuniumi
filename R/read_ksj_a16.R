#' Kokudosuuchi A10 parser
#' @inheritParams read_ksj_n03
#' @param translate Translate the variable name into the specified language.
#' You can choose either Original (`raw`) and Japanese (`jp`) or English (`en`).
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_a16 <- function(path = NULL, translate = "jp", .pref_code = NULL, .year = NULL, .download = TRUE) {
  if (is.null(path)) {
    dl_zip <-
      zip_a16_url(.pref_code, .year)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
  }
  if (sum(grepl(".shp$", basename(path))) >= 1L) {
    path <-
      path %>%
      stringr::str_subset("shp$")
    d <-
      st_read_crs6668(path)
  } else if (sum(grepl(".geojson$", basename(path))) >= 1L) {
    path <-
      path %>%
      stringr::str_subset("geojson$")
    d <-
      sf::st_read(dsn = path,
                  as_tibble = TRUE,
                  stringsAsFactors = FALSE) %>%
      sf::st_transform(crs = 6668)
  }
    lang <-
      rlang::arg_match(translate,
                       c("raw", "jp", "en"))
    if (lang != "raw") {
      if (lang == "jp") {
        d <-
          d %>%
          kokudosuuchi::translateKSJData()
      } else {
        d <-
          d %>%
          purrr::set_names(c("didId", "administrativeAreaCode", "cityName", "didCode",
                             "population", "area", "previousPopulation", "previousArea",
                             "populationRatio", "areaRatio", "censusYear", "geometry"))
      }
    }
d
}

zip_a16_url <- function(area_code, year) {
  area_code <- sprintf("%02d", area_code)
  year <- as.character(year)

  area_code <- rlang::arg_match(area_code,
                                sprintf("%02d", seq.int(0, 47)))
  year <- rlang::arg_match(year,
                           seq.int(1960, 2015, by = 5) %>%
                             as.character())

  if (area_code %in% c("00", "47") & year %in% c("1960", "1965") | area_code == "00" & year %in% as.character(seq.int(1970, 2005, by = 5))) {
    rlang::abort("mismatch")
  }
  glue::glue("http://nlftp.mlit.go.jp/ksj/gml/data/{id}/{id}-{yy}/{id}-{yy}_{area_code}_GML.zip",
             id = "A16",
             yy = substr(year, 3, 4))
}
