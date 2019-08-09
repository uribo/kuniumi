#' Kokudosuuchi N03 parser
#' @param path Input file path (`.shp` or `.geojson`)
#' @param .year Specific year
#' @param .pref_code Specific prefecture code
#' @param .download If `true`, the specified file is downloaded to the current working directory.
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_n03 <- function(path = NULL,
                         .year = NULL, .pref_code = NULL, .download = FALSE) {
    if (is.null(path)) {
      dl_zip <-
        zip_n03_url(.year, .pref_code)
      path <- download_ksj_zip(dl_zip, .download = .download)
    }
    if (grepl(".shp$", basename(path))) {
      d <- sf::st_read(
        dsn = path,
        options = c("ENCODING=CP932"),
        as_tibble = TRUE,
        stringsAsFactors = FALSE)
    } else if (grepl(".geojson$", basename(path))) {
      d <- sf::st_read(dsn = path,
                       as_tibble = TRUE,
                       stringsAsFactors = FALSE)
    }
  d %>%
        purrr::set_names(dplyr::recode(names(d),
                                       N03_001 = "prefectureName",
                                       N03_002 = "subPrefectureName",
                                       N03_003 = "countyName",
                                       N03_004 = "cityName",
                                       N03_005 = "formationDate",
                                       N03_006 = "disappearanceDate",
                                       N03_007 = "administrativeAreaCode")) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::ends_with("Date")),
                     as.Date)
}

#' Kokudosuuchi N02 parser
#' @inheritParams read_ksj_n03
#' @param .type The type of file. "Railway section (railroadsection)" (default) or "station"
#' @description If there is no local file, specify the year to download.
#' @export
read_ksj_n02 <- function(path = NULL,
                         .year = NULL, .download = NULL,
                         .type = c("railroadsection", "station")) {
  if (is.null(path)) {
    dl_zip <-
      zip_n02_url(.year)
    path <- download_ksj_zip(dl_zip, .download = .download)
    .type <- rlang::arg_match(.type)
    path <- dplyr::if_else(.type == "station",
                           grep("Station", path, value = TRUE),
                           grep("RailroadSection", path, value = TRUE))
  }
  if (grepl(".shp$", basename(path))) {
    d <- sf::st_read(
      dsn = path,
      options = c("ENCODING=CP932"),
      as_tibble = TRUE,
      stringsAsFactors = FALSE)
  } else if (grepl(".geojson$", basename(path))) {
    d <- sf::st_read(dsn = path,
                     as_tibble = TRUE,
                     stringsAsFactors = FALSE)
  }
  d %>%
    purrr::set_names(dplyr::recode(names(d),
                                   N02_001 = "railwayType",
                                   N02_002 = "serviceProviderType",
                                   N02_003 = "railwayLineName",
                                   N02_004 = "operationCompany",
                                   N02_005 = "stationName"))
}
