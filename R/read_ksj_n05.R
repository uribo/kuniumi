#' Kokudosuuchi N05 parser
#' @inheritParams read_ksj_n03
#' @param .type File type. Select either "railroadSection" or "station"
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_n05 <- function(path = NULL, .year = NULL, .download = FALSE,
                         .type = c("railroadSection", "station")) {
  if (is.null(path)) {
    dl_zip <-
      zip_n05_url(.year)
    path <- download_ksj_zip(dl_zip, .download = .download)

    rlang::arg_match(.type)
    path <- switch(.type,
            railroadSection = grep("RailroadSection", path, value = TRUE),
            station = grep("Station", path, value = TRUE))

    if (sum(grepl("(.shp|.geojson)$", basename(path))) == 1L) {
      d <- st_read_crs6668(
        path = grep(".shp$", path, value = TRUE))
    } else if (sum(grepl("(.shp|.geojson)$", basename(path))) == 2L) {
      d <- sf::st_read(dsn = grep(".geojson$", path, value = TRUE),
                       as_tibble = TRUE,
                       stringsAsFactors = FALSE)
    }
  } else {
    if (grepl(".shp$", basename(path))) {
      d <- st_read_crs6668(path)
    } else if (grepl(".geojson$", basename(path))) {
      d <- sf::st_read(dsn = path,
                       as_tibble = TRUE,
                       stringsAsFactors = FALSE)
    }
  }
  d %>%
    purrr::set_names(dplyr::recode(names(d),
                                   N05_001 = "serviceProviderType",
                                   N05_002 = "railwayLineName", # lin
                                   N05_003 = "operationCompany", # opc
                                   N05_004 = "timePosition",
                                   N05_005b = "timePeriod_Begin",
                                   N05_005e = "timePeriod_End",
                                   N05_006 = "rfid",
                                   N05_007 = "trid",
                                   N05_008 = "trrm",
                                   N05_009 = "remark",
                                   N05_010 = "rfrm",
                                   N05_011 = "stationName"))
}
