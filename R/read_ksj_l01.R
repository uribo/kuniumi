#' Kokudosuuchi L01 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_l01 <- function(path = NULL, .year = NULL, .pref_code = NULL, .download = FALSE) { # nolint
  if (is.null(path)) {
    dl_zip <-
      zip_l01_url(.year, .pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
    if (sum(grepl("(.shp|.geojson)$", basename(path))) == 1L) {
      d <- sf::st_read(
        dsn = grep(".shp$",
                   path,
                   value = TRUE),
        options = c("ENCODING=CP932"),
        as_tibble = TRUE,
        stringsAsFactors = FALSE)
    } else if (sum(grepl("(.shp|.geojson)$", basename(path))) == 2L) {
      d <- sf::st_read(dsn = grep(".geojson$",
                                  path,
                                  value = TRUE),
                       as_tibble = TRUE,
                       stringsAsFactors = FALSE)
    }
  } else {
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
  }
  d %>%
    kokudosuuchi::translateKSJData()
}
