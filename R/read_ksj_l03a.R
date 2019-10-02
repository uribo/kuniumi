#' Kokudosuuchi L03a parser
#' @inheritParams read_ksj_n03
#' @inheritParams read_ksj_a30a5
#' @description If there is no local file, specify year and meshcode to download.
#' @export
read_ksj_l03a <- function(path = NULL, .year = NULL, .meshcode = NULL, .download = FALSE) { # nolint
  if (is.null(path)) {
    dl_zip <-
      zip_l03a_url(.year, .meshcode, datum = 2)
    if (rlang::is_null(dl_zip))
      rlang::abort("Data does not match with the condition")
    path <- download_ksj_zip(dl_zip, .download = .download, source = "ksj")
  }
  if (stringr::str_detect(basename(path), "tky.(geojson|shp)")) {
    d <- sf::st_read(path,
                as_tibble = TRUE,
                crs = 4301,
                stringsAsFactors = FALSE,
                options = c("ENCODING=CP932"))
  } else {
    d <- st_read_crs4612(path,
                    options = c("ENCODING=CP932"))
  }
  xml_info <-
    list.files(gsub(pattern = "(.+)/.+$",
                    replacement = "\\1",
                    path),
               pattern = "^L03-a.+.xml",
               full.names = TRUE)
  if (length(xml_info) == 1L) {
    xml_info <-
      xml_info %>%
      xml2::read_xml() %>%
      xml2::as_list()
    d <-
      d %>%
      purrr::set_names(c("meshcode", "ta", "nouchi", "sinrin",
                         "kouch", "tatemono", "douro", "tetudou",
                         "other", "kasen", "kaihin", "kaisui",
                         "golf", "geometry"))
  }
  d
}
