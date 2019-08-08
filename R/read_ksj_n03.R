#' Kokudosuuchi N03 parser
#' @importFrom utils download.file unzip
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
      path <- dplyr::if_else(.download == TRUE,
                             ".",
                             tempdir())
      zip_path <-
        paste0(path, "/", basename(dl_zip))
      dl_zip %>%
        download.file(zip_path)
      path <- paste0(path, "/", gsub(".zip", "", basename(dl_zip)))
      dir.create(path)
      unzip(zipfile = zip_path,
            exdir = path)
      path <-
        grep("(shp|geojson)$",
             list.files(path,
                        full.names = TRUE,
                        recursive = TRUE),
             value = TRUE)
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
    xml_info <-
      list.files(
        gsub(pattern = "(.+)/.+$",
             replacement = "\\1",
             path),
        pattern = paste0("^N03.+",
                         gsub(basename(path),
                              "..+", ""),
                         ".xml$"),
        full.names = TRUE)
    if (length(xml_info) == 1) {
      xml_info <-
        xml_info %>%
        xml2::read_xml() %>%
        xml2::as_list()
      d <-
        d %>%
        purrr::set_names(names(xml_info$Dataset$AdministrativeBoundary)[2:6], # nolint
                         attr(d, "sf_column"))
    }
    d
}
