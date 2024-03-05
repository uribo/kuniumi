zip_p07_url <- function(year, pref_code) {
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           values = c("2010", "2015"))
  glue::glue(
    "https://nlftp.mlit.go.jp/ksj/gml/data/P07/P07-{year_dir}/P07-{year_dir}_{pref_code}_GML.zip", # nolint
    year_dir = substr(year, 3L, 4L),
    pref_code = make_prefcode(pref_code))
}

#' Kokudosuuchi P07 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_p07 <- function(path = NULL, .year = NULL, .pref_code = NULL, .download = FALSE) {
  if (is.null(path)) {
    dl_zip <-
      zip_p07_url(year = .year, pref_code = .pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download)
  }
  d <-
    st_read_crs6668(path)
  xml_info <-
    list.files(gsub(pattern = "(.+)/.+$",
                    replacement = "\\1",
                    path),
               pattern = "^P07-.+.xml",
               full.names = TRUE)
  if (length(xml_info) == 1L) {
    # xml_info <-
    #   xml_info %>%
    #   xml2::read_html() %>%
    #   xml2::as_list()
    # xml_info$html$body$dataset$fuelfillstation %>% names()
    d <-
      d %>%
      purrr::set_names(c("fuelstoreclassification", "address", "geometry"))
  }
  d
}
