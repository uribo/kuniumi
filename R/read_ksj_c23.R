zip_c23_url <- function(pref_code) {
  pref_code <-
    make_prefcode(pref_code)
  if (pref_code %in% c("09", "10", "11", "19", "20", "21",
                       "25", "29")) {
    rlang::abort("There is no target prefecture data.")
  } else {
    glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/C23/C23-06/C23-06_{pref_code}_GML.zip") # nolint
  }
}

#' Kokudosuuchi C23 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify prefecture code to download.
#' @export
read_ksj_c23 <- function(path = NULL, .pref_code = NULL, .download = FALSE) {
  if (is.null(path)) {
    dl_zip <-
      zip_c23_url(.pref_code)
    path <- download_ksj_zip(dl_zip, .download = .download, source = "ksj")
  }
  d <-
    st_read_crs4612(path) %>%
    dplyr::mutate(C23_007 = dplyr::case_when(
      C23_007 == "false" ~ FALSE,
      C23_007 == "true" ~ TRUE))
  xml_info <-
    list.files(gsub(pattern = "(.+)/.+$",
                    replacement = "\\1",
                    path),
               pattern = "^C23-06.+.xml",
               full.names = TRUE)
  if (length(xml_info) == 1) {
    xml_info <-
      xml_info %>%
      xml2::read_xml() %>%
      xml2::as_list()
    d <-
      d %>%
      purrr::set_names(
        c("administrativeAreaCode", "competentAuthorities", "areaNumber",
          "areaName", "administrator", "administratorname", "branchingBay",
          attr(d, "sf_column"))) %>%
      dplyr::mutate_if(is.character,
                       stringi::stri_trans_general,
                       id = "nfkc")
  }
  d
}
