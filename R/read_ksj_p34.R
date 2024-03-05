#' Kokudosuuchi P34 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_p34 <- function(path = NULL, .pref_code = NULL, .download = FALSE) {
  if (is.null(path)) {
    dl_zip <-
      zip_p34_url(.pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
  }
  d <-
    sf::st_read(
    path,
    options = c("ENCODING=CP932"),
    stringsAsFactors = FALSE,
    as_tibble = TRUE)
  d %>%
    purrr::set_names(dplyr::recode(names(d),
                                   P34_001 = "行政区域コード",
                                   P34_002 = "施設分類",
                                   P34_003 = "名称",
                                   P34_004 = "所在地")) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                                .fns = stringi::stri_trans_general,
                                id = "nfkc"))
}

zip_p34_url <- function(pref_code = NULL) {
  pref_code <-
    sprintf("%02d", as.numeric(pref_code)) %>%
    jpndistrict:::prefcode_validate()
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/P{file_code}/P{file_code}-{year_last2}/P{file_code}-{year_last2}_{pref_code}_GML.zip", # nolint
             file_code = "34",
             year_last2 = "14")
}
