#' Kokudosuuchi P13 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_p13 <- function(path = NULL, .pref_code = NULL, .download = FALSE) {
  if (is.null(path)) {
    path <-
      zip_p13_url(.pref_code) %>%
      download_ksj_zip(.download = .download,
                                 source = "ksj")
  }
  d <-
    sf::st_read(dsn = grep(".shp$", path, value = TRUE),
                options = c("ENCODING=CP932"),
                as_tibble = TRUE,
                stringsAsFactors = FALSE)
  d %>%
    purrr::set_names(dplyr::recode(names(d),
                                   P13_001 = "管理都道府県・整備局",
                                   P13_002 = "管理市区町村",
                                   P13_003 = "公園名",
                                   P13_004 = "公園種別",
                                   P13_005 = "所在地都道府県名",
                                   P13_006 = "所在地市区町村名",
                                   P13_007 = "供用開始年",
                                   P13_008 = "供用済面積",
                                   P13_009 = "都市計画決定",
                                   P13_010 = "備考")) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                                .fns = stringi::stri_trans_general,
                                id = "nfkc"))
}

zip_p13_url <- function(pref_code) {
  pref_code <-
    sprintf("%02d", as.numeric(pref_code)) %>%
    jpndistrict:::prefcode_validate()
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/P13/P13-11/P13-11_{pref_code}_GML.zip")
}
