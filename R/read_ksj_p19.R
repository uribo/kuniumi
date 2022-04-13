zip_p19_url <- function(pref_code = NULL) {
  pref_code <-
    sprintf("%02d", as.numeric(pref_code)) %>%
    jpndistrict:::prefcode_validate()
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/P19/P19-12/P19-12_{pref_code}_GML.zip") # nolint
}

#' Kokudosuuchi P19 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify prefecture code to download.
#' @export
read_ksj_p19 <- function(path = NULL, .pref_code = NULL, .download = FALSE) {
  if (is.null(path)) {
    dl_zip <-
      zip_p19_url(.pref_code)
    path <- download_ksj_zip(dl_zip, .download = .download, source = "ksj")
  }
  st_read_crs4612(path) %>%
    purrr::set_names(
      c("aRegionResourcesId",
        "prefectureCode",
        "prefectureName",
        "typeCode",
        "typeName",
        "vistaResourcesClassCode",
        "vistaResourcesClassName",
        "vistaResourcesName",
        "geometry")
    )
}
