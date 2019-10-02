zip_p12_url <- function(pref_code = NULL) {
  if (is.null(pref_code))
    "http://nlftp.mlit.go.jp/ksj/gml/data/P12/P12-14/P12-14_GML.zip"
  else {
    pref_code <-
      sprintf("%02d", as.numeric(pref_code)) %>%
      jpndistrict:::prefcode_validate()
    glue::glue("http://nlftp.mlit.go.jp/ksj/gml/data/P12/P12-14/P12-14_{pref_code}_GML.zip") # nolint
  }
}

#' Kokudosuuchi P12 parser
#' @inheritParams read_ksj_n03
#' @param .type File type. When downloading a file, select the type of data to use, either "point", "line" or "polygon".
#' @description If there is no local file, specify prefecture code to download.
#' @export
read_ksj_p12 <- function(path = NULL, .pref_code = NULL, .download = FALSE,
                         .type = c("point", "line", "polygon")) {
  if (is.null(path)) {
    dl_zip <-
      zip_p12_url(.pref_code)
    path <- download_ksj_zip(dl_zip, .download = .download, source = "ksj")
    rlang::arg_match(.type)
    if (sum(file.exists(path)) == 3L)
      path <- switch(.type,
                     point   = grep("P12a", path, value = TRUE),
                     line    = grep("P12b", path, value = TRUE),
                     polygon = grep("P12c", path, value = TRUE))
  }
  st_read_crs4612(path,
                  options = c("ENCODING=CP932")) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, y = "\u2010") %>%
    purrr::set_names(
      c("number",
        "turismResorceName",
        "prefectureCode",
        "administartiveAreaCode",
        "turismResorceKindName",
        "address",
        "tourismResourceCategoryCode",
        "geometry"))
}
