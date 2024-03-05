zip_p12_url <- function(pref_code = NULL) {
  if (is.null(pref_code))
    "https://nlftp.mlit.go.jp/ksj/gml/data/P12/P12-14/P12-14_GML.zip"
  else {
    glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/P12/P12-14/P12-14_{pref_code}_GML.zip", # nolint
               pref_code = make_prefcode(pref_code))
  }
}

#' Kokudosuuchi P12 parser
#' @inheritParams read_ksj_a16
#' @param .type File type. When downloading a file, select the type of data to use, either "point", "line" or "polygon".
#' @description If there is no local file, specify prefecture code to download.
#' @export
read_ksj_p12 <- function(path = NULL, translate = "jp",
                         .pref_code = NULL, .download = FALSE,
                         .type) {
  if (is.null(path)) {
    dl_zip <-
      zip_p12_url(.pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
    .type <-
      rlang::arg_match(.type,
                       c("point", "line", "polygon"))
    if (sum(file.exists(path)) == 3L)
      path <- switch(.type,
                     point   = grep("P12a", path, value = TRUE),
                     line    = grep("P12b", path, value = TRUE),
                     polygon = grep("P12c", path, value = TRUE))
  }
  lang <-
    rlang::arg_match(translate,
                     c("raw", "jp", "en"))
  d <-
    st_read_crs4612(path,
                  options = c("ENCODING=CP932")) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, y = "\u2010")
  if (lang == "jp") {
    d <-
      d %>%
      kokudosuuchi::translateKSJData()
  } else if (lang == "en") {
    d <-
      d %>%
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
  d
}
