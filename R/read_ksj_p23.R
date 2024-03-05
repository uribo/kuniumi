zip_p23_url <- function(pref_code) {
  pref_code <-
    make_prefcode(pref_code)
  if (length(pref_code[pref_code %in% c("09", "10", "11", "19", "20", "21", "25", "29")]) > 0) # nolint
    rlang::abort("There is no target prefecture data.")
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/P23/P23-12/P23-12_{pref_code}_GML.zip") # nolint
}

#' Kokudosuuchi P23 parser
#' @inheritParams read_ksj_a16
#' @param .type File type. When downloading a file, select the type of data to use, either "point" or "line".
#' @description If there is no local file, specify prefecture code to download.
#' @export
read_ksj_p23 <- function(path = NULL, translate = "jp", .pref_code = NULL, .download = FALSE,
                         .type) {
  if (is.null(path)) {
    dl_zip <-
      zip_p23_url(.pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
    type <-
      rlang::arg_match(.type,
                     c("point", "line"))
    if (sum(file.exists(path)) == 2L)
      path <- switch(.type,
                     point = grep("P23a", path, value = TRUE),
                     line = grep("P23b", path, value = TRUE))
  }
  lang <-
    rlang::arg_match(translate,
                     c("raw", "jp", "en"))
  d <-
    st_read_crs4612(path)
  if (lang == "jp") {
    d <-
      d %>%
      kokudosuuchi::translateKSJData()
  } else if (lang == "en") {
    d <-
      d %>%
      purrr::set_names(c(
        "administrativeAreaCode",
        "competentAuthority",
        "administrator",
        paste0("facilityType_",
               c("bank",
                 "groin",
                 "bankProtection",
                 "breastWall",
                 "offshoreBreakwater",
                 "sandyBeach",
                 "otherFacilities")),
        "length",
        "baseLevel",
        "copeLevelMaxPresent",
        "copeLevelMinPresent",
        "copeLevelMaxPlan",
        "copeLevelMinPlan",
        "geometry"
      ))
  }
  d %>%
    purrr::modify_at(seq.int(4, 10),
                     ~ dplyr::case_when(
                       .x == "f" ~ FALSE,
                       .x == "t" ~ TRUE))
}
