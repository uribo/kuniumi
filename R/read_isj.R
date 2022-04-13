#' Read ISJ format data
#' @param path Input file path (`csv`)
#' @param .area_code Area code. 5 digits numeric character.
#' E.g. Chiba prefecture ("12000"), Ichikawa city in Chiba ("12033")
#' @param .fiscal_year Japanese imperial fiscal year as Kanji.
#' @param .pos_level E.g. 0 is to street level 3. 1 is to street level 1.
#' @param .download If `TRUE`, download file to working directory.
#' @param return_class Select "tbl_df" (data.frame) or "data.table"
#' @seealso \url{https://nlftp.mlit.go.jp/isj/about_api.html}
#' @export
read_isj <- function(path = NULL,
                     .area_code = NULL, .fiscal_year = NULL, .pos_level = NULL,
                     .download = FALSE, return_class = NULL) {
  . <- prefecture <- city <- cs_num <- coord_x <- coord_y <- zipFileUrl <- NULL
  latitude <- longitude <- flag_residence <- flag_represent <- NULL
  flag_history_before <- flag_history_after <- city_code <- NULL
  street_lv1_code <- street_lv1 <- street_levels <- NULL
  return_class <- rlang::arg_match(return_class,
                                   c("tbl_df", "data.table"))
  if (is.null(path)) {
    path <- isj_data_url(.area_code, fiscal_year = .fiscal_year, pos_level = .pos_level) %>%
      dplyr::pull(zipFileUrl) %>% # nolint
      download_ksj_zip(.download = .download, source = "isj")
  }
  df <-
    data.table::fread(file = path) %>%
    purrr::set_names(iconv(names(.), from = "cp932", to = "utf8"))
  if (is.null(.pos_level)) {
    .pos_level <- dplyr::case_when(
      ncol(df) >= 13L ~ 0L,
      ncol(df) == 10L ~ 1L)
  }
  if (.pos_level == 0L) {
    if (ncol(df) == 13L) {
      df <-
        df %>%
        purrr::set_names(c("prefecture",
                           "city",
                           "street_lv1",
                           "street_lv3",
                           "cs_num",
                           "coord_x",
                           "coord_y",
                           "latitude",
                           "longitude",
                           "flag_residence",
                           "flag_represent",
                           "flag_history_before",
                           "flag_history_after"))
    } else {
      df <-
        df %>%
        purrr::set_names(c("prefecture", "city",
                           "street_lv1",
                           "street_lv1b",
                           "street_lv3",
                           "cs_num",
                           "coord_x",
                           "coord_y",
                           "latitude",
                           "longitude",
                           "flag_residence",
                           "flag_represent",
                           "flag_history_before",
                           "flag_history_after"))
    }
    df <-
      df %>%
      dplyr::mutate_at(dplyr::vars(prefecture,
                                   city,
                                   tidyselect::num_range("street_lv",
                                                         seq(1, 3))),
                       as.character) %>%
      dplyr::mutate_at(dplyr::vars(cs_num, coord_x, coord_y,
                                   latitude, longitude,
                                   flag_residence,
                                   flag_represent,
                                   flag_history_before,
                                   flag_history_after),
                       as.double)
  }
  if (.pos_level == 1L) {
    df <-
      df %>%
      purrr::set_names(c("prefecture_code", "prefecture",
                         "city_code", "city",
                         "street_lv1_code",
                         "street_lv1",
                         "latitude",
                         "longitude",
                         "source_code",
                         "street_levels")) %>%
      dplyr::select(prefecture, city_code, city,
             street_lv1_code, street_lv1,
             longitude, latitude, street_levels) %>%
      dplyr::mutate_at(dplyr::vars(prefecture, city_code, city,
                     street_lv1_code, street_lv1,
                     street_levels),
                as.character) %>%
      dplyr::mutate(city_code = sprintf("%05s", city_code),
                    street_lv1_code = sprintf("%012s", street_lv1_code)) %>%
      dplyr::mutate_at(dplyr::vars(longitude, latitude), as.double)
  }
  df <-
    df %>%
    dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
    dplyr::mutate_all(list(~ iconv(., from = "cp932", to = "utf8")))
  if (return_class == "tbl_df") {
    tibble::as_tibble(df)
  } else if (return_class == "data.table") {
    data.table::as.data.table(df)
  }
}
