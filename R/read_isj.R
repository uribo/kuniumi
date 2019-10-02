read_isj <- function(path = NULL,
                     .area_code = NULL, .fiscal_year = NULL, .pos_level = NULL,
                     .download = FALSE) {
  if (is.null(path)) {
    path <- isj_data_url(.area_code, fiscal_year = .fiscal_year) %>%
      dplyr::pull(zipFileUrl) %>% # nolint
      download_ksj_zip(.download = .download, source = "isj")
  }
  if (is.null(.pos_level)) {
    df <-
      data.table::fread(file = path) %>%
      as.data.frame()
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
  } else if (.pos_level == 1L) {
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
  df %>%
    dplyr::mutate_all(list(~ iconv(., from = "cp932", to = "utf8"))) %>%
    tibble::as_tibble()
}
