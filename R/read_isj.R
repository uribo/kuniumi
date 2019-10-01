read_isj <- function(path = NULL, .area_code = NULL, .fiscal_year = NULL, .type = NULL, .download = FALSE) {
  if (is.null(path)) {
    path <- isj_data_url(.area_code, fiscal_year = .fiscal_year) %>%
      dplyr::pull(zipFileUrl) %>%
      download_ksj_zip(.download = .download)
  }
  if (is.null(.type)) {
    df <-
      data.table::fread(file = path) %>%
      as.data.frame()
    .type <- dplyr::case_when(
      ncol(df) >= 13L ~ "街区レベル",
      ncol(df) == 10L ~ "大字・町丁目レベル")
  }
  if (.type == "街区レベル") {
    if (ncol(df) == 13L) {
      df <-
        df %>%
        purrr::set_names(c("prefecture", "city",
                           "street_lv1",
                           "street_lv3",
                           "cs_num",
                           "coord_x", "coord_y",
                           "latitude", "longitude",
                           "住居表示フラグ", "代表フラグ",
                           "更新前履歴フラグ", "更新後履歴フラグ"))
    } else {
      df <-
        df %>%
        purrr::set_names(c("prefecture", "city",
                           "street_lv1",
                           "street_lv1b",
                           "street_lv3",
                           "cs_num",
                           "coord_x", "coord_y",
                           "latitude", "longitude",
                           "住居表示フラグ", "代表フラグ",
                           "更新前履歴フラグ", "更新後履歴フラグ"))
    }
    df <-
      df %>%
      dplyr::mutate_at(dplyr::vars(prefecture, city,
                                   tidyselect::num_range("street_lv", seq(1, 3))),
                       as.character) %>%
      dplyr::mutate_at(dplyr::vars(cs_num, coord_x, coord_y,
                                   latitude, longitude,
                                   `住居表示フラグ`, `代表フラグ`,
                                   `更新前履歴フラグ`,`更新後履歴フラグ`),
                       as.double)
  } else if (.type == "大字・町丁目レベル") {
    df <-
      df %>%
      purrr::set_names(c("prefecture_code", "prefecture",
                         "city_code", "city",
                         "street_lv1_code", # "大字_町丁目コード"
                         "street_lv1", # ... "大字_町丁目名"
                         "latitude", "longitude",
                         "原典資料コード",
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
