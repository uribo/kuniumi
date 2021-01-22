#' Kokudosuuchi L02 parser (v2.7)
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year and pref_code to download.
#' @import tidyselect
#' @export
read_ksj_l02 <- function(path = NULL, .year = NULL, .pref_code = NULL, .download = FALSE) { # nolint
  # L01_006 <- L01_023 <- L01_054 <- NULL
  if (is.null(path)) {
    dl_zip <-
      zip_l02_url(.year, .pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
    if (sum(grepl("(.shp|.geojson)$", basename(path))) == 1L) {
      d <-
        sf::st_read(
        dsn = grep(".shp$",
                   path,
                   value = TRUE),
        options = c("ENCODING=CP932"),
        as_tibble = TRUE,
        stringsAsFactors = FALSE)
    } else if (sum(grepl("(.shp|.geojson)$", basename(path))) == 2L) {
      d <-
        sf::st_read(dsn = grep(".geojson$",
                               path,
                               value = TRUE),
                       as_tibble = TRUE,
                       stringsAsFactors = FALSE)
    }
  } else {
    if (grepl(".shp$", basename(path))) {
      d <- sf::st_read(
        dsn = path,
        options = c("ENCODING=CP932"),
        as_tibble = TRUE,
        stringsAsFactors = FALSE)
    } else if (grepl(".geojson$", basename(path))) {
      d <-
        sf::st_read(dsn = path,
                       as_tibble = TRUE,
                       stringsAsFactors = FALSE)
    }
  }
  common_vars <-
    c(paste("基準地コード",
            c("見出し番号",
              "一連番号"),
            sep = "_"),
      paste("前年度基準地コード",
            c("見出し番号",
              "一連番号"),
            sep = "_"),
      "年度",
      # "地点",
      "調査価格",
      paste("属性移動",
            c("選定状況",
              "住居漢字",
              "地積",
              "利用の現況",
              "建物構造",
              "供給施設",
              "駅からの距離",
              "用途区分",
              "防火区分",
              "都市計画区分",
              "森林区分",
              "公園区分",
              "建ぺい率",
              "容積率"),
            sep = "_"),
      "基準地行政区域コード",
      "基準地市区町村名称",
      "住居表示",
      "地積",
      "利用現況",
      "利用状況表示",
      "建物構造",
      "供給施設有無_水道",
      "供給施設有無_ガス",
      "供給施設有無_下水",
      "形状",
      "間口比率",
      "奥行比率",
      "地上階層",
      "地下階層",
      "前面道路状況",
      "前面道路の方位",
      "前面道路の幅員",
      "前面道路の舗装状況",
      "側道状況",
      "側道の方位",
      "周辺の土地利用の状況",
      "駅名",
      "駅からの距離",
      "用途区分",
      "防火区分",
      "都市計画区分",
      "森林区分",
      "公園区分",
      "建ぺい率",
      "容積率",
      "共通地点",
      "選定年次ビット")
  status_vars <-
    c(paste("調査価格",
            seq.int(
              zipangu::convert_jyear("昭和58年"),
              zipangu::convert_jyear("令和2年")
            ),
            sep = "_"),
      paste("属性移動",
            seq.int(
              zipangu::convert_jyear("昭和59年"),
              zipangu::convert_jyear("令和2年")
            ),
            sep ="_"))
  d %>%
    purrr::set_names(c(common_vars,
                       status_vars,
                       "geometry")) %>%
    dplyr::mutate(dplyr::across(where(is.character),
                  .fns = ~dplyr::na_if(.x, "_"))) %>%
    dplyr::mutate(dplyr::across(.cols = c(tidyselect::starts_with("属性移動"),
                                          tidyselect::starts_with("供給施設有無")),
                  .fns = ~dplyr::case_when(.x == "false" ~ FALSE,
                                    .x == "true" ~ TRUE)))
}
