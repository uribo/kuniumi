#' Kokudosuuchi L01 parser (v2.5 and v2.4)
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year and pref_code to download.
#' @export
read_ksj_l01 <- function(path = NULL, .year = NULL, .pref_code = NULL, .download = FALSE) { # nolint
  L01_006 <- L01_023 <- L01_054 <- NULL
  if (is.null(path)) {
    dl_zip <-
      zip_l01_url(.year, .pref_code)
    path <-
      download_ksj_zip(dl_zip, .download = .download, source = "ksj")
    if (sum(grepl("(.shp|.geojson)$", basename(path))) == 1L) {
      d <- sf::st_read(
        dsn = grep(".shp$",
                   path,
                   value = TRUE),
        options = c("ENCODING=CP932"),
        as_tibble = TRUE,
        stringsAsFactors = FALSE)
    } else if (sum(grepl("(.shp|.geojson)$", basename(path))) == 2L) {
      d <- sf::st_read(dsn = grep(".geojson$",
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
      d <- sf::st_read(dsn = path,
                       as_tibble = TRUE,
                       stringsAsFactors = FALSE)
    }
  }
  if(ncol(d) < 127) {
    kubun_vars <- "法規制"
  } else {
    kubun_vars <- c("用途区分", "防火区分", "都市計画区分", "森林区分", "公園区分")
  }
  zokusei_vars <-
    c("選定状況", "住所漢字", "地積", "利用の現況", "建物構造",
      "供給施設", "駅からの距離",
      kubun_vars[-1],
      "法規制", "建ぺい率", "容積率")
  common_vars <-
    c("標準地コード_見出し番号", "標準地コード_一連番号", "前年度標準地コード_見出し番号", "前年度標準地コード_一連番号",
      "年度", "公示価格",
      paste0("属性移動_",
             zokusei_vars),
      "標準地行政コード", "標準地市区町村名称", "住居表示", "地積",
      "利用現況", "利用状況表示", "建物構造",
      "供給施設有無（水道）", "供給施設有無（ガス）", "供給施設有無（下水）",
      "形状", "間口比率", "奥行比率", "地上階層", "地下階層",
      "前面道路状況", "前面道路の方位", "前面道路の幅員", "前面道路の駅前状況", "前面道路の舗装状況",
      "側道状況", "側道の方位", "交通施設との近接状況", "周辺の土地利用の状況",
      "駅名", "駅からの距離",
      kubun_vars,
      "建ぺい率", "容積率",
      "共通地点", "選定年次ビット")
  wareki_years <-
    c(paste0("S", seq.int(58, 63)),
      paste0("H", seq.int(31)))
  kakaku_vars <-
    paste0(wareki_years,
           "公示価格")
  zokuseiido_vars <-
    paste0("属性移動",
           wareki_years[-1])
  names(d)[seq.int(length(common_vars))] <- common_vars
  last_vars <- ncol(d) - length(common_vars)
  names(d)[seq.int((length(common_vars) + 1), (((length(common_vars) + 1) + length(kakaku_vars[seq.int(last_vars %/% 2)])))-1)] <- kakaku_vars[seq.int(last_vars %/% 2)]
  names(d)[seq.int((length(common_vars) + 1) + length(kakaku_vars[seq.int(last_vars %/% 2)]), ncol(d) -1)] <- zokuseiido_vars[seq.int(last_vars %/% 2)-1]
  d %>%
    dplyr::mutate_if(is.character, .funs = list(~ dplyr::na_if(., "_"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches(
      glue::glue("属性移動_({vars})",
                 vars = paste0(zokusei_vars[-1], collapse = "|"))
    ),
    tidyselect::starts_with("供給施設有無"),
    tidyselect::matches("^共通地点$")),
    .funs = list( ~ dplyr::recode(.,
                                  `false` = FALSE,
                                  `true` = TRUE)))
}
