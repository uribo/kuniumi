pkgload::load_all()
library(dplyr)
# [ ] いくつかの市区町村は文字コードの関係でエラーになるので除外しておく
if (file.exists("~/Library/Mobile Documents/com~apple~CloudDocs/d_tmp1_1747last.rds") == FALSE) {
  collect_address <- function(url, city_name) {
    # rawToChar(httr::content(httr::GET(url), as = "raw"))
    x <-
      xml2::read_html(url)
    x_address <-
      x %>%
      rvest::html_nodes(css = "address") %>%
      rvest::html_text(trim = TRUE)
    res <-
      stringr::str_extract(x_address, ".+(大字|丁目|番地|字.+|号)")
    if (identical(res, character(0)) || is.na(res)) {
      res_tmp <-
        x %>%
        rvest::html_text(trim = TRUE) %>%
        stringr::str_split("\n", simplify = TRUE) %>%
        as.vector()
      res <-
        res_tmp %>%
        stringr::str_subset(city_name) %>%
        stringr::str_subset("[0-9]$")
      if (identical(res, character(0))) {
        res <-
          res_tmp %>%
          stringr::str_subset("文字", negate = TRUE) %>%
          stringr::str_extract(".+(字|大字|丁目|番地|号)")
        if (identical(res, character(0))) {
          res <-
            res_tmp %>%
            stringr::str_extract("(\u3012.+)")
          if (identical(res, character(0))) {
            res <-
              res_tmp %>%
              stringr::str_extract(".+-[0-9]")
          }
        }
      }
    }
    res %>%
      na.omit() %>%
      c() %>%
      stringr::str_subset(city_name) %>%
      stringr::str_remove(".+(役所|役場)") %>%
      stringr::str_remove("所在地") %>%
      stringr::str_remove("(\uff1a|:)") %>%
      stringr::str_remove("(\u3012 |\u3012)([0-9]{3}-[0-9]{4})") %>%
      # stringr::str_remove("(TEL |TEL)([0-9])") %>%
      # FAX
      stringr::str_trim() %>%
      unique()
  }
  # collect_address(url = city_list$url[1], city_name = city_list$city_name[1])
  slow_collect_address <-
    purrr::slowly(
      ~ collect_address(url = .x, city_name = .y),
      rate = purrr::rate_delay(pause = 5),
      quiet = FALSE)
  df_address <-
    city_list %>%
    dplyr::mutate(address = purrr::pmap(.,
                                        ~ slow_collect_address(url = ..5, city_name = ..2)))
  readr::write_rds(df_address, "~/Library/Mobile Documents/com~apple~CloudDocs/d_tmp1_1747last.rds")
} else {
  # 1747 - 1729
  df_address <-
    readr::read_rds("~/Library/Mobile Documents/com~apple~CloudDocs/d_tmp1_1747last.rds") %>%
    assertr::verify(nrow(.) == 1729)
}

df_address <-
  df_address %>%
  mutate(addr_length = purrr::map_dbl(address,
                              ~ length(.x)))

df_address1 <-
  df_address %>%
  filter(addr_length == 1) %>%
  tidyr::unnest(cols = address)

df_address1 %>%
  slice(11:20)

c(
  `01205` = "北海道室蘭市幸町1番2号",
  `01211` = "北海道網走市南６条東４丁目",
  `01216` = "北海道芦別市北1条東1丁目3番地",
  `01221` = "北海道名寄市風連町西町196番地1"
)


city_list %>%
  anti_join(df_address,
            by = c("prefecture_name", "city_name", "administrative_area_code", "admin_type", "url")) %>%
  mutate(address = recode(
    administrative_area_code,
    `01462` = "北海道空知郡南富良野町字幾寅867番地",
    `01661` = "北海道釧路郡釧路町別保1丁目1番地",
    `05215` = "秋田県仙北市田沢湖生保内字宮ノ後30",
    `10209` = "群馬県藤岡市中栗須327番地",
    `16202` = "富山県高岡市広小路7-50",
    `35321` = "山口県玖珂郡和木町和木1丁目1番1号",
    `40100` = "北九州市小倉北区城内1番1号",
    `40202` = "福岡県大牟田市有明町2丁目3番地",
    `40203` = "福岡県久留米市城南町15番地3",
    `47358` = "沖縄県島尻郡北大東村字中野218",
    .default = NA_character_
  ))
