#############################
# 市町村の役場・役所の住所および座標
# 座標はジオコーディングして付与する
# 課題 [ ] 役所が複数ある場合（支所）
#############################
pkgload::load_all()
source("data-raw/city_keys.R")
library(dplyr)
library(stringr)
# [ ] いくつかの市区町村はソースのエンコーディングが原因でエラーになるので除外しておく
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
  slice(151:160) %>%
  mutate(address = recode(
    administrative_area_code,
    !!!city_address_fixed,
    .default = address
  ))

# city_list %>%
#   anti_join(df_address,
#             by = c("prefecture_name", "city_name", "administrative_area_code", "admin_type", "url")) %>%
#   mutate(address = recode(
#     administrative_area_code,
#     !!!city_address_fixed,
#     .default = NA_character_
#   ))

df_address_tmp <-
  df_address1 %>%
  slice(1:50) %>%
  mutate(address = recode(
    administrative_area_code,
    !!!city_address_fixed,
    .default = address
  )) %>%
  mutate(address = if_else(str_detect(address, str_c("^", prefecture_name)),
                           address,
                           str_c(prefecture_name, address)))


# geocoding ---------------------------------------------------------------
# library(jpndistrict)
library(sf)
df_isj_a <-
  readr::read_rds("~/Documents/projects2019/jp-address/data-raw/isj_2018a.rds")

df_address_tmp <-
  df_address_tmp %>%
  select(administrative_area_code, address) %>%
  mutate(address_elem = purrr::map(address,
                                   zipangu::separate_address)) %>%
  tidyr::hoist(address_elem,
               prefecture = "prefecture",
               city = "city",
               streeet = "street")

df_address_tmp

df_search_res <-
  df_isj_a %>%
  filter(`代表フラグ` == 1) %>%
  filter(prefecture == "北海道",
         city == "札幌市中央区",
         str_detect(street_lv1, "北一条西二丁目")) %>%
  select(num_range("street_lv", 1:3), longitude, latitude)

# タイルを元にポイントを変更する必要はありそう。あくまでも大まかな場所
df_search_res %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mapview::mapview()

# こっちの方が正確...
# jpndistrict::jpn_admins(1) # 373
st_sfc(st_point(c(141.3544, 43.06197)), crs = 4326) %>%
  mapview::mapview()
