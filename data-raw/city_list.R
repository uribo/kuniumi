###################################
# 全国の市区町村についての情報 (ウェブページのURL)
###################################
## code to prepare `city_list` dataset goes here
pkgload::load_all()
source("data-raw/city_keys.R")
library(dplyr)
library(sf)
library(stringr)
# 全国の市町村数 -----------------------------------------------------------------
# 220116 --------------------------------------------------------------------
dir.create("data-raw/N03")
if (!file.exists("data-raw/N03/N03-22_220101.geojson")) {
  options(timeout = 3600)
  download.file("https://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-2022/N03-20220101_GML.zip",
                "data-raw/N03/N03-20220101_GML.zip")
  options(timeout = 60)
  unzip("data-raw/N03/N03-20220101_GML.zip",
        exdir = "data-raw/N03")
  usethis::use_git_ignore("data-raw/N03")
}
df_jp_all <-
  read_ksj_n03("data-raw/N03/N03-22_220101.geojson") %>%
  st_drop_geometry() %>%
  filter(!is.na(administrativeAreaCode)) %>%
  distinct(prefectureName, countyName, cityName, administrativeAreaCode) %>%
  assertr::verify(nrow(.) == 1902L) %>%
  janitor::clean_names() %>%
  mutate(admin_type = case_when(
    str_detect(city_name, "区$") ~ "ward",
    str_detect(city_name, "市$") ~ "city",
    str_detect(city_name, "町$") ~ "town",
    str_detect(city_name, "村$") ~ "village",
    str_detect(city_name, "^所属未定地$") ~ "unfixed")) %>%
  filter(admin_type != "unfixed") %>%
  # arrange(administrative_area_code) %>%
  assertr::verify(nrow(.) == 1902L)

# forcats::fct_relevel("ward", "city", "town", "village")

# 1724 --------------------------------------------------------------------
# 政令指定都市の市町村コード
df_ward <-
  tibble::tribble(
    ~prefecture_code, ~city_name, ~administrative_area_code,
    "01", "札幌市",     "01100",
    "04", "仙台市",     "04100",
    "11", "さいたま市", "11100",
    "12", "千葉市",     "12100",
    "14", "横浜市",     "14100",
    "14", "川崎市",     "14130",
    "14", "相模原市",   "14150",
    "15", "新潟市",     "15100",
    "22", "静岡市",     "22100",
    "22", "浜松市",     "22130",
    "23", "名古屋市",   "23100",
    "26", "京都市",     "26100",
    "27", "大阪市",     "27100",
    "27", "堺市",       "27140",
    "28", "神戸市",     "28100",
    "33", "岡山市",     "33100",
    "34", "広島市",     "34100",
    "40", "福岡市",     "40130",
    "40", "北九州市",   "40100",
    "43", "熊本市",     "43100"
  ) %>%
  assertr::verify(nrow(.) == 20L)

df_jp_all1724 <-
  df_jp_all %>%
  mutate(city_name = if_else(str_detect(county_name, "市$") & !is.na(county_name),
                             county_name,
                             city_name)) %>%
  mutate(administrative_area_code = if_else(str_detect(county_name, "市$") & !is.na(county_name),
                                            NA_character_,
                                            administrative_area_code)) %>%
  distinct(prefecture_name, city_name, administrative_area_code, admin_type) %>%
  assertr::verify(nrow(.) == 1747L) %>%
  left_join(df_ward %>%
              select(city_name,
                     administrative_area_code_fix = administrative_area_code),
            by = c("city_name")) %>%
  mutate(administrative_area_code = if_else(is.na(administrative_area_code),
                                            administrative_area_code_fix,
                                            administrative_area_code)) %>%
  select(-administrative_area_code_fix) %>%
  arrange(administrative_area_code)

df_jp_all1724 %>%
  ensurer::ensure_that(nrow(.) - 23 == 1724L)

df_jp_all1724 %>%
  filter(is.na(administrative_area_code)) %>%
  assertr::verify(nrow(.) == 0L)

df_jp_all <-
  df_jp_all %>%
  arrange(administrative_area_code)

collect_city_website_url <- memoise::memoise(
  function(city) {
    city %>%
      purrr::map_chr(
        function(.x) {
          res <-
            WikidataR::find_item(.x, language = "ja")
          res_id <-
            res %>%
            purrr::keep(~ !is.null(.x$description) &&
                          grepl("((village|town|city|one of).+ Japan|special ward of Tokyo)",
                                .x$description,
                                ignore.case = FALSE)) %>%
            purrr::flatten() %>%
            purrr::pluck("id")
          if (is.null(res_id)) {
            NA_character_
          } else {
            res_url <-
              res_id %>%
              WikidataR::get_item() %>%
              purrr::map(c("claims", "P856", "mainsnak", "datavalue", "value")) %>%
              purrr::flatten_chr() %>%
              purrr::pluck(1)
            if (is.null(res_url)) {
              res_url <- NA_character_
            }
            res_url
          }
        }
      )
  }
)
# collect_city_website_url("つくば市")
# collect_city_website_url("清川村")
# collect_city_website_url("丹波篠山市") # 新しいものもOK
# collect_city_website_url("小美玉市")
# collect_city_website_url("色丹村")
# collect_city_website_url("函館市") # URLが複数、一つを選択

# 同じ名前の市町村 (26) は除外しておく
duplicate_city_names <-
  df_jp_all1724 %>%
  count(city_name, sort = TRUE) %>%
  filter(n >= 2) %>%
  pull(city_name) %>%
  ensurer::ensure(length(.) == 26L)

if (file.exists("data-raw/city_list1724_url.csv") == FALSE) {
  # 1 hour
  df_jp_all1724_comp <-
    df_jp_all1724 %>%
    filter(!city_name %in% duplicate_city_names) %>%
    assertr::verify(nrow(.) == 1688L)

  urls <-
    df_jp_all1724_comp$city_name %>%
    purrr::map_chr(collect_city_website_url)
  df_jp_all1724_comp$url <- urls
  df_jp_all1724_comp %>%
    readr::write_csv("data-raw/city_list1724_url.csv")
} else {
  df_jp_all1724_comp <-
    readr::read_csv("data-raw/city_list1724_url.csv",
                    col_types = "ccccc")
}

df_jp_all1724_manualfix <-
  bind_rows(
    df_jp_all1724 %>%
      filter(city_name %in% duplicate_city_names) %>%
      mutate(admin_type = as.character(admin_type)),
    df_jp_all1724_comp %>%
      filter(is.na(url)) %>%
      assertr::verify(nrow(.) == 49L) %>%
      select(-url)) %>%
  assertr::verify(nrow(.) == 108L)

ensurer::ensure(length(.) == nrow(df_jp_all1724_manualfix))

df_jp_all1724_manualfix <-
  df_jp_all1724_manualfix %>%
  mutate(url = recode(administrative_area_code, !!!city_url_fix, .default = NA_character_))

df_jp_all1724_manualfix %>%
  filter(is.na(url)) %>%
  slice(-seq.int(7)) %>%
  assertr::verify(nrow(.) == 0L)

city_list <-
  bind_rows(
    df_jp_all1724_comp %>%
      filter(!is.na(url)),
    df_jp_all1724_manualfix)
usethis::use_data(city_list, overwrite = TRUE)
# city_list <-
#   city_list %>%
#   mutate(url = recode(administrative_area_code, !!!city_url_fix, .default = url))
