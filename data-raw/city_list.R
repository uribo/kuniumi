###################################
# 全国の市区町村についての情報 (ウェブページのURL)
###################################
## code to prepare `city_list` dataset goes here
pkgload::load_all()
library(dplyr)
library(sf)
library(stringr)
# 全国の市町村数 -----------------------------------------------------------------
# 1902 --------------------------------------------------------------------
df_jp_all <-
  read_ksj_n03("~/Documents/resources/国土数値情報/N03/2019/N03-190101_GML/N03-19_190101.geojson") %>%
  st_drop_geometry() %>%
  distinct(prefectureName, countyName, cityName, administrativeAreaCode) %>%
  assertr::verify(nrow(.) == 1909) %>%
  janitor::clean_names() %>%
  mutate(admin_type = case_when(
    str_detect(city_name, "区$") ~ "ward",
    str_detect(city_name, "市$") ~ "city",
    str_detect(city_name, "町$") ~ "town",
    str_detect(city_name, "村$") ~ "village",
    str_detect(city_name, "^所属未定地$") ~ "unfixed")) %>%
  filter(admin_type != "unfixed") %>%
  # arrange(administrative_area_code) %>%
  mutate(
    city_name = if_else(
      administrative_area_code == "28221",
      "丹波篠山市",
      city_name)) %>%
  assertr::verify(nrow(.) == 1902L)

# forcats::fct_relevel("ward", "city", "town", "village")


# df_jp_all %>% arrange(administrative_area_code) するとだめ。df_jp_all1724 を作成した後に行う

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
  filter(str_detect(prefecture_name, "東京都", negate = TRUE) && str_detect(admin_type, "ward", negate = TRUE)) %>%
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

collect_city_website_url <- function(city) {
  city %>%
    purrr::map_chr(
      function(.x) {
        res <-
          WikidataR::find_item(.x, language = "ja") %>%
          purrr::keep(~ !is.null(.x$description) &&
                        grepl("((village|town|city|one of).+ Japan|special ward of Tokyo)",
                              .x$description,
                              ignore.case = FALSE)) %>%
          purrr::flatten() %>%
          purrr::pluck("id")
        if (is.null(res)) {
          NA_character_
        } else {
          res %>%
            WikidataR::get_item() %>%
            purrr::map(c("claims", "P856", "mainsnak", "datavalue", "value")) %>%
            purrr::flatten_chr() %>%
            purrr::pluck(1)
        }
      }
    )
}
# collect_city_website_url("つくば市")
# collect_city_website_url("清川村")
# collect_city_website_url("丹波篠山市") # 新しいものもOK
# collect_city_website_url("小美玉市")
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
    assertr::verify(nrow(.) == 1688L) %>%
    mutate(url = collect_city_website_url(city = city_name))
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

level_key <- c(`01233` = "https://www.city.date.hokkaido.jp/",
               `01331` = "http://www.town.matsumae.hokkaido.jp/",
               `01345` = "https://www.town.hokkaido-mori.lg.jp/",
               `01403` = "http://www.vill.tomari.hokkaido.jp/",
               `01601` = "http://www.town.hidaka.hokkaido.jp/",
               `01636` = "http://www.town.shimizu.hokkaido.jp/",
               `01644` = "https://www.town.hokkaido-ikeda.lg.jp/",
               `01696` = NA_character_,
               `08302` = "http://www.town.ibaraki.lg.jp/index2.html",
               `02445` = "http://www.town.aomori-nanbu.lg.jp/",
               `04324` = "http://www.town.kawasaki.miyagi.jp/",
               `04505` = "http://www.town.misato.miyagi.jp/",
               `05434` = "https://www.town.misato.akita.jp/",
               `06323` = "https://www.town.asahi.yamagata.jp/",
               `06361` = "https://www.town.kaneyama.yamagata.jp/",
               `06382` = "http://www.town.kawanishi.yamagata.jp/",
               `06401` = "http://www.town.oguni.yamagata.jp/",
               `07213` = "https://www.city.fukushima-date.lg.jp/",
               `07445` = "https://www.town.kaneyama.fukushima.jp/",
               `07446` = "http://www.vill.showa.fukushima.jp/",
               `10383` = "http://www.nanmoku.ne.jp/",
               `10428` = "https://www.vill.takayama.gunma.jp/",
               `10448` = "https://www.vill.showa.gunma.jp/",
               `10522` = "https://www.town.meiwa.gunma.jp/",
               `11381` = "http://www.town.saitama-misato.lg.jp/",
               `13206` = "https://www.city.fuchu.tokyo.jp/",
               `16343` = "https://www.town.asahi.toyama.jp/",
               `18382` = "https://www.town.ikeda.fukui.jp/",
               `18442` = "http://www.town.mihama.fukui.jp/",
               `19366` = "https://www.town.nanbu.yamanashi.jp/",
               `20304` = "http://www.vill.kawakami.nagano.jp/",
               `20305` = "http://www.minamimakimura.jp/",
               `20403` = "http://www.town.nagano-takamori.lg.jp/",
               `20481` = "https://www.ikedamachi.net/",
               `20543` = "https://www.vill.takayama.nagano.jp/",
               `21404` = "https://www.town.gifu-ikeda.lg.jp/",
               `22341` = "http://www.town.shimizu.shizuoka.jp/",
               `22461` = "https://www.town.morimachi.shizuoka.jp/",
               `23446` = "http://www.town.aichi-mihama.lg.jp/",
               `24343` = "http://www.town.asahi.mie.jp/",
               `24442` = "https://www.town.meiwa.mie.jp/",
               `25383` = "http://www.town.shiga-hino.lg.jp/",
               `27381` = "https://www.town.taishi.osaka.jp/",
               `28464` = "http://www.town.hyogo-taishi.lg.jp/",
               `29361` = "https://www.town.nara-kawanishi.lg.jp/",
               `29452` = "http://www.vill.kawakami.nara.jp/",
               `30362` = "https://www.town.hirogawa.wakayama.jp/",
               `30381` = "http://www.town.mihama.wakayama.jp/",
               `30382` = "http://www.town.wakayama-hidaka.lg.jp/",
               `31389` = "https://www.town.nanbu.tottori.jp/",
               `31402` = "https://www.town.hino.tottori.jp/",
               `32448` = "http://www.town.shimane-misato.lg.jp/",
               `34208` = "http://www.city.fuchu.hiroshima.jp/",
               `38401` = "https://www.town.masaki.ehime.jp/",
               `40544` = "http://www.town.hirokawa.fukuoka.jp/",
               `40605` = "https://www.town-kawasaki.com/",
               `43348` = "https://www.town.kumamoto-misato.lg.jp/",
               `43424` = "https://www.town.kumamoto-oguni.lg.jp/",
               `43428` = "http://town.takamori.kumamoto.jp/",
               `45431` = "http://www.town.miyazaki-misato.lg.jp/",
               `01393` = "http://www.kuromatsunai.com/",
               `01665` = "https://www.town.teshikaga.hokkaido.jp/",
               `01695` = NA_character_,
               `01697` = NA_character_,
               `01698` = NA_character_,
               `01699` = NA_character_,
               `01700` = NA_character_,
               `02384` = "http://www.town.tsuruta.lg.jp/",
               `02441` = "https://www.town.sannohe.aomori.jp/",
               `07545` = "https://www.town.okuma.fukushima.jp/",
               `08221` = "https://www.city.hitachinaka.lg.jp/",
               `10201` = "https://www.city.maebashi.gunma.jp/",
               `11202` = "https://www.city.kumagaya.lg.jp/",
               `12204` = "https://www.city.funabashi.lg.jp/",
               `12215` = "http://www.city.asahi.lg.jp/",
               `13104` = "https://www.city.shinjuku.lg.jp/",
               `13121` = "https://www.city.adachi.tokyo.jp/",
               `13210` = "https://www.city.koganei.lg.jp/",
               `13214` = "http://www.city.kokubunji.tokyo.jp/",
               `13229` = "https://www.city.nishitokyo.lg.jp/",
               `13303` = "https://www.town.mizuho.tokyo.jp/",
               `13305` = "https://www.town.hinode.tokyo.jp/",
               `13307` = "https://www.vill.hinohara.tokyo.jp/",
               `13308` = "http://www.town.okutama.tokyo.jp/",
               `18201` = "https://www.city.fukui.lg.jp/",
               `18501` = "https://www.town.fukui-wakasa.lg.jp/",
               `20212` = "http://www.city.omachi.nagano.jp/",
               `23441` = "http://www.town.agui.lg.jp/",
               `31325` = "http://www.town.wakasa.tottori.jp/",
               `32207` = "http://www.city.gotsu.lg.jp/",
               `32343` = "https://www.town.okuizumo.shimane.jp/",
               `32441` = "http://www.town.shimane-kawamoto.lg.jp/",
               `32527` = "http://www.vill.chibu.lg.jp/",
               `32528` = "https://www.town.okinoshima.shimane.jp/",
               `33208` = "http://www.city.soja.okayama.jp/",
               `34203` = "https://www.takeharakankou.jp/",
               `36468` = "https://www.town.tokushima-tsurugi.lg.jp/",
               `40344` = "https://www.town.sue.fukuoka.jp/",
               `42201` = "https://www.city.nagasaki.lg.jp/",
               `46218` = "http://www.city-kirishima.jp/",
               `46225` = "http://www.city.aira.lg.jp/",
               `46491` = "http://www.town.minamiosumi.lg.jp/",
               `47209` = "http://www.city.nago.okinawa.jp/",
               `47211` = "https://www.city.okinawa.okinawa.jp/",
               `47302` = "http://www.vill.ogimi.okinawa.jp/",
               `47306` = "https://www.nakijin.jp/",
               `47311` = "https://www.vill.onna.okinawa.jp/",
               `47324` = "http://www.vill.yomitan.okinawa.jp/",
               `47325` = "http://www.town.kadena.okinawa.jp/") %>%
  ensurer::ensure(length(.) == nrow(df_jp_all1724_manualfix))

df_jp_all1724_manualfix <-
  df_jp_all1724_manualfix %>%
  mutate(url = recode(administrative_area_code, !!!level_key, .default = NA_character_))

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
