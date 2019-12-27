#############################
# 市町村の役場・役所の住所および座標
# 座標はジオコーディングして付与する
# 課題 [ ] 役所が複数ある場合（支所）
#############################
pkgload::load_all()
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

city_address_fixed <- c(
  `01205` = "北海道室蘭市幸町1番2号",
  `01211` = "北海道網走市南６条東４丁目",
  `01216` = "北海道芦別市北1条東1丁目3番地",
  `01221` = "北海道名寄市風連町西町196番地1",
  `01224` = "北海道千歳市東雲町2丁目34番地",
  `01230` = "北海道登別市中央町6丁目11番地",
  `01231` = "北海道恵庭市京町1番地",
  `01233` = "北海道伊達市鹿島町20-1",
  `01304` = "北海道石狩郡新篠津村第47線北13番地",
  `01333` = "北海道上磯郡知内町字重内21番地1",
  `01346` = "北海道二海郡八雲町住初町138",
  `01361` = "北海道檜山郡江差町字中歌町193-1",
  `01391` = "北海道島牧郡島牧村字泊83-1",
  `01394` = "北海道磯谷郡蘭越町蘭越町258番地5",
  `01395` = "北海道虻田郡ニセコ町字富士見47番地",
  `01396` = "北海道虻田郡真狩村字真狩118番地",
  `01400` = "北海道虻田郡倶知安町北1条東3丁目3番地",
  `01402` = "北海道岩内郡岩内町字高台134番地",
  `01409` = "北海道余市郡赤井川村字赤井川74番地2",
  `01428` = "北海道夕張郡長沼町中央北1丁目1番1号",
  `01434` = "北海道雨竜郡秩父別町4101番地",
  `01462` = "北海道空知郡南富良野町字幾寅867番地",
  `01463` = "北海道勇払郡占冠村字中央",
  `01464` = "北海道上川郡和寒町字西町120番地",
  `01471` = "北海道中川郡中川町字中川337番地",
  `01483` = "北海道苫前郡苫前町字旭37番地の1",
  `01487` = "北海道天塩郡天塩町新栄通8丁目",
  `01511` = "北海道宗谷郡猿払村鬼志別西町172番地1",
  `01516` = "北海道天塩郡豊富町大通6丁目",
  `01520` = "北海道天塩郡幌延町宮園町1番地1",
  `01545` = "北海道斜里郡斜里町本町12番地",
  `01547` = "北海道斜里郡小清水町元町2丁目1番1号",
  `01559` = "北海道紋別郡湧別町上湧別屯田市街地318番地",
  `01562` = "北海道紋別郡西興部村字西興部100番地",
  `01563` = "北海道紋別郡雄武町本町",
  `01571` = "北海道虻田郡豊浦町字船見町95番地2",
  `01584` = "北海道虻田郡洞爺湖町栄町58番地",
  `01602` = "北海道沙流郡平取町本町28番地",
  `01604` = "北海道新冠郡新冠町字北星町3番地",
  `01661` = "北海道釧路郡釧路町別保1丁目1番地",
  `01641` = "北海道広尾郡大樹町東本通33",
  `05215` = "秋田県仙北市田沢湖生保内字宮ノ後30",
  `10209` = "群馬県藤岡市中栗須327番地",
  `16202` = "富山県高岡市広小路7-50",
  `16210` = "富山県南砺市苗島4880番地",
  `01662` = "北海道厚岸郡厚岸町真栄3丁目1番地",
  `01663` = "北海道厚岸郡浜中町霧多布東4条1丁目35番地1",
  `01691` = "北海道野付郡別海町別海常盤町280番地",
  `02202` = "青森県弘前市大字上白銀町1-1",
  `02208` = "青森県むつ市中央一丁目8番1号",
  `02209` = "青森県つがる市木造若緑61番地1",
  `35321` = "山口県玖珂郡和木町和木1丁目1番1号",
  `40100` = "北九州市小倉北区城内1番1号",
  `40202` = "福岡県大牟田市有明町2丁目3番地",
  `40203` = "福岡県久留米市城南町15番地3",
  `47358` = "沖縄県島尻郡北大東村字中野218"
)

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
