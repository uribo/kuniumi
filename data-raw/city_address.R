pkgload::load_all()
library(dplyr)
collect_address <- function(url, city_name) {
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
collect_address_slice <- function(slice_index) {
  city_list %>%
    slice(slice_index) %>%
    dplyr::mutate(address = purrr::pmap(.,
                                        ~ collect_address(url = ..5, city_name = ..2)))
}

d_address_91 <- collect_address_slice(1:91)
# collect_address(url = city_list$url[92], city_name = city_list$city_name[92])
# 北海道空知郡南富良野町字幾寅867番地
d_address_100 <- collect_address_slice(93:100)

d_address_125 <- collect_address_slice(101:125)
d_address_150 <- collect_address_slice(126:150)
d_address_160 <- collect_address_slice(151:160)
# collect_address(url = city_list$url[161], city_name = city_list$city_name[161]) # skip # 北海道釧路郡釧路町別保1丁目1番地

d_address_200 <- collect_address_slice(162:200)
d_address_250 <- collect_address_slice(201:250)
d_address_270 <- collect_address_slice(251:270)
d_address_280 <- collect_address_slice(271:280)
d_address_285 <- collect_address_slice(281:285)
# collect_address(url = city_list$url[286], city_name = city_list$city_name[286]) # skip # 秋田県仙北市田沢湖生保内字宮ノ後30
d_address_300 <- collect_address_slice(287:300)
d_address_320 <- collect_address_slice(301:320)
d_address_340 <- collect_address_slice(321:340)
d_address_350 <- collect_address_slice(341:350)
d_address_370 <- collect_address_slice(351:370)
d_address_390 <- collect_address_slice(371:390)
d_address_410 <- collect_address_slice(391:410)
d_address_450 <- collect_address_slice(411:450)
d_address_458 <- collect_address_slice(451:458)
# collect_address(url = city_list$url[459], city_name = city_list$city_name[459]) # skip #
d_address_480 <- collect_address_slice(460:480)
d_address_500 <- collect_address_slice(481:500)
d_address_520 <- collect_address_slice(501:520)
d_address_540 <- collect_address_slice(521:540)
d_address_560 <- collect_address_slice(541:560)
d_address_580 <- collect_address_slice(561:580)
d_address_600 <- collect_address_slice(581:600)
d_address_630 <- collect_address_slice(601:630)
d_address_660 <- collect_address_slice(631:660)
d_address_690 <- collect_address_slice(661:690)
d_address_710 <- collect_address_slice(691:710)
# collect_address(url = city_list$url[711], city_name = city_list$city_name[711]) # skip #
d_address_715 <- collect_address_slice(712:715)
d_address_717 <- collect_address_slice(716:717)
# collect_address(url = city_list$url[718], city_name = city_list$city_name[718])
d_address_740 <- collect_address_slice(719:740)
d_address_780 <- collect_address_slice(741:780)

d_address_800 <- collect_address_slice(781:800)
d_address_850 <- collect_address_slice(801:850)
d_address_900 <- collect_address_slice(851:900)
d_address_950 <- collect_address_slice(901:950)
d_address_1000 <- collect_address_slice(951:1000)
d_address_1050 <- collect_address_slice(1001:1050)
d_address_1100 <- collect_address_slice(1051:1100)
d_address_1150 <- collect_address_slice(1101:1150)
d_address_1200 <- collect_address_slice(1151:1200)
d_address_1285 <- collect_address_slice(1201:1285)
# collect_address(url = city_list$url[1286], city_name = city_list$city_name[1286])
# collect_address(url = city_list$url[1384], city_name = city_list$city_name[1384])
# collect_address(url = city_list$url[1386], city_name = city_list$city_name[1386])
# collect_address(url = city_list$url[1387], city_name = city_list$city_name[1387])
# collect_address(url = city_list$url[1391], city_name = city_list$city_name[1391])
# collect_address(url = "http://vill.kitadaito.okinawa.jp/top_html.html", city_name = city_list$city_name[1632])
# collect_address(url = city_list$url[1640], city_name = city_list$city_name[1640])
d_address_1300 <- collect_address_slice(1287:1300)

d_address_1350 <- collect_address_slice(1301:1350)
d_address_1383 <- collect_address_slice(1351:1383)
d_address_1385 <- collect_address_slice(1385)

d_address_1390 <- collect_address_slice(1388:1390)
d_address_1400 <- collect_address_slice(1391:1400)
d_address_1500 <- collect_address_slice(1401:1500)
d_address_1600 <- collect_address_slice(1501:1600)
d_address_1625 <- collect_address_slice(1601:1631)
d_address_1639 <- collect_address_slice(1633:1639)
d_address_1646 <- collect_address_slice(1641:1646)
d_address_1660 <- collect_address_slice(1648:1660)
d_address_1680 <- collect_address_slice(1661:1680)
d_address_1700 <- collect_address_slice(1681:1700)

d_address_1730 <- collect_address_slice(1706:1730)
d_address_1747 <- collect_address_slice(1731:1747)

df_address <-
  ls(pattern = "d_address_") %>%
      sort() %>%
      purrr::map_dfr(
        get
      )
readr::write_rds(df_address, "d_tmp1_1747last.rds")
rm(list = ls(pattern = "d_address"))
