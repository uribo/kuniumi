# library(rvest)
# read_html("http://nlftp.mlit.go.jp/ksj/gml/codelist/NaturalsceneCd.html") %>%
#   html_table() %>%
#   purrr::pluck(1) %>%
#   tibble::as_tibble() %>%
#   dput()

natural_scene <-
  tibble::tibble(
    typeCode = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    typeName = c("火山景観",
      "山地（非火山性）景観", "石灰岩景観", "陸景特殊地学景観",
      "陸景その他", "河川景観", "湖沼景観", "海岸景観",
      "水景特殊地学景観", "水景その他"))

natural_feature <-
  tibble::tibble(
    vistaResourcesClassCode = c(
      "101",
      "102",
      "103",
      "104",
      "105",
      "106",
      "107",
      "108",
      "109",
      "110",
      "111",
      "112",
      "113",
      "114",
      "115",
      "116",
      "201",
      "202",
      "203",
      "204",
      "205",
      "206",
      "207",
      "208",
      "209",
      "210",
      "211",
      "212",
      "213",
      "214",
      "215",
      "216",
      "217",
      "218",
      "219",
      "301",
      "302",
      "303",
      "304",
      "401",
      "402",
      "501",
      "502",
      "601",
      "602",
      "603",
      "604",
      "605",
      "606",
      "607",
      "608",
      "609",
      "610",
      "611",
      "701",
      "702",
      "801",
      "802",
      "803",
      "804",
      "805",
      "806",
      "807",
      "808",
      "809",
      "810",
      "811",
      "812",
      "813",
      "814",
      "815",
      "816",
      "817",
      "818",
      "819",
      "901",
      "902",
      "903",
      "1001",
      "1002"
    ),
    vistaResourcesName = c(
      "火山群",
      "火山",
      "火山性高原",
      "火口・カルデラ",
      "カルデラ壁",
      "流れ山群",
      "火山　特徴的な稜線",
      "溶岩トンネル・風穴",
      "溶岩流末端崖",
      "地獄、泥火山",
      "噴泉",
      "噴気孔",
      "間歇泉",
      "火山　構造土",
      "高山域・亜高山域",
      "火山　万年雪",
      "山脈、山地、高地",
      "丘陵",
      "非火山性高原",
      "大断層崖",
      "非火山性弧峰",
      "U字谷（氷食谷）",
      "カール",
      "非対称山稜",
      "山地　特徴的な稜線",
      "モレーン",
      "二重山稜（綿状凹地）",
      "山地　断崖・岩壁",
      "岩塊斜面、岩海",
      "山地　構造土",
      "山地　岩峰・岩柱",
      "崖錐",
      "山地　天然橋・岩門・石門",
      "高山域、亜高山域",
      "山地　万年雪",
      "カルスト地形",
      "ポリエ",
      "カッレンフェルト・ドリーネ群",
      "鍾乳洞",
      "陸景　節理",
      "陸景　岩脈",
      "陸景で上記以外の際立った地形",
      "陸景で顕著な自然現象を記録する地形",
      "峡谷・渓谷",
      "河成段丘",
      "自由蛇行河川",
      "穿入蛇行河川",
      "河川　断崖・岩壁",
      "瀞",
      "河川　岩峰・岩柱",
      "淵",
      "河川　甌穴群",
      "滝",
      "河川　天然橋・岩門・石門",
      "湖沼",
      "湿原",
      "溺れ谷",
      "海成段丘",
      "断層海岸",
      "火山海岸",
      "多島海",
      "隆起サンゴ礁",
      "砂浜、礫浜",
      "砂嘴",
      "砂州",
      "陸けい砂州",
      "砂丘",
      "海食崖",
      "波食台（ベンチ）",
      "岩礁",
      "海食洞",
      "岩門",
      "湖吹穴",
      "海岸　甌穴群",
      "潮流、渦流",
      "水景　節理",
      "水景　岩脈",
      "湧泉群",
      "水景で上記以外の際立った地形",
      "水景で顕著な自然現象を記録する地形"))

ksj_code_list <-
  list(
    natural_scene = natural_scene,
    natural_feature = natural_feature)
usethis::use_data(ksj_code_list, overwrite = TRUE)