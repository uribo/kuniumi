library(dplyr)
library(rvest)
extract_ksj_codelist <- function(url, names = NULL) {
  x <-
    xml2::read_html(url)
  d <-
    x %>%
    rvest::html_table(header = TRUE,
                      fill = TRUE) %>%
    purrr::pluck(1) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate_if(is.character,
                     list(~ dplyr::na_if(., "-")))
  if (!is.null(names)) {
    d <-
      d %>%
      purrr::set_names(names)
  }
  d
}

l_ksj_landuse <-
  list(`2016` = "https://nlftp.mlit.go.jp/ksj/gml/codelist/LandUseCd-09.html",
       `2006`   = "https://nlftp.mlit.go.jp/ksj/gml/codelist/LandUseCd-YY.html",
       `1987`   = "https://nlftp.mlit.go.jp/ksj/gml/codelist/LandUseCd-88.html",
       `1976`   = "https://nlftp.mlit.go.jp/ksj/gml/codelist/LandUseCd-77.html") %>%
  purrr::map(extract_ksj_codelist,
             names = c("code", "type", "definition"))

landuse_rgb_cols <-
  tibble::tibble(
    code = l_ksj_landuse$`2016` %>%
      filter(!is.na(type)) %>%
      pull(code),
    color = matrix(
      c(255, 255, 0,
        255, 204, 153,
        0, 170, 0,
        255, 153, 0,
        255, 0, 0,
        140, 140, 140,
        180, 180, 180,
        200, 70, 15,
        0, 0, 255,
        255, 255, 153,
        0, 204, 255,
        0, 255, 0
        #,
        #255, 255, 255
      ),
      ncol = 3,
      byrow = TRUE
    ) %>%
      apply(1, function(x) {
        rgb(red = x[1],
            green = x[2],
            blue = x[3],
            maxColorValue = 255)}))

l_ksj_landuse <-
  l_ksj_landuse %>%
  purrr::modify_at(1,
                   ~ dplyr::left_join(.x, landuse_rgb_cols, by = "code")) %>%
  purrr::list_modify(`2014` = .$`2016`,
                     `2009` = .$`2016`,
                     `1997` = .$`2006`,
                     `1991` = .$`2006`)

l_ksj_landuse <-
  l_ksj_landuse[order(names(l_ksj_landuse), decreasing = TRUE)]

# Pref (P12)
l_ksj_pref <-
  extract_ksj_codelist("https://nlftp.mlit.go.jp/ksj/gml/codelist/PrefCd.html") %>% {
  bind_rows(
    .[, c(1, 2)],
    .[, c(3, 4)]
  )
} %>%
  select(1, 2) %>%
  purrr::set_names(c("コード", "対応する内容"))
# AdminArea (P12)
l_ksj_adminarea <-
  extract_ksj_codelist("https://nlftp.mlit.go.jp/ksj/gml/codelist/AdminAreaCd.html")
# tourismResource (P12)
l_ksj_tourismresource <-
  extract_ksj_codelist("https://nlftp.mlit.go.jp/ksj/gml/codelist/tourismResourceCategoryCd.html")
# Naturalfeature (P19)
l_ksj_naturalfeature <-
  extract_ksj_codelist("https://nlftp.mlit.go.jp/ksj/gml/codelist/NaturalfeatureCd.html")
# Naturalscene (P19)
l_ksj_naturalscene <-
  list(extract_ksj_codelist("https://nlftp.mlit.go.jp/ksj/gml/codelist/NaturalsceneCd.html"))

ksj_code_list <-
  list(
  pref = l_ksj_pref,
  adminarea = l_ksj_adminarea,
  landuse = l_ksj_landuse,
  tourismresource = l_ksj_tourismresource,
  naturalfeature = l_ksj_naturalfeature,
  naturalscene = l_ksj_naturalscene)

usethis::use_data(ksj_code_list, overwrite = TRUE)
