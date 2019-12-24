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
slow_collect_address <-
  purrr::slowly(
    ~ collect_address(url = .x, city_name = .y),
    rate = purrr::rate_delay(pause = 5),
    quiet = FALSE)
df_address <-
  city_list %>%
  dplyr::mutate(address = purrr::pmap(.,
                                      ~ slow_collect_address(url = ..5, city_name = ..2)))
readr::write_rds(df_address, "d_tmp1_1747last.rds")
