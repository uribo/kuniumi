#' Kokudosuuchi C02 parser
#' @inheritParams read_ksj_n03
#' @description If there is no local file, specify the year to download.
#' @export
read_ksj_c02 <- function(path = NULL, .year = NULL, .download = FALSE) {
  C02_005 <- C02_007 <- NULL
  if (is.null(path)) {
    path <-
      zip_c02_url(.year) %>%
      download_ksj_zip(.download = .download, source = "ksj")
  }
  path %>%
    purrr::map(
      function(.x) {
        x <-
          grepl("PortAndHarbor", .x)
        if (x == TRUE) {
          d <-
            st_read_crs4612(.x)

          if (stringi::stri_enc_detect(d$C02_005[1])[[1]]$Encoding[1] != "UTF-8") {
            d <-
              d %>%
              dplyr::mutate_at(dplyr::vars(C02_005, C02_007),
                               list(~ iconv(., from = "cp932", to = "UTF8")))
          }
          d
        } else {
          st_read_crs4612(.x)
        }
      }
    )
}

zip_c02_url <- function(year) {
  year <-
    as.character(year)
  year <-
    rlang::arg_match(year,
                     c("2006", "2008", "2014"))
  glue::glue("http://nlftp.mlit.go.jp/ksj/gml/data/{id}/{id}-{yy}/{id}-{yy}_GML.zip",
             id = "C02",
             yy = substr(year, 3, 4))
}