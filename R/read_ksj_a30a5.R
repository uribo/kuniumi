#' Kokudosuuchi A30a5 parser
#' @inheritParams read_ksj_n03
#' @param .meshcode Specific 80km meshcode
#' @description If there is no local file, specify the meshcode to download.
#' @export
read_ksj_a30a5 <- function(path = NULL, .meshcode = NULL, .download = FALSE) {
  hazardDate <- maxRainfallFor_24h <- maxRainfall_h <- outflowSediment_m3 <- landslideLength_m <- NULL # nolint
  if (is.null(path)) {
    dl_zip <-
      zip_a30a5_url(meshcode = .meshcode)
    path <- download_ksj_zip(dl_zip, .download = .download)
  }
  d <-
    st_read_crs4612(path) %>%
    dplyr::mutate_if(is.character,
                     dplyr::na_if,
                     y = intToUtf8(c(31354, 30333), multiple = FALSE))
  xml_info <-
    list.files(gsub(pattern = "(.+)/.+$",
                    replacement = "\\1",
                    path),
               pattern = paste0("^",
                                gsub("_SedimentDisasterAndSnowslide.+",
                                     "",
                                     basename(path)),
                                ".xml"),
               full.names = TRUE)
  if (length(xml_info) == 1) {
    d <-
      d %>%
      purrr::set_names(
        c(
          "prefectureName", "cityName",
          "hazardDate", "hazardType",
          stringr::str_glue("{x[1]}_{x[2]}",
                            x = c(stringr::str_split("maxRainfallFor24h",
                                                     "(?=24h)",
                                                     simplify = TRUE))),
          paste0("maxRainfall", "_h"),
          "inclination",
          paste0("outflowSediment", "_m3"),
          paste0("landslideLength", "_m"),
          "meshCode",
          attr(d, "sf_column")
        )) %>%
      # nolint start
      dplyr::mutate(hazardDate = lubridate::as_date(hazardDate)) %>%
      dplyr::mutate(maxRainfallFor_24h = units::set_units(as.numeric(stringr::str_remove(maxRainfallFor_24h, "mm/24hr")), "mm"),
                    maxRainfall_h = units::set_units(as.numeric(stringr::str_remove(maxRainfall_h, "mm/hr")), "mm"),
                    outflowSediment_m3 = units::set_units(as.numeric(stringr::str_remove(outflowSediment_m3, "m3")), "m3"),
                    landslideLength_m = units::set_units(as.numeric(stringr::str_remove(landslideLength_m, "m")), "m")) %>%
      # nolint end
      dplyr::mutate_if(is.character,
                       stringi::stri_trans_general,
                       id = "nfkc")
  }
  d
}
