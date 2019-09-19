build_req_url <- function(api = c("getKSJSummary", "getKSJURL"), ...) {
  rlang::arg_match(api)
  req_url <-
    glue::glue(
      "http://nlftp.mlit.go.jp/ksj/api/{version}/index.php/app/{api}.xml?appId={app_id}&lang={lang}&dataformat={data_format}", # nolint
      version = "1.0b",
      app_id = "ksjapibeta1",
      lang = "J",
      data_format = 1 # JPGIS2.1
    ) %>%
    httr::parse_url()
  if (api == "getKSJURL")
    req_url$query <- c(req_url$query,
                       purrr::map_at(list(...),
                       c("prefCode", "meshCode", "metroArea", "fiscalyear"),
                       paste,
                       collapse = ","))
  req_url
}

parse_ksj_xml <- function(x) {
  item <- NULL
  if (x[[1]][[1]][[1]][[1]] != 0)
    rlang::abort("error")
  cat(cli::col_cyan("Hit"),
      cli::style_bold(x[[1]]$NUMBER[[1]]),
      cli::col_cyan("records.\n"))
  tibble::tibble(item = purrr::pluck(purrr::pluck(x, 1), 4)) %>%
    tidyr::unnest_wider(item) %>%
    dplyr::mutate_all(unlist)
}

request_to_ksj <- function(x) {
  httr::build_url(x) %>%
    httr::GET() %>%
    httr::content(encoding = "UTF-8") %>%
    xml2::as_list() %>%
    parse_ksj_xml()
}

ksj_data_url <- function(identifier = identifier, ...) {
  build_req_url("getKSJURL", identifier = identifier, ...) %>%
    request_to_ksj()
}

st_read_crs4612 <- function(path, ...) {
  sf::st_read(path,
              stringsAsFactors = FALSE,
              crs = 4612,
              as_tibble = TRUE,
              ...)
}

st_read_crs6668 <- function(path, ...) {
  sf::st_read(
    path,
    crs = 6668,
    options = c("ENCODING=CP932"),
    stringsAsFactors = FALSE,
    as_tibble = TRUE,
    ...)
}

zip_a30a5_url <- function(meshcode) {
  jpmesh::is_meshcode(meshcode)
  target_mesh <- stringr::str_subset(jpmesh::meshcode_set(mesh_size = 80),
                      paste0(c("3036", "3622", "3623", "3624", "3631", "3641",
                               "3653", "3724", "3725", "3741", "3823", "3824",
                               "3831", "3841", "3926", "3942", "4027", "4028",
                               "4040", "4042", "4129", "4142", "4230", "4328",
                               "4329", "4440", "4529", "4540", "4629", "4728",
                               "4739", "4740", "4828", "4839", "5038", "5039",
                               "5137", "5229", "5435", "5531", "5541", "5939",
                               "5942", "6039", "6139", "6239", "6243", "6339",
                               "6343", "6439", "6442", "6445", "6540", "6541",
                               "6544", "6545", "6546", "6641", "6642", "6644",
                               "6646", "6647", "6740", "6742", "6747", "6748",
                               "6840", "6842", "6847", "6848"),
                             collapse = "|"),
                      negate = TRUE)
  if (as.character(meshcode) %in% target_mesh)
    paste0(
      "http://nlftp.mlit.go.jp/ksj/gml/data/A30a5/A30a5-11/A30a5-11_",
      meshcode,
      "-jgd_GML.zip")
}

zip_l03a_url <- function(year, meshcode, datum = 2) {
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           values = as.character(
                             c(1976, 1987, 1991, 1997,
                               2006, 2009, 2014, 2016)))
  jpmesh::is_meshcode(meshcode)
  if (as.character(meshcode) %in% jpmesh::meshcode_set(mesh_size = 80))
    year_dir <-
    dplyr::case_when(
      year == "1976" ~ "76",
      year == "1987" ~ "87",
      year == "1991" ~ "91",
      year == "1997" ~ "97",
      year == "2006" ~ "06",
      year == "2009" ~ "09",
      year == "2014" ~ "14",
      year == "2016" ~ "16")
  target_mesh <-
    stringr::str_subset(
      jpmesh::meshcode_set(mesh_size = 80),
      paste0(c("3036", "3622", "3623", "3624", "3631", "3641",
               "3653", "3724", "3725", "3741", "3823", "3824",
               "3831", "3841", "3926", "3927", "3928", "3942",
               "4027", "4028", "4040", "4042", "4128", "4129",
               "4142", "4229", "4230", "4328", "4329", "4429",
               "4440", "4529", "4530", "4531", "4540", "4629",
               "4728", "4739", "4740", "4839", "4939", "5038",
               "5039", "5129", "5139", "5229", "5432", "5433",
               "5531", "5941", "5942", "6139", "6140", "6141",
               "6239", "6240", "6241", "6243", "6339", "6340",
               "6341", "6342", "6343", "6439", "6440", "6441",
               "6442", "6443", "6444", "6445", "6540", "6541",
               "6542", "6543", "6544", "6545", "6546", "6641",
               "6642", "6643", "6644", "6645", "6646", "6647",
               "6740", "6741", "6742", "6747", "6748", "6840",
               "6841", "6842", "6847", "6848"),
             collapse = "|"),
      negate = TRUE)
    if (year == "2016" & !as.character(meshcode) %in% target_mesh) {
      NULL
    } else {
      glue::glue(
        "http://nlftp.mlit.go.jp/ksj/gml/data/L03-a/L03-a-{year_dir}/L03-a-",
        dplyr::if_else(stringr::str_detect(year_dir, "09|14|16"),
                       "{year_dir}_{meshcode}-jgd_GML.zip",
                       "{year_dir}_{meshcode}_GML.zip"))
      }
}

zip_n05_url <- function(year) {
  year_dir <- NULL
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           values = as.character(
                             seq.int(2011, 2018, by = 1L)))
  glue::glue(
    "http://nlftp.mlit.go.jp/ksj/gml/data/N05/N05-{year_dir}/N05-{year_dir_zip}", # nolint
    year_dir = substr(year, 3L, 4L),
    year_dir_zip = dplyr::if_else(year == "2013",
                                  "13.zip",
                                  paste0(year_dir, "_GML.zip")))
}

zip_n03_url <- function(year, pref_code) {
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           values = as.character(
                             c(1920L,
                               seq.int(1950, 1985, by = 5L),
                               seq.int(1995, 2005, by = 5L),
                               seq.int(2006, 2019, by = 1L))))
  year_dir <-
    dplyr::case_when(
      year == "1920" ~ "200101",
      year == "1950" ~ "501001",
      year == "1955" ~ "551001",
      year == "1960" ~ "601001",
      year == "1965" ~ "651001",
      year == "1970" ~ "701001",
      year == "1975" ~ "751001",
      year == "1980" ~ "801001",
      year == "1985" ~ "851001",
      year == "1995" ~ "951001",
      year == "2000" ~ "001001",
      year == "2005" ~ "05",
      year == "2006" ~ "06",
      year == "2007" ~ "071001",
      year == "2008" ~ "090320",
      year == "2009" ~ "100329",
      year == "2010" ~ "110331",
      year == "2011" ~ "120331",
      year == "2012" ~ "120401",
      year == "2013" ~ "130401",
      year == "2014" ~ "140401",
      year == "2015" ~ "150101",
      year == "2016" ~ "160101",
      year == "2017" ~ "170101",
      year == "2018" ~ "180101",
      year == "2019" ~ "190101")
  paste0(
    "http://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-",
    year,
    "/N03-", # nolint
    year_dir,
    "_",
    sprintf("%02d", as.numeric(pref_code)) %>%
      jpndistrict:::prefcode_validate(),
    "_GML.zip"
  )
}

zip_n02_url <- function(year) {

  year_dir <- substr(year, 3, 4)
  paste0(
    "http://nlftp.mlit.go.jp/ksj/gml/data/N02/N02-",
    year_dir,
    "/N02-", # nolint
    year_dir,
    dplyr::if_else(year_dir == "13",
                   ".zip",
                   "_GML.zip")
  )
}

zip_w05_url <- function(pref_code) {
  pref_code <-
    sprintf("%02d", as.numeric(pref_code)) %>%
    jpndistrict:::prefcode_validate()
  d <-
    tibble::tibble(
    year = c(rep("2006", 4),
             rep("2007", 18),
             rep("2008", 18),
             rep("2009", 7)),
    pref_code = c(
      "36", "37", "38", "39", "02",
      "03", "04", "05", "06", "07",
      "15", "16", "17", "18", "40",
      "41", "42", "43", "44", "45",
      "46", "47", "08", "09", "10",
      "11", "12", "13", "14", "19",
      "20", "21", "22", "23", "24",
      "31", "32", "33", "34", "35",
      "01", "25", "26", "27", "28",
      "29", "30")) %>%
    dplyr::filter(pref_code == !! rlang::enquo(pref_code))

  glue::glue("http://nlftp.mlit.go.jp/ksj/gml/data/W05/W05-{year_dir}/W05-{year_dir}_{pref_code}_GML.zip", # nolint
             year_dir = dplyr::case_when(
                 d$year == "2006" ~ "06",
                 d$year == "2007" ~ "07",
                 d$year == "2008" ~ "08",
                 d$year == "2009" ~ "09"))
}

#' @importFrom utils download.file unzip
download_ksj_zip <- function(dl_zip, .download = FALSE) {
  path <- dplyr::if_else(.download == TRUE,
                         ".",
                         tempdir())
  zip_path <-
    paste0(path, "/", basename(dl_zip))
  dl_zip %>%
    download.file(zip_path)
  path <- paste0(path, "/", gsub(".zip", "", basename(dl_zip)))
  dir.create(path)
  unzip(zipfile = zip_path,
        exdir = path,
        overwrite = TRUE)
  grep("(shp|geojson)$",
         list.files(path,
                    full.names = TRUE,
                    recursive = TRUE),
         value = TRUE)
}
