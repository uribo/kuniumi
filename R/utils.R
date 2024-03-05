#' @import utils
utils::globalVariables("where")

build_req_url <- function(api = c("getKSJSummary", "getKSJURL"), ...) {
  rlang::arg_match(api)
  req_url <-
    glue::glue(
      "https://nlftp.mlit.go.jp/ksj/api/{version}/index.php/app/{api}.xml?appId={app_id}&lang={lang}&dataformat={data_format}", # nolint
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

build_isj_req_url <- function(area_code = NULL, fiscal_year = NULL, pos_level = 0) {
  # nolint start
  glue::glue("{url}?appId=isjapibeta1&areaCode={area_code}&fiscalyear='{fiscal_year}'&posLevel={pos_level}",
             url = "https://nlftp.mlit.go.jp/isj/api/1.0b/index.php/app/getISJURL.xml") %>%
    httr::parse_url()
  # nolint end
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

view_ksj_description <- function(identifier) {
  url <-
    paste0("https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-",
           identifier,
           ".html")
  utils::browseURL(url)
}

ksj_data_url <- function(identifier = identifier, ...) {
  build_req_url("getKSJURL", identifier = identifier, ...) %>%
    request_to_ksj()
}

isj_data_url <- function(area_code, fiscal_year, pos_level) {
  build_isj_req_url(area_code, fiscal_year, pos_level) %>%
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
      "https://nlftp.mlit.go.jp/ksj/gml/data/A30a5/A30a5-11/A30a5-11_",
      meshcode,
      "-jgd_GML.zip")
}

zip_l01_url <- function(year, pref_code) {
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           values = as.character(seq.int(1983, 2019)))
  glue::glue(
    "https://nlftp.mlit.go.jp/ksj/gml/data/L01/L01-{year_dir}/L01-{year_dir}_{pref_code}_GML.zip", # nolint
    year_dir = substr(year, 3L, 4L),
    pref_code = sprintf("%02d", as.numeric(pref_code)) %>%
      jpndistrict:::prefcode_validate())
}

zip_l02_url <- function(year, pref_code) {
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           values = as.character(seq.int(1983, 2020)))
  glue::glue(
    "https://nlftp.mlit.go.jp/ksj/gml/data/L02/L02-{year_dir}/L02-{year_dir}_{pref_code}_GML.zip", # nolint
    year_dir = substr(year, 3L, 4L),
    pref_code = sprintf("%02d", as.numeric(pref_code)) %>%
      jpndistrict:::prefcode_validate())
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
        "https://nlftp.mlit.go.jp/ksj/gml/data/L03-a/L03-a-{year_dir}/L03-a-",
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
    "https://nlftp.mlit.go.jp/ksj/gml/data/N05/N05-{year_dir}/N05-{year_dir_zip}", # nolint
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
                               seq.int(2006, 2022, by = 1L))))

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
      year == "2019" ~ "190101",
      year == "2020" ~ "20200101",
      year == "2021" ~ "20210101",
      year == "2022" ~ "20220101")
  paste0(
    "https://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-",
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
    "https://nlftp.mlit.go.jp/ksj/gml/data/N02/N02-",
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

  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/W05/W05-{year_dir}/W05-{year_dir}_{pref_code}_GML.zip", # nolint
             year_dir = dplyr::case_when(
                 d$year == "2006" ~ "06",
                 d$year == "2007" ~ "07",
                 d$year == "2008" ~ "08",
                 d$year == "2009" ~ "09"))
}

zip_a16 <- function(year, pref_code = NULL) {
  year <- rlang::arg_match(year,
                           values = c("2006", "2011", "2018"))
  pref_code <-
    sprintf("%02d", as.numeric(pref_code)) %>%
    jpndistrict:::prefcode_validate()
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/A09/A09-{yy}/A09-{yy}_{pref_code}_GML.zip", # nolint
             yy = substr(year, 3, 4))
}

#' @importFrom utils download.file unzip
download_ksj_zip <- function(dl_zip, .download = FALSE, ...) {
  path <- dplyr::if_else(.download == TRUE,
                         ".",
                         tempdir())
  check_dl_comment(...)
  zip_path <-
    paste0(path, "/", basename(dl_zip))
  dl_zip %>%
    download.file(zip_path)
  path <- paste0(path, "/", gsub(".zip", "", basename(dl_zip)))
  dir.create(path)
  unzip(zipfile = zip_path,
        exdir = path,
        overwrite = TRUE)
  grep("(shp|geojson|csv)$",
         list.files(path,
                    full.names = TRUE,
                    recursive = TRUE),
         value = TRUE,
       ignore.case = TRUE)
}

check_area_code <- function(area_code) {
  if (is.numeric(area_code)) {
    area_code <-
      sprintf("%02d", area_code)
  } else if (is.character(area_code)) {
    area_code <-
      sprintf("%02s", area_code)
  }
  rlang::arg_match(area_code,
                   sprintf("%02d", seq.int(0, 47)))
}

check_dl_comment <- function(source = NULL) {
  warning(
    glue::glue(
      intToUtf8(c(12371, 12398, 12469, 12540, 12499, 12473, 12399,
                  12289, 12300, 22269, 22303, 20132, 36890, 30465),
                multiple = FALSE),
      " {source_service}",
      intToUtf8(c(65288, 12459, 12486, 12468, 12522, 21517, 65289,
                  12301, 12434, 12418, 12392, 12395, 21152, 24037,
                  32773, 12364, 20316, 25104),
                multiple = FALSE),
      "\n",
      intToUtf8(c(20197, 19979, 12398), multiple = FALSE),
      "{source_service}",
      intToUtf8(c(12480, 12454, 12531, 12525, 12540, 12489, 12469,
                  12540, 12499, 12473, 12398, 21033, 29992, 32004,
                  27454, 12434, 12372, 30906, 35469, 12398, 19978,
                  12372, 21033, 29992, 12367, 12384, 12373, 12356, 65306),
                multiple = FALSE),
      "\n",
      "{source_url}",
      source_service = dplyr::case_when(
        source == "isj"
        ~ intToUtf8(c(20301, 32622, 21442, 29031, 24773, 22577), # nolint
        multiple = FALSE),
        source == "ksj"
        ~ intToUtf8(c(22269, 22303, 25968, 20516, 24773, 22577), # nolint
        multiple = FALSE)),
      source_url = dplyr::case_when(
        source == "isj" ~ "https://nlftp.mlit.go.jp/isj/agreement.html",
        source == "ksj" ~ "https://nlftp.mlit.go.jp/ksj/other/yakkan.html"
      )
    ),
    call. = FALSE
  )
}

ksj_common_meshes <- function(identifier) {
  areaCode <- year <- NULL
  d <-
    kokudosuuchi::getKSJURL(identifier = identifier)
  d_meshes <-
    d %>%
    dplyr::distinct(year, areaCode) %>%
    dplyr::group_by(year) %>%
    dplyr::group_map(
      ~ .x %>%
        dplyr::pull(areaCode) %>%
        unique() %>%
        sort()) %>%
    purrr::set_names(d %>%
                       dplyr::group_by(year, areaCode) %>%
                       dplyr::group_keys() %>%
                       dplyr::pull(year) %>%
                       unique())

  list(
    all = d_meshes,
    common = d_meshes %>%
      purrr::reduce(c) %>%
      table() %>%
      as.list() %>%
      purrr::keep(~ .x == length(d_meshes)) %>%
      names()
  )
}

prefcode_validate <- function(pref_code) {
  codes <-
    sapply(seq(1, 47, 1), sprintf, fmt = "%d")
  if (identical(codes[codes %in% pref_code], character(0)))
    rlang::abort("jis_code must be start a integer or as character from 1 to 47.")
  as.numeric(pref_code)
}
