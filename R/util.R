zip_n03_url <- function(year, pref_code) {
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           values = as.character(
                             c(1920L,
                               seq.int(1950, 1985, by = 5L),
                               seq.int(1995, 2005, by = 5L),
                               seq.int(2006, 2018, by = 1L))))
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
      year == "2018" ~ "180101")
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
