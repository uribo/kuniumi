zip_p09 <- function(meshcode) {
  jpmesh::is_meshcode(meshcode)
  target_mesh <- c("3623", "3624", "3725", "3831", "3926",
                   "3927", "3928", "3942", "4027", "4028",
                   "4042", "4128", "4129", "4229", "4429",
                   "4530", "4630", "4631", "4729", "4730",
                   "4731", "4828", "4829", "4830", "4831",
                   "4928", "4929", "4930", "4931", "4932",
                   "4933", "4934", "4939", "5029", "5030",
                   "5031", "5032", "5033", "5034", "5035",
                   "5036", "5129", "5130", "5131", "5132",
                   "5133", "5134", "5135", "5136", "5137",
                   "5138", "5139", "5231", "5232", "5233",
                   "5234", "5235", "5236", "5237", "5238",
                   "5239", "5240", "5332", "5333", "5334",
                   "5335", "5336", "5337", "5338", "5339",
                   "5340", "5432", "5433", "5435", "5436",
                   "5437", "5438", "5439", "5440", "5536",
                   "5537", "5538", "5539", "5540", "5541",
                   "5636", "5637", "5638", "5639", "5640",
                   "5641", "5738", "5739", "5740", "5741",
                   "5839", "5840", "5841", "5939", "5940",
                   "5941", "6039", "6040", "6041", "6140",
                   "6141", "6240", "6241", "6243", "6339",
                   "6340", "6341", "6342", "6343", "6440",
                   "6441", "6442", "6443", "6444", "6445",
                   "6540", "6541", "6542", "6543", "6544",
                   "6545", "6641", "6642", "6643", "6644",
                   "6645", "6741", "6742", "6841")
  if (length(meshcode[meshcode %in% target_mesh]) == 0L)
    rlang::abort("No data with selected meshcode.")
  glue::glue("https://nlftp.mlit.go.jp/ksj/gml/data/P09/P09-10/P09-10_{meshcode}-jgd_GML.zip") # nolint
}

#' Kokudosuuchi P09 parser
#' @inheritParams read_ksj_a16
#' @inheritParams read_ksj_a30a5
#' @export
read_ksj_p09 <- function(path, translate = "jp", .meshcode = NULL, .download = FALSE) {
  if (is.null(path)) {
    dl_zip <-
      zip_p09(.meshcode)
    path <- download_ksj_zip(dl_zip, .download = .download)
  }
  lang <-
    rlang::arg_match(translate,
                     c("raw", "jp", "en"))
  d <-
    st_read_crs4612(path)
  if (lang == "jp") {
    d <-
      d %>%
      kokudosuuchi::translateKSJData()
  }
  d
}
