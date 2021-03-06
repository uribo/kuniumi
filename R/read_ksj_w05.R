#' Kokudosuuchi W05 parser
#' @inheritParams read_ksj_n03
#' @param .type File type.
#' @description If there is no local file, specify the prefecture to download.
#' @export
read_ksj_w05 <- function(path = NULL, .pref_code = NULL, .download = FALSE, .type = c("stream", "node")) { # nolint
  if (is.null(path)) {
    dl_zip <-
      zip_w05_url(.pref_code)
    path <- download_ksj_zip(dl_zip, .download = .download, source = "ksj")
  }
  d <-
    st_read_crs4612(path)
  rlang::arg_match(.type)
  if (grepl("RiverNode", basename(path))) {
    d <-
      d
  } else if (grepl("Stream", basename(path))) {
    w05_stream_vars <-
      c(
      # "waterSystemCode",
      "location", "riverCode",
      "sectionType", "riverName", "originalDataType",
      "flowDirection",
      "startRiverNode", "endRiverNode",
      "startStreamNode", "endStreamNode",
      "geometry")
    if (ncol(d) == 12L) {
      d <-
        d %>%
        purrr::set_names(append(w05_stream_vars,
                                "length",
                                after = 10))
    } else if (ncol(d) == 11L) {
      d <-
        d %>%
        purrr::set_names(w05_stream_vars)
    }
  }
  d
}
