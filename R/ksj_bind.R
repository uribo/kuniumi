#' Bind KSJ files in directory
#' @param dir path
#' @param identifier Data identifier. (e.g. "N02")
#' @param regexp A regular expression passed on target directory.
#' @param translate Path to parser function. "raw", "jp" or "en".
#' @param ... Additional arguments to parser function
#' @export
ksj_bind <- function(dir, identifier, regexp = NULL, translate = NULL) {
  files <-
    list.files(dir,
               pattern = regexp,
               recursive = TRUE,
               full.names = TRUE)
  if (is.null(translate)) {
    d <-
      files %>%
      purrr::map(
        ~ rlang::exec(paste0("read_ksj_", identifier),
                      path = .x))
  } else {
    d <-
      files %>%
      purrr::map(
        ~ rlang::exec(paste0("read_ksj_", identifier),
                      path = .x,
                      translate = translate))
  }
  d %>%
    purrr::reduce(rbind)
}
