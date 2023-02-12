#' Get a DESCRIPTION Property
#'
#' @param package A file path to the root directory of an R package source
#'   folder.
#' @param tag The name of the tag whose contents are desired
#'
#' @return A length-one character vector with the desired text from a package's
#'   DESCRIPTION file
#' @keywords internal
.get_desc <- function(package, tag) {
  desc::desc_get(tag, glue::glue("{package}/DESCRIPTION"))
}
