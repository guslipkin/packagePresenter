#' Title
#'
#' @param path A file path
#'
#' @return A length-one character vector with the last portion of the supplied
#'   path
#' @keywords internal
.get_file_from_path <- function(path) {
  rev(strsplit(path, '/')[[1]])[1]
}

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

#' Puts Content in an `r-fit-content` div
#'
#' @param content A length-one character vector
#'
#' @return A length-one character vector that renders a Quarto `r-fit-content`
#'   div
#' @keywords internal
.fit_content <- function(content) {
  glue::glue("\n\n::: {.r-fit-text}\n\n{{content}\n\n:::", .open = "{{")
}

#' Title
#'
#' @param l A list
#'
#' @return A list with null values removed
#' @keywords internal
.drop_null_from_list <- function(l) {
  l[sapply(l, is.null)] <- NULL
  if (length(l) == 0) { return(NULL) }
  return(l)
}
