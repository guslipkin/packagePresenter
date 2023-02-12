#' Title
#'
#' @inheritParams create_presentation
#'
#' @return A list of roxygen2 properties for the package's DESCRIPTION file used
#'   to create the title slide
#' @keywords internal
.get_title <- function(package) {
  desc_file <- glue::glue("{package}/DESCRIPTION")

  lib <- desc::desc_get("Package", desc_file)
  version <- desc::desc_get_version(desc_file)

  list(
    "lib" = lib,
    "version" = version
  )
}

#' Title
#'
#' @param package_details A list of function details from `.get_title`
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.collate_title <- function(title_details) {
  title_contents <- c(
    "---",
    glue::glue("title: {title_details$lib}"),
    glue::glue("subtitle: {title_details$version}"),
    "format:",
    "  revealjs:",
    "    navigation-mode: vertical",
    "    self-contained: true",
    "---"
  )
}
