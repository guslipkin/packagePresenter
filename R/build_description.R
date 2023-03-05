#' Title
#'
#' @inheritParams build_presentation
#'
#' @return A list of roxygen2 properties for the package's DESCRIPTION file used
#'   to create the package description slide
#' @keywords internal
.get_description <- function(package) {
  lib <- .get_desc(package, "Package")
  title <- .get_desc(package, "Title")
  description <- .get_desc(package, "Description")

  list(
    "lib" = lib,
    "title" = title,
    "description" = description
  )
}

#' Title
#'
#' @param package_details A list of function details from `.get_description`
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.collate_description <- function(package_details) {
  description_header <- glue::glue("\n\n## {package_details$title}")

  package_details$description <- .fit_content(package_details$description)

  package_details$lib <-
    glue::glue("\n\n```{r}\n#| echo: false\nlibrary({{package_details$lib})\n```", .open = "{{")

  package_contents <- c(
    description_header,
    package_details$description,
    package_details$lib
  )

  return(package_contents)
}
