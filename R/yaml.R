#' Parse `_pkgslides.yml`
#'
#' @param package A file path to the root directory of an R package source
#'   folder.
#'
#' @return A list
#' @keywords internal
.parse_yaml <- function(package) {
  file <- glue::glue("{package}/_pkgslides.yml")
  if (file.exists(file)) {
    yaml <- yaml::read_yaml(file)
  } else {
    yaml <- NULL
  }
  return(yaml)
}

#' Create `_pkgslides.yml`
#'
#' @param package A file path to the root directory of an R package source
#'   folder.
#'
#' @return This will not fill the file, simply create it.
#' @export
create_yaml <- function(package) {
  file <- glue::glue("{package}/_pkgslides.yml")
  if (!file.exists(file)) {
    file.create(file)
  } else {
    print(glue::glue("{file} already exists"))
  }
}
