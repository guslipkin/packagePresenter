#' Parse `_pkgslides.yml`
#'
#' @param package A file path to the root directory of an R package source
#'   folder.
#'
#' @return A list representing a yaml file
#' @keywords internal
.parse_yaml <- function(package) {
  file <- glue::glue("{package}/_pkgslides.yml")
  if (file.exists(file)) {
    yaml <-
      yaml::read_yaml(file) |>
      .check_yaml()
  } else {
    yaml <- .check_yaml(list())
  }
  return(yaml)
}

#' Fill an Incomplete `_pkgslides.yml`
#'
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A list representing a yaml file
#' @keywords internal
.check_yaml <- function(yaml) {
  if (is.null(yaml$layout)) { yaml$layout <- "auto" }

  if (is.null(yaml$format$theme)) { yaml$format$theme <- "default" }

  # adds any missing function options as TRUE
  all_options <-
    c("description", "returns", "parameters", "examples", "code",  "tests")
  requested <- names(yaml$format$functions)
  not_specified <- all_options[!(all_options %in% requested)]

  yaml$format$functions <-
    lapply(seq_along(not_specified), \(s) TRUE) |>
    `names<-`(not_specified) |>
    append(yaml$format$functions)

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
