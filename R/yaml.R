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
  if (is.null(yaml$functions)) { yaml$functions <- "auto" }

  if (is.null(yaml$datasets)) { yaml$datasets <- "all" }

  if (is.null(yaml$format$theme)) { yaml$format$theme <- "default" }

  yaml <-
    yaml |>
    .set_as_true(
      "functions",
      c("description", "returns", "parameters",
        "examples", "code",  "tests")
    ) |>
    .set_as_true(
      "datasets",
      c("format", "source", "references", "view")
    )

  return(yaml)
}

#' Title
#'
#' @param yaml A yaml file as a list
#' @param type "functions" or "datasets"
#' @param options A vector of the options exposed to users in the yaml format
#'
#' @return A yaml where non-included options are set to TRUE
#' @keywords internal
.set_as_true <- function(yaml, type, options) {
  requested <- names(yaml$format[[type]])
  not_specified <- options[!(options %in% requested)]

  yaml$format[[type]] <-
    lapply(seq_along(not_specified), \(s) TRUE) |>
    `names<-`(not_specified) |>
    append(yaml$format[[type]])

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
