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
#' @param path A file path to where you want the yaml file written. This should
#'   not end in a slash of any kind.
#' @param format_theme A length-one character vector of theme details
#' @param format_functions A named list of format function options
#' @param format_datasets A named list of format dataset options
#' @param choose_functions A vector of function names
#' @param choose_datasets A vector of dataset names
#'
#' @return This will not fill the file, simply create it.
#' @export
#'
#' @examples
#' # NOT RUN
#' # To create a yaml like this for the palmerpenguins package
#' # YAML
#' # ----
#' # format:
#' #   theme: default
#' #   functions:
#' #     tests: false
#' #
#' # functions: all
#' #
#' # datasets:
#' #   - penguins
#' #
#' # R:
#' # ----
#' # create_yaml(
#' #   "palmerpenguins",
#' #   format_functions = list(tests = FALSE),
#' #   choose_datasets = c("penguins")
#' # )
create_yaml <- function(
    path = ".",
    format_theme = c(), format_functions = list(), format_datasets = list(),
    choose_functions = c(), choose_datasets = c()
    ) {
  # package <- .find_package(package)
  file <- glue::glue("{path}/_pkgslides.yml")
  stopifnot(!file.exists(file))

  # formatting
  format_theme <- format_theme[format_theme %in% c("theme")]
  format_functions <-
    format_functions[names(format_functions) %in% c("description", "returns", "parameters", "examples", "code",  "tests")]
  format_datasets <-
    format_datasets[names(format_datasets) %in% c("format", "source", "references", "view")]

  .append_to_yaml <- function(yaml, obj, name) {
    if (length(obj) > 0) { yaml$format[[name]] <- obj }
    return(yaml)
  }

  yaml <-
    list() |>
    .append_to_yaml(format_theme, "theme") |>
    .append_to_yaml(format_functions, "functions") |>
    .append_to_yaml(format_datasets, "datasets")

  # print options
  yaml$functions <- choose_functions
  yaml$datasets <- choose_datasets

  yaml |>
    .check_yaml() |>
    yaml::write_yaml(file)
}
