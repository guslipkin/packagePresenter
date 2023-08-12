#' Create `_pkgslides.yml`
#'
#' @param path A file path to where you want the yaml file written. This should
#'   not end in a slash of any kind.
#' @param format_theme A length-one character vector of theme details
#' @param format_functions A named list of format function options
#' @param format_datasets A named list of format dataset options
#' @param choose_functions A list of file or function names
#' @param choose_datasets A vector of dataset names
#'
#' @return This will create the file and write to it, then return the
#'  file path.
#' @export
create_yaml <- function(
    path = getwd(),
    format_theme = c(), format_functions = list(), format_datasets = list(),
    choose_functions = list(), choose_datasets = c()
) {
  # package <- .find_package(package)
  cd <-
    path |>
    strsplit("/") |>
    unlist() |>
    utils::tail(1)
  if (cd == "_pkgslides.yml" && file.exists(path)) {
    return(path)
  }
  file <- glue::glue("{path}/_pkgslides.yml")
  # stopifnot(!file.exists(file))

  # formatting
  format_theme <- format_theme[format_theme %in% c("theme")]
  format_functions <-
    format_functions[names(format_functions) %in% c("description", "return", "parameters", "examples", "code")]
  format_datasets <-
    format_datasets[names(format_datasets) %in% c("format", "source", "references")]

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
  if (length(choose_functions) > 0) {
    yaml <- .process_choose_functions(yaml, choose_functions)
  }
  yaml$datasets <- choose_datasets

  yaml |>
    .check_yaml() |>
    yaml::write_yaml(file)

  print(glue::glue("Config written to '{file}'"))

  return(file)
}

#' Parse `_pkgslides.yml`
#'
#' @param file A file path the _pkgslides.yml file
#'
#' @return A list representing a yaml file
#' @keywords internal
.parse_yaml <- function(file) {
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
      c("description", "return", "parameters",
        "examples", "code")
    ) |>
    .set_as_true(
      "datasets",
      c("format", "source", "references")
    )

  return(yaml)
}

#' Set Empty Values as True
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

#' Process Choose Functions
#'
#' @param yaml A list representing a yaml file
#' @param choose The list passed to `choose_functions` in the `create_yaml`
#'   function
#'
#' @return A list representing a yaml file
#' @keywords internal
.process_choose_functions <- function(yaml, choose) {
  yaml2 <- list()
  dim <- vapply(choose, length, FUN.VALUE = 1)
  choose_names <- names(choose)

  opt_regex <- "^(auto|exported|all)$|\\.R$"

  if (all(dim == 1) & is.null(choose_names)) {
    stopifnot(all(grepl(opt_regex, choose)))
    yaml2$functions <- lapply(choose, \(x) { list(file = x) })
  } else {
    if (any(choose_names == "")) {
      needs_name <- which(choose_names == "" & dim == 1)
      names(choose)[needs_name] <- unlist(sapply(choose, unlist)[needs_name])
      choose[needs_name] <- "all"
      choose_names <- names(choose)
    }
    stopifnot(all(grepl(opt_regex, choose_names)))
    yaml2$functions <-
      mapply(\(x, y) {
        list(file = x, slides = y)
      }, choose_names, choose, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }


  yaml <-
    list(yaml, yaml2) |>
    unlist(recursive = FALSE)

  return(yaml)
}
