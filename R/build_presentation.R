#' Create a Revealjs Presentation with Quarto
#'
#' @description When pointed at the root directory of an R Package Project, this
#'   function will automatically generate and render a Quarto/Revealjs
#'   presentation for that package. It will have a title slide and description
#'   slide generated from the package's DESCRIPTION file and each exported
#'   function will have slides with its description and returns, parameters,
#'   examples, and source code.
#'
#' @param package A file path to the root directory of an R package source
#'   folder.
#' @param file The file name for your .qmd file. This can be a path so long as
#'   it ends with a `file_name.qmd`
#'
#' @return This function creates and renders a .qmd presentation but does not
#'   return an R object.
#' @export
build_presentation <- function(package = NULL, file = NULL) {

  package <- .find_package(package)
  file <- .find_file(package, file)

  yaml <- .parse_yaml(package)

  title_contents <-
    .get_title(package) |>
    .collate_title(yaml)

  package_contents <-
    .get_description(package) |>
    .collate_description()

  r_files <- paste0(package, "/R")
  package_functions <- list.files(r_files, pattern = "\\.R$")
  function_contents <-
    .get_functions(package, yaml) |>
    lapply(.collate_functions) |>
    unlist()

  file_contents <- c(
    title_contents,
    package_contents,
    function_contents
  )

  print(file)
  file.create(file)
  fileConn <- file(file)
  writeLines(file_contents, fileConn)
  close(fileConn)

  quarto::quarto_render(file)

}

.find_file <- function(package, file) {
  if (is.null(file)) {
    file <- .get_file_from_path(package)
  }

  if (!grepl("\\.qmd$", file)) {
    file <- paste0(file, ".qmd")
  }

  file <- glue::glue("{getwd()}/{file}")

  return(file)
}

.find_package <- function(package) {
  if (is.null(package)) {
    package <- getwd()
    return(package)
  }

  source_path <- tempdir()
  utils::download.packages(package, source_path)
  source_name <-
    list.files(source_path, pattern = glue::glue("{package}.*\\.tar\\.gz"))
  utils::untar(
    glue::glue("{source_path}/{source_name}"),
    exdir = glue::glue("{source_path}")
  )
  package <- glue::glue("{source_path}/{package}")
  return(package)
}

# package <- "/Users/guslipkin/Documents/GitHub/pkgslides"
# file <- "/Users/guslipkin/Documents/GitHub/packagePresenter/test.qmd"
# create_presentation(package)
