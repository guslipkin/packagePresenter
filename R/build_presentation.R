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

  if (rev(strsplit(package, "/")[[1]])[1] %in% rownames(utils::installed.packages())) {
    chunk_opt <- "echo"
  } else { chunk_opt <- "eval" }

  yaml <- .parse_yaml(package)

  title_contents <-
    .get_title(package) |>
    .collate_title(yaml)

  package_contents <-
    .get_description(package) |>
    .collate_description(chunk_opt)

  contents <- .get_roxygen(package, yaml)

  r_files <- paste0(package, "/R")
  package_functions <- list.files(r_files, pattern = "\\.R$")
  function_contents <-
    .get_functions(contents$functions, yaml) |>
    lapply(.collate_functions, chunk_opt) |>
    unlist()

  data_contents <-
    .get_datasets(contents$datasets, yaml) |>
    lapply(.collate_datasets) |>
    unlist()

  file_contents <- c(
    title_contents,
    package_contents,
    function_contents,
    data_contents
  )

  print(file)
  file.create(file)
  fileConn <- file(file)
  writeLines(file_contents, fileConn)
  close(fileConn)

  quarto::quarto_render(file)

}

#' Title
#'
#' @param package A path to a package source
#' @param file A file path
#'
#' @return A file path to write the .qmd to
#' @keywords internal
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

#' Title
#'
#' @param package A package name or file path to a package source
#'
#' @return A path to a package source
#' @keywords internal
.find_package <- function(package) {
  if (is.null(package)) {
    package <- getwd()
    return(package)
  } else if (dir.exists(package)) {
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

# package <- "/Users/guslipkin/Documents/GitHub/palmerpenguins"
# file <- "/Users/guslipkin/Documents/GitHub/packagePresenter/test.qmd"
# build_presentation(package)
