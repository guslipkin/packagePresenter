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
#'   folder, the name of an R package from CRAN, or an R package from
#'   GitHub in the username/repository format.
#' @param file The file name for your .qmd file. This can be a path so long as
#'   it ends with a `file_name.qmd`
#' @param yaml A `_pkgslides.yml` file or function call to `create_yaml()`
#'
#' @return This function creates and renders a .qmd presentation but does not
#'   return an R object.
#' @export
build_presentation <- function(package = getwd(), file = NULL, yaml = create_yaml()) {

  package <- .find_package(package)
  file <- .find_file(package$path, file)

  yaml <- .parse_yaml(yaml)

  if (package$name %in% rownames(utils::installed.packages())) {
    chunk_opt <- "echo"
  } else { chunk_opt <- "eval" }

  credits <- .get_credits(package$path)

  title_contents <-
    .get_title(package$path, credits) |>
    .collate_title(yaml)

  package_contents <-
    .get_description(package$path, credits) |>
    .collate_description(chunk_opt)

  contents <- .get_roxygen(package$path, yaml)

  function_contents <- .get_functions(contents$functions, yaml)
  r_files <-
    sapply(contents$functions, \(b) .get_file_from_path(b$file))
  function_contents <-
    r_files |>
    unique() |>
    lapply(
      .construct_verticals,
      r_files, function_contents, chunk_opt
    ) |>
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

#' Tries to Find or Download a Package
#'
#' @param package A package name or file path to a package source
#'
#' @return A path to a package source
#' @keywords internal
.find_package <- function(package) {
  if (package == getwd() | dir.exists(package)) {
    name <- rev(strsplit(package, "/")[[1]])[1]
    return(list(name = name, path = package))
  }

  source_path <- tempdir()

  if (package %in% rownames(utils::available.packages())) {
    print("cran")
    utils::download.packages(package, source_path)
    source_name <-
      list.files(source_path, pattern = glue::glue("{package}.*\\.tar\\.gz"))
    utils::untar(
      glue::glue("{source_path}/{source_name}"),
      exdir = glue::glue("{source_path}")
    )
  } else {
    print("github")
    repo <- remotes::parse_repo_spec(package)
    package <- repo$repo
    stopifnot(repo$username != "" & repo$repo != "")
    zip <- glue::glue("{source_path}/{package}.zip")
    utils::download.file(url = glue::glue("https://github.com/{repo$username}/{repo$repo}/archive/master.zip"), zip)
    if (file.exists(zip)) {
      utils::unzip(zip, exdir = glue::glue("{source_path}"), overwrite = TRUE)
    }
    dirs <- list.dirs(glue::glue("{source_path}"), recursive = FALSE)
    dirs <- grep(glue::glue("({package}-(main|master))"), dirs, value = TRUE)
    new_name <- glue::glue("{source_path}/{package}")
    unlink(new_name, recursive = TRUE)
    file.rename(dirs, new_name)
    # file.copy(glue::glue("{getwd()}/_pkgslides.yml"), source_path)
  }

  package_path <- glue::glue("{source_path}/{package}")
  return(list(name = package, path = package_path))
}

# package <- "/Users/guslipkin/Documents/GitHub/pkgslides"
# file <- "/Users/guslipkin/Documents/GitHub/packagePresenter/test.qmd"
# build_presentation(package)
