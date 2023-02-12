#' Title
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
create_presentation <- function(package, file = "") {

  # if I do package = getwd() in the arguments or the below chunk then I get
  #   a recursive loop when I try to assign file in the live block below
  # if (is.null(package)) {
  #   package <- getwd()
  # }
  if (file == "") {
    package_name <- rev(strsplit(package, '/')[[1]])[1]
    file <- package_name
  }

  if (grepl("\\.qmd$", file)) {
    file.create(file)
  } else {
    file <- paste0(file, ".qmd")
    file.create(file)
  }

  title_contents <-
    .get_title(package) |>
    .collate_title()

  package_contents <-
    .get_description(package) |>
    .collate_description()

  r_files <- paste0(package, "/R")
  package_functions <- list.files(r_files, pattern = "\\.R$")
  function_contents <-
    paste0(r_files, "/", package_functions) |>
    lapply(.get_functions) |>
    unlist()

  file_contents <- c(
    title_contents,
    package_contents,
    function_contents
  )

  fileConn <- file(file)
  writeLines(file_contents, fileConn)
  close(fileConn)

  quarto::quarto_render(file)

}

# package <- "/Users/guslipkin/Documents/GitHub/cipheR"
# file <- "/Users/guslipkin/Documents/GitHub/packagePresenter/test.qmd"
# create_presentation(package)
