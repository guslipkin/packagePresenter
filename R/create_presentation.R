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

  r_files <- paste0(package, "/R")
  package_functions <- list.files(r_files, pattern = "\\.R$")

  package_contents <-
    .get_description(package) |>
    .collate_description()

  function_contents <-
    paste0(r_files, "/", package_functions) |>
    lapply(.get_functions) |>
    unlist()

  title_contents <-
    .get_title(package) |>
    .collate_title()

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

package <- "/Users/guslipkin/Documents/GitHub/cipheR"
# file <- "/Users/guslipkin/Documents/GitHub/packagePresenter/test.qmd"
create_presentation(package)
