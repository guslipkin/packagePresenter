create_presentation <- function(package, file) {

  if (grepl("\\.qmd", file)) {
    file.create(file)
  } else {
    file <- paste0(file, ".qmd")
    file.create(file)
  }

  r_files <- paste0(package, "/R")
  package_functions <- list.files(r_files, pattern = "\\.R$")

  # paste0(r_files, "/", package_functions) |>
    # lapply(\(f) {
      f <- paste0(r_files, "/", package_functions)[2]
      f <- roxygen2::parse_file(f)
      function_details <- .get_tags(f[[1]])

      function_header <- glue::glue("# `{f[[1]]$object$alias}.R`")
      description <- function_details$description

      param_header <- "## Parameters"
      param <- .process_params(function_details$param)


    # }) |>
    # print()

  file_header <- c(
    "---",
    glue::glue("title: {rev(strsplit(package, '/')[[1]])[1]}"),
    "format:",
    "  revealjs:",
    "    navigation-mode: vertical",
    "---"
  )

  file_contents <- c(
    file_header,
    function_header,
    description,
    param_header,
    param
  )

  fileConn <- file(file)
  writeLines(file_contents, fileConn)
  close(fileConn)

  quarto::quarto_render(file)

}

.get_tags <- function(block) {
  description <- roxygen2::block_get_tags(f[[1]], "description")[[1]]$raw

  param <-
    roxygen2::block_get_tags(block, "param") |>
    lapply(\(tag) {
      tag <- roxygen2::tag_name_description(tag)
      list(
        "name" = tag$val$name,
        "param_description" = tag$val$description
      )
    })

  list(
    "description" = description,
    "param" = param
  )
}

.process_params <- function(param) {
  sapply(param, \(p) {
    glue::glue("- `{p$name}`: {p$param_description}")
  }) |>
    paste0(collapse = "\n")
}

# package <- "/Users/guslipkin/Documents/GitHub/cipheR"
# file <- "/Users/guslipkin/Documents/GitHub/packagePresenter/test.qmd"
# create_presentation(package, file)



