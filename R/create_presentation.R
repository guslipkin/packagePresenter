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
      f <- paste0(r_files, "/", package_functions)[1]
      f <- roxygen2::parse_file(f)
      function_details <- .get_tags(f[[1]])

      function_details$title <- glue::glue("\n\n# {function_details$title}")

      function_details$description <- glue::glue("- Description: {function_details$description}")

      function_details$returns <- glue::glue("- Returns: {function_details$returns}")

      param_header <- "\n\n## Parameters"
      function_details$param <- .process_params(function_details$param)

      function_file_header <- glue::glue("\n\n## `{rev(strsplit(f[[1]]$file, '/')[[1]])[1]}`")
      function_details$code <- glue::glue("```{.r}\n{{function_details$code}\n```", .open = "{{")



    # }) |>
    # print()

  file_header <- c(
    "---",
    glue::glue("title: {package_name}"),
    "format:",
    "  revealjs:",
    "    navigation-mode: vertical",
    "---"
  )

  file_contents <- c(
    file_header,
    function_details$title,
    function_details$description,
    function_details$returns,
    param_header,
    function_details$param,
    function_file_header,
    function_details$code
  )

  fileConn <- file(file)
  writeLines(file_contents, fileConn)
  close(fileConn)

  quarto::quarto_render(file)

}

.get_tags <- function(block) {
  title <- roxygen2::block_get_tags(block, "title")[[1]]$raw
  description <- roxygen2::block_get_tags(block, "description")[[1]]$raw
  returns <- roxygen2::block_get_tags(block, "returns")[[1]]$raw

  param <-
    roxygen2::block_get_tags(block, "param") |>
    lapply(\(tag) {
      tag <- roxygen2::tag_name_description(tag)
      list(
        "name" = tag$val$name,
        "param_description" = tag$val$description
      )
    })

  code <- rlang::expr_text(block$call)

  list(
    "title" = title,
    "description" = description,
    "returns" = returns,
    "param" = param,
    "code" = code
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
# create_presentation(package)
