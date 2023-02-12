#' Title
#'
#' @param file A file path
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.get_functions <- function(file) {
  # file <- paste0(r_files, "/", package_functions)[1]
  f <- roxygen2::parse_file(file)

  function_contents <-
    .get_tags(f[[1]]) |>
    .collate_functions()

  return(function_contents)
}

#' Title
#'
#' @param block A roxygen2 block
#'
#' @return A list of roxygen2 properties for a specific source file
#' @keywords internal
.get_tags <- function(block) {
  file <- rev(strsplit(block$file, '/')[[1]])[1]
  title <- roxygen2::block_get_tags(block, "title")[[1]]$raw
  description <- roxygen2::block_get_tags(block, "description")[[1]]$raw
  # description <- "\n'test'"
  return <- roxygen2::block_get_tags(block, c("return", "returns"))[[1]]$raw
  examples <- roxygen2::block_get_tags(block, "examples")[[1]]$raw
  # examples <- "\n'test'"

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
    "file" = file,
    "title" = title,
    "description" = description,
    "returns" = return,
    "param" = param,
    "examples" = examples,
    "code" = code
  )
}

#' Title
#'
#' @param function_details A list of function details from `.get_tags`
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.collate_functions <- function(function_details) {
  function_details$title <- glue::glue("\n\n# {function_details$title}")

  function_details$description <- glue::glue("- **Description**: {function_details$description}")

  function_details$return <- glue::glue("- **Return**: {function_details$return}")

  param_header <- "\n\n## Parameters"
  function_details$param <- .process_params(function_details$param)

  examples_header <- "\n\n## Examples"
  function_details$examples <- glue::glue("```{r}\n#| echo: true{{function_details$examples}\n```", .open = "{{")

  function_file_header <- glue::glue("\n\n## `{function_details$file}`")
  function_details$code <- glue::glue("```{.r}\n{{function_details$code}\n```", .open = "{{")

  function_contents <- c(
    function_details$title,
    function_details$description,
    function_details$return,
    param_header,
    function_details$param,
    examples_header,
    function_details$examples,
    function_file_header,
    function_details$code
  )

  return(function_contents)
}

#' Title
#'
#' @param param A function parameter from an roxygen2 tag
#'
#' @return A length-one character vector of parameters formatted as a bulleted
#'   list for writing to a file
#' @keywords internal
.process_params <- function(param) {
  sapply(param, \(p) {
    glue::glue("- `{p$name}`: {p$param_description}")
  }) |>
    paste0(collapse = "\n")
}
