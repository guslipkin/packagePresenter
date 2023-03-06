#' Title
#'
#' @param functions A list of roxygen2 blocks for functions
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A list of details ready to be collated into slides
#' @keywords internal
.get_functions <- function(functions, yaml) {
  lapply(functions, .get_function_tags, yaml)
}

#' Gets a List of Roxygen2 Tags
#'
#' @param block A roxygen2 block
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A list of roxygen2 tags for a specific source file
#' @keywords internal
.get_function_tags <- function(block, yaml) {
  topic <- block$object$topic
  file <- .get_file_from_path(block$file)
  title <- .get_tag(block, "title")
  description <- .get_tag(block, "description")
  return <- .get_tag(block, c("return", "returns"))
  examples <- .get_tag(block, "examples")

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

  function_details <-
    list(
      "title" = title,
      "topic" = topic,
      "description" = description,
      "return" = return,
      "file" = file
    )
  # allow optional items to be dropped
  function_details_yaml <-
    list(
      "parameters" = param,
      "examples" = examples,
      "code" = code
    )
  function_details_yaml <- mapply(
    .drop_options,
    function_details_yaml, names(function_details_yaml),
    MoreArgs = list(yaml, "functions")
    ) |>
    `names<-`(names(function_details_yaml))
  function_details <-
    append(function_details, function_details_yaml)

  return(function_details)
}

#' Title
#'
#' @param function_details A list of function details from `.get_function_tags`
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.collate_functions <- function(function_details) {
  function_details$topic <- .collate_slide(
    "- **Topic:** ",
    function_details$topic,
    "`{{content}`",
    fit = FALSE
  ) |>
    paste0(collapse = "")

  function_details$description <- .collate_slide(
      "\n\n- **Description:** ",
      function_details$description,
      fit = FALSE
    ) |>
    paste0(collapse = "")

  function_details$return <- .collate_slide(
      "\n\n- **Return:** ",
      function_details$return,
      fit = FALSE
    ) |>
    paste0(collapse = "")

  title_details <-
    paste0(
      function_details$topic,
      function_details$description,
      function_details$return,
      collapse = ""
    )

  function_details$title <- .collate_slide(
      glue::glue("\n\n# {function_details$title}"),
      title_details
    )

  function_details$parameters <- .collate_slide(
    "\n\n## Parameters",
    .process_params(function_details$parameters)
    )

  function_details$examples <- .collate_slide(
    "\n\n## Examples",
    function_details$examples,
    "```{r}\n#| echo: true\n{{content}\n```"
    )

  function_details$code <- .collate_slide(
    glue::glue("\n\n## `{function_details$file}`"),
    function_details$code,
    "```{.r}\n{{content}\n```"
    )

  function_contents <- c(
    function_details$title,
    function_details$parameters,
    function_details$examples,
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
  if (is.null(param)) {
    return(NULL)
  } else if (!all(sapply(param, is.list))) {
    param <- list(param)
  }
  sapply(param, \(p) {
    glue::glue("- `{p$name}`: {p$param_description}")
  }) |>
    paste0(collapse = "\n")
}
