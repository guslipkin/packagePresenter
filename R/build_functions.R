#' Title
#'
#' @param file A file path
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.get_functions <- function(package, yaml) {
  # file <- paste0(r_files, "/", package_functions)[4]
  # f <- roxygen2::parse_file(file)
  function_contents <-
    .get_roxygen(package, yaml) |>
    lapply(.get_tag_list, yaml) |>
    lapply(.collate_functions) |>
    unlist()
  return(function_contents)
}

#' Gets a List of Roxygen2 Tags
#'
#' @param block A roxygen2 block
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A list of roxygen2 tags for a specific source file
#' @keywords internal
.get_tag_list <- function(block, yaml) {
  # block has a name so we need to drop it
  # this also ensures block has length 1
  block <- block[[1]]
  topic <- block$object$topic
  file <- .get_file_from_path(block$file)
  title <- .get_tag(block, "title")
  description <- .get_tag(block, "description")
  return <- .get_tag(block, c("return", "returns"))
  examples <- .get_tag(block, "examples")

  c("description", "returns", "parameters", "examples", "code",  "tests")

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
    .drop_function_options,
    function_details_yaml, names(function_details_yaml),
    MoreArgs = list(yaml)
    ) |>
    `names<-`(names(function_details_yaml))
  function_details <-
    append(function_details, function_details_yaml)

  return(function_details)
}

#' Title
#'
#' @param block A roxygen2 block
#' @param tag A character vector of roxygen2 block tag names
#'
#' @return Returns the raw text of the desired tag
#' @keywords internal
.get_tag <- function(block, tag) {
  tag <- roxygen2::block_get_tag_value(block, tag)
  return(tag)
}

#' Title
#'
#' @param item A list item from `.get_tag_list`
#' @param item_name The name of a list item from `.get_tag_list`
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return Either `item` or `NULL`
#' @keywords internal
.drop_function_options <- function(item, item_name, yaml) {
  if (yaml$format$functions[[item_name]]) { return(item) }
  return(NULL)
}

#' Title
#'
#' @param function_details A list of function details from `.get_tag_list`
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
    "```{r}\n#| echo: true{{content}\n```"
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

#' Collates a Slide with a Header and Content
#'
#' @param header A length-one character vector. These usually start with
#'   `\n\n##`
#' @param content A length-one character vector with the function_details for
#'   the slide
#' @param string A length-one character vector formatted to be used in
#'   `glue::glue(string, .open = "{{")` where the object being replaced is
#'   `content`. This is done to avoid needing to reach outside the function
#'   environment to access `content`.
#' @param fit A logical if `r-fit-text` should be applied to the content
#'   supplied
#'
#' @return A length-two character vector
#' @keywords internal
.collate_slide <- function(header, content, string = "{{content}", fit = TRUE) {
  if (is.null(content)) { return("") }

  string <- glue::glue(string, .open = "{{")

  if (fit) { string <- .fit_content(string) }

  content <- c(
    header,
    string
  )
  return(content)
}

#' Title
#'
#' @param param A function parameter from an roxygen2 tag
#'
#' @return A length-one character vector of parameters formatted as a bulleted
#'   list for writing to a file
#' @keywords internal
.process_params <- function(param) {
  if (is.null(param)) { return(NULL) }
  sapply(param, \(p) {
    glue::glue("- `{p$name}`: {p$param_description}")
  }) |>
    paste0(collapse = "\n")
}
