#' Title
#'
#' @param path A file path
#'
#' @return A length-one character vector with the last portion of the supplied
#'   path
#' @keywords internal
.get_file_from_path <- function(path) {
  rev(strsplit(path, '/')[[1]])[1]
}

#' Get a DESCRIPTION Property
#'
#' @param package A file path to the root directory of an R package source
#'   folder.
#' @param tag The name of the tag whose contents are desired
#'
#' @return A length-one character vector with the desired text from a package's
#'   DESCRIPTION file
#' @keywords internal
.get_desc <- function(package, tag) {
  desc::desc_get(tag, glue::glue("{package}/DESCRIPTION"))
}

#' Title
#'
#' @param block A roxygen2 block
#' @param tag A character vector of roxygen2 block tag names
#'
#' @return Returns the raw text of the desired tag
#' @keywords internal
.get_tag <- function(block, tag) {
  roxygen2::block_get_tag_value(block, tag)
}

#' Title
#'
#' @param item A list item from `.get_*_tags`
#' @param item_name The name of a list item from `.get_*_tags`
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return Either `item` or `NULL`
#' @keywords internal
.drop_options <- function(item, item_name, yaml, type) {
  if (yaml$format[[type]][[item_name]]) { return(item) }
  return(NULL)
}

#' Collates a Slide with a Header and Content
#'
#' @param header A length-one character vector. These usually start with
#'   `\\n\\n##`
#' @param content A length-one character vector with the function_details for
#'   the slide
#' @param string A length-one character vector formatted to be used in
#'   `glue::glue(string, .open = "\{\{")` where the object being replaced is
#'   `content`. This is done to avoid needing to reach outside the function
#'   environment to access `content`.
#' @param fit A logical if `r-fit-text` should be applied to the content
#'   supplied
#'
#' @return A length-two character vector
#' @keywords internal
.collate_slide <- function(header, content, string = "{{content}", fit = TRUE) {
  if (is.null(content)) { return("") }

  content <-
    tryCatch(
    {
      content <-
        content |>
        pkgdown::rd2html() |>
        paste0(collapse = "\n")
    },
    error = function(cond) {
      message("There's something weird about your documentation. This seems to happen when there is LaTeX written into the roxygen2 blocks.")
      print(cond)

      content <-
        content |>
        stringr::str_extract_all("(?<=\\{).+?(?=\\})") |>
        unlist() |>
        paste(collapse = " ")

      return(content)
    }
  )

  string <- glue::glue(string, .open = "{{")

  if (fit) { string <- .fit_content(string) }

  content <- c(
    header,
    string
  )

  return(content)
}

#' Puts Content in an `r-fit-content` div
#'
#' @param content A length-one character vector
#'
#' @return A length-one character vector that renders a Quarto `r-fit-content`
#'   div
#' @keywords internal
.fit_content <- function(content) {
  glue::glue("\n\n::: {.r-fit-text}\n\n{{content}\n\n:::", .open = "{{")
}

#' Title
#'
#' @param l A list
#'
#' @return A list with null values removed
#' @keywords internal
.drop_null_from_list <- function(l) {
  l[sapply(l, is.null)] <- NULL
  if (length(l) == 0) { return(NULL) }
  return(l)
}
