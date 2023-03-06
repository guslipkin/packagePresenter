#' Title
#'
#' @param file A file path
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A list of details ready to be collated into slides
#' @keywords internal
.get_datasets <- function(datasets, yaml) {
  lapply(datasets, .get_dataset_tags, yaml)
}

#' Title
#'
#' @param block A roxygen2 block
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A list of roxygen2 tags for a specific source file
#' @keywords internal
.get_dataset_tags <- function(block, yaml) {
  topic <- block$object$topic
  file <- .get_file_from_path(block$file)
  title <- .get_tag(block, "title")
  description <- .get_tag(block, "description")
  format <- .get_tag(block, "format")
  source <- .get_tag(block, "source")
  references <- .get_tag(block, "references")

  dataset_details <-
    list(
      "title" = title,
      "topic" = topic,
      "description" = description,
      "file" = file
    )
  # allow optional items to be dropped
  dataset_details_yaml <-
    list(
      "format" = format,
      "source" = source,
      "references" = references
    )
  dataset_details_yaml <- mapply(
    .drop_options,
    dataset_details_yaml, names(dataset_details_yaml),
    MoreArgs = list(yaml, "datasets")
  ) |>
    `names<-`(names(dataset_details_yaml))
  dataset_details <-
    append(dataset_details, dataset_details_yaml)

  return(dataset_details)
}

#' Title
#'
#' @param dataset_details A list of function details from `.get_dataset_tags`
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.collate_datasets <- function(dataset_details) {
  dataset_details$topic <- .collate_slide(
    "- **Topic:** ",
    dataset_details$topic,
    "`{{content}`",
    fit = FALSE
  ) |>
    paste0(collapse = "")

  dataset_details$description <- .collate_slide(
    "\n\n- **Description:** ",
    dataset_details$description,
    fit = FALSE
  ) |>
    paste0(collapse = "")

  title_details <-
    paste0(
      dataset_details$topic,
      dataset_details$description,
      collapse = ""
    )

  dataset_details$title <- .collate_slide(
    glue::glue("\n\n# {dataset_details$title}"),
    title_details
  )

  dataset_details$format <- .collate_slide(
    "\n\n## Format",
    dataset_details$format
  )

  dataset_details$source <- .collate_slide(
    "\n\n- **Source:** ",
    dataset_details$source,
    fit = FALSE
  ) |>
    paste0(collapse = "")

  dataset_details$references <- .collate_slide(
    "\n\n- **References:** ",
    dataset_details$references,
    fit = FALSE
  ) |>
    paste0(collapse = "")

  bib_details <-
    paste0(
      dataset_details$source,
      dataset_details$references,
      collapse = ""
    )

  bib_details <- .collate_slide(
    glue::glue("\n\n## Bibliography"),
    bib_details
  )

  dataset_contents <- c(
    dataset_details$title,
    dataset_details$format,
    bib_details
  )

  return(dataset_contents)
}
