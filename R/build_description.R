.get_description <- function(package) {
  desc_file <- glue::glue("{package}/DESCRIPTION")

  lib <- desc::desc_get("Package", desc_file)
  title <- desc::desc_get("Title", desc_file)
  description <- desc::desc_get("Description", desc_file)

  list(
    "lib" = lib,
    "title" = title,
    "description" = description
  )
}

.collate_description <- function(package_details) {
  description_header <- glue::glue("\n\n## {package_details$title}")

  package_details$lib <- glue::glue("\n\n```{r}\n#| echo: false\nlibrary({{package_details$lib})\n```", .open = "{{")

  package_contents <- c(
    description_header,
    package_details$description,
    package_details$lib
  )

  return(package_contents)
}
