.get_title <- function(package) {
  desc_file <- glue::glue("{package}/DESCRIPTION")

  lib <- desc::desc_get("Package", desc_file)
  version <- desc::desc_get_version(desc_file)

  list(
    "lib" = lib,
    "version" = version
  )
}

.collate_title <- function(title_details) {
  title_contents <- c(
    "---",
    glue::glue("title: {title_details$lib}"),
    glue::glue("subtitle: {title_details$version}"),
    "format:",
    "  revealjs:",
    "    navigation-mode: vertical",
    "    self-contained: true",
    "---"
  )
}
