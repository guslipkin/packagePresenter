#' Title
#'
#' @param package A file path to the root directory of an R package source
#'   folder.
#' @param yaml A list of properties from `.parse_yaml()`
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.get_roxygen <- function(package, yaml) {
  p <- roxygen2::parse_package(package)

  if (!is.null(yaml)) {
    p <- .choose_slides(p, yaml$layout)
  }
  return(p)
}

#' Title
#'
#' @param p A list of roxygen2 blocks
#' @param slides A list from `_pkgslides.yml`
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.choose_slides <- function(p, layout) {
  if (is.atomic(layout) & length(layout) == 1) {
    if (layout == "auto") {
      p <- .filter_roxygen_auto(p)
    } else if (layout == "exported") {
      p <- .filter_roxygen_exported(p)
    } else if (layout == "all") {
      # p already is all
    }
  } else {
    p <- .filter_roxygen_custom(p, layout)
  }

  return(p)
}

#' Title
#'
#' @param p A list of roxygen2 blocks
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.filter_roxygen_exported <- function(p) {
  p <- lapply(p, \(p) {
    exported <- roxygen2::block_has_tags(p, "export")
    if (exported) { return(p) }
  })
  p[sapply(p, is.null)] <- NULL

  return(p)
}

#' Title
#'
#' @param p A list of roxygen2 blocks
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.filter_roxygen_auto <- function(p) {
  files <- vapply(p, \(p) {
    exported <- roxygen2::block_has_tags(p, "export")
    if (exported) {
      return(.get_file_from_path(p$file))
    } else { return("") }
  }, FUN.VALUE = "character")
  files <- files[files != ""]

  p <- lapply(p, \(p) {
    if(.get_file_from_path(p$file) %in% files) {
      return(p)
    }
  })
  p[sapply(p, is.null)] <- NULL

  return(p)
}


#' Title
#'
#' @param p A list of roxygen2 blocks
#' @param slides
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.filter_roxygen_custom <- function(p, layout) {
  package_files <-
    vapply(p, \(p) .get_file_from_path(p$file), FUN.VALUE = "character")
  yaml_files <-
    vapply(layout, \(s) s$file, FUN.VALUE = "character")
  yaml_slides <- sapply(layout, \(s) {
    if (is.null(s$slides)) { return("all") }
    return(s$slides)
    }) |>
    `names<-`(yaml_files)

  names(p) <- vapply(p, \(p) p$object$topic, FUN.VALUE = "character")
  p <- p[which(package_files %in% yaml_files)]

  p <- mapply(
    .filter_roxygen_functions,
    yaml_slides, names(yaml_slides),
    MoreArgs = list(p)
  )

  return(p)
}

#' Title
#'
#' @param y A list of yaml slide options
#' @param y_names A list of yaml slide option name
#' @param p A list of roxygen2 blocks
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.filter_roxygen_functions <- function(y, y_names, p) {
  if (any(y == "auto")) {
    p <- .filter_roxygen_blocks(p, y_names)
    internal <- sapply(p, \(p) {
      "internal" %in% roxygen2::block_get_tag_value(p, "keywords")
    })
    if (all(internal)) { p <- NA }
  } else if (any(y == "all")) {
    p <- .filter_roxygen_blocks(p, y_names)
  } else {
    p <- p[which(names(p) %in% y)]
  }

  return(p)
}

#' Title
#'
#' @param p A list of roxygen2 blocks
#' @param y_names A list of yaml slide option name
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.filter_roxygen_blocks <- function(p, y_names) {
  file <- sapply(p, \(p) .get_file_from_path(p$file))
  p <- p[which(file %in% y_names)]
  return(p)
}
