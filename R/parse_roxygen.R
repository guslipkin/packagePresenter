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

  type <-
    vapply(p, \(x) { class(x$object)[1] }, FUN.VALUE = "character")

  p <-
    list(
      "functions" = p[which(type == "function")],
      "datasets" = p[which(type == "data")]
    )

  p$functions <-
    p$functions |>
    .choose_functions(yaml$functions) |>
    .organize_functions()

  p$datasets <-
    p$datasets |>
    .choose_datasets(yaml$datasets) |>
    .organize_datasets()

  return(p)
}

#' Title
#'
#' @param p A list of roxygen2 blocks
#'
#' @return The list p in the same file order, but functions are organized by
#'   exported first
#' @keywords internal
.organize_functions <- function(p) {
  files <-
    p |>
    sapply(\(b) .get_file_from_path(b$file)) |>
    unname()
  unique_files <- unique(files)

  p <-
    unique_files |>
    lapply(\(f) {
      temp_p <- p[which(files == f)]
      list(
        .filter_roxygen_exported(temp_p, exported = TRUE),
        .filter_roxygen_exported(temp_p, exported = FALSE)
      ) |>
        unlist(recursive = FALSE)
    }) |>
    unlist(recursive = FALSE)

  return(p)
}

#' Title
#'
#' @param p A list of roxygen2 blocks
#' @param exported A boolean if you want exported or non-exported functions
#'   returned
#'
#' @inherit .get_roxygen return
#' @keywords internal
.filter_roxygen_exported <- function(p, exported = TRUE) {
  p <- lapply(p, \(p) {
    p_exported <- roxygen2::block_has_tags(p, "export")
    if (exported & p_exported) { return(p) }
    else if (!exported & !p_exported) { return(p) }
    return(NULL)
  }) |>
    .drop_null_from_list()

  return(p)
}

#' Title
#'
#' @param p A list of roxygen2 blocks
#' @param slides A list from `_pkgslides.yml`
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.choose_functions <- function(p, functions) {
  if (is.atomic(functions) & length(functions) == 1) {
    if (functions == "auto") {
      p <- .filter_roxygen_auto(p)
    } else if (functions == "exported") {
      p <- .filter_roxygen_exported(p)
    } else if (functions == "all") {
      # p already is all
    }
  } else {
    p <-
      .filter_roxygen_custom(p, functions) |>
      unlist(recursive = FALSE) |>
      unname()
  }

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
#' @param chosen A vector of the chosen functions or dataset names
#'
#' @return A list of roxygen2 blocks
#' @keywords internal
.filter_roxygen_custom <- function(p, chosen) {
  package_files <-
    vapply(p, \(p) .get_file_from_path(p$file), FUN.VALUE = "character")
  yaml_files <-
    vapply(chosen, \(s) s$file, FUN.VALUE = "character")
  yaml_slides <- sapply(chosen, \(s) {
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

.choose_datasets <- function(p, datasets) {
  if (is.atomic(datasets) & length(datasets) == 1) {
    if (datasets == "none") {
      p <- NULL
    } else if (datasets == "all") {
      # p already is all
    }
  } else {
    p <-
      .filter_roxygen_custom(p, datasets) |>
      unlist(recursive = FALSE) |>
      unname()
  }

  return(p)
}

.organize_datasets <- function(p) {
  data_order <-
    vapply(p, \(x) { x$call }, FUN.VALUE = "character") |>
    order()

  p <- p[data_order]

  return(p)
}
