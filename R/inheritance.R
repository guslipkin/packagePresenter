.resolve_inheritance <- function(p) {
  inherit_types <-
    c("inherit", "inherits", "inheritParams", "inheritDotParams")
  function_names <- sapply(p, \(p) p$object$topic)
  names(p) <- function_names

  inherit_list <-
    lapply(inherit_types, \(inherit_type) {
      inherit_type <- lapply(p, \(b) {
        roxygen2::block_get_tag_value(b, inherit_type)
      }) |>
        `names<-`(function_names) |>
        .drop_null_from_list()
      return(inherit_type)
    }) |>
    `names<-`(inherit_types) |>
    .drop_null_from_list()

  .inherit(inherit_list, p)
}

.inherit <- function(inherit, p) {
  inherit <- inherit_list$inherit
  # i <- inherit_list$inherit$.filter_roxygen_exported
  # n_i <- ".filter_roxygen_exported"
  mapply(\(i, n_i) {
    tag <- roxygen2::roxy_tag(
      tag = i$fields,
      raw = .get_tag(p[[i$source]], i$fields),
      file = p[[n_i]]$file,
    ) |>
      roxygen2::roxy_tag_parse()
    p[[n_i]]$tags[[length(p[[n_i]]$tags) + 1]] <- tag
    b <- roxygen2::roxy_block(
      tags = p[[n_i]]$tags,
      file = p[[n_i]]$file,
      line = p[[n_i]]$line,
      call = p[[n_i]]$call,
      object = p[[n_i]]$object
    )
    p[[n_i]] <<- b
  }, inherit, names(inherit))
}

.inherit_params <- function(inherit, p) {

}

.inherit_dot_params <- function(inherit, p) {

}

i <- 1

for(n in 1:10) {
  i <- i + n
}
i
