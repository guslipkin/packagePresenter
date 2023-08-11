#' Title
#'
#' @inheritParams build_presentation
#'
#' @return A list of roxygen2 properties for the package's DESCRIPTION file used
#'   to create the package description slide
#' @keywords internal
.get_description <- function(package, credits) {
  lib <- .get_desc(package, "Package")
  title <- .get_desc(package, "Title")
  description <- .get_desc(package, "Description")

  list(
    "lib" = lib,
    "title" = title,
    "description" = description,
    "credits" = credits
  )
}

#' Title
#'
#' @param package_details A list of function details from `.get_description`
#'
#' @return A character vector of properties formatted for writing to a file
#' @keywords internal
.collate_description <- function(package_details, chunk_opt) {
  description_header <- glue::glue("\n\n## {package_details$title}")

  package_details$description <- .fit_content(package_details$description)

  package_details$lib <-
    glue::glue("\n\n```{r}\n#| {{chunk_opt}: false\nlibrary({{package_details$lib})\n```\n", .open = "{{")

  package_details$script <- glue::glue("
  <script type='text/javascript' charset='utf-8'>
    function add_author_footer() {
      Reveal.on( 'slidechanged' , event => {
          let footer = document.querySelector('div.footer p');
          if (event.currentSlide.matches('#title-slide')) {
            footer.innerHTML = '{{package_details$credits}}';
          } else {
            footer.innerHTML = '';
          }
        });
    };
    window.addEventListener('load', (event) => {
      add_author_footer();
    });
  </script>
  ", .open = "{{", .close = "}}")

  package_contents <- c(
    description_header,
    package_details$description,
    package_details$lib,
    package_details$script
  )

  return(package_contents)
}
