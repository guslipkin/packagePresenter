
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgslides

<!-- badges: start -->
<!-- badges: end -->

The goal of `pkgslides` is to make it easier for you to learn about R
packages by converting existing documentation into an easily digested
slideshow.

## Installation

You can install the development version of pkgslides from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("guslipkin/pkgslides")
```

## Example

This is a basic example which will build and render a [Quarto
RevealJS](https://quarto.org/docs/presentations/revealjs/) presentation
for `pkgslides`:

``` r
pkgslides::build_presentation()
#> Config written to './_pkgslides.yml'
#> ℹ Loading pkgslides
#> /Users/guslipkin/Documents/GitHub/pkgslides/pkgslides.qmd
#> [31m
#> 
#> processing file: pkgslides.qmd
#> [39m  |                                                                              |                                                                      |   0%  |                                                                              |.......................                                               |  33%
#>   ordinary text without R code
#> 
#>   |                                                                              |...............................................                       |  67%
#> label: unnamed-chunk-1 (with options) 
#> List of 1
#>  $ echo: logi FALSE
#> 
#>   |                                                                              |......................................................................| 100%
#>   ordinary text without R code
#> 
#> 
#> [31moutput file: pkgslides.knit.md
#> 
#> [39m[1mpandoc [22m
#>   to: revealjs
#>   output-file: pkgslides.html
#>   standalone: true
#>   self-contained: true
#>   wrap: none
#>   default-image-extension: png
#>   html-math-method:
#>     method: mathjax
#>     url: >-
#>       https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML-full
#>   slide-level: 2
#>   
#> [1mmetadata[22m
#>   link-citations: true
#>   width: 1050
#>   height: 700
#>   margin: 0.1
#>   center: false
#>   navigationMode: default
#>   controlsLayout: edges
#>   controlsTutorial: false
#>   hash: true
#>   history: true
#>   hashOneBasedIndex: false
#>   fragmentInURL: false
#>   transition: none
#>   backgroundTransition: none
#>   pdfSeparateFragments: false
#>   lang: en
#>   auto-stretch: true
#>   title: pkgslides
#>   subtitle: 0.1.0
#>   theme: default
#>   scrollable: true
#>   
#> Output created: pkgslides.html
```
