#' An Object Representing a Quarto Book
#' 
#' @include quartowebsite-class.R
#' @export
setClass(
  "QuartoBook",
  contains = "QuartoObject"
)

#' Constructor for the `QuartoBook` class
#' 
#' @param templateSearchPath the initial value of the `templateSearchPath` slot
#' @param variables the initial value of the `variables` slot
#' @param chapters the initial value of the `chapters` slot
#' @export
QuartoBook <- function(templateSearchPath=list("."), variables=list(), chapters=list()) {
  methods::new("QuartoBook", templateSearchPath, variables, chapters)
}

setMethod(
  "initialize",
  "QuartoBook",
  function(.Object, ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    .Object@type <- "book"
    .Object
  }
)