#' An Object Representing a Quarto Website
#' 
#' @slot templateSearchPath a `list` containing the paths to be searched for the 
#' template files that define the `chapters` of this object.
#' @slot variables a list containing the names (and values) of the variables required to
#' create the book
#' @slot chapters a list containing the names of the quarto documents that comprise the book
#' @export
setClass(
  "QuartoWebsite",
  slots=c(
    templateSearchPath = "list",
    variables = "list",
    chapters = "list"
  ),
  prototype = list(
    templateSearchPath = list("."),
    variables = list(),
    chapters = list()
  )
)

#' Constructor for the `QuartoWebsite` class
#' 
#' @param templateSearchPath the initial value of the `templateSearchPath` slot
#' @param variables the initial value of the `variables` slot
#' @param chapters the initial value of the `chapters` slot
#' @export
QuartoWebsite <- function(templateSearchPath=list("."), variables=list(), chapters=list()) {
  methods::new("QuartoWebsite", templateSearchPath, variables, chapters)
}

setMethod(
  "initialize", 
  "QuartoWebsite",
  function(.Object, templateSearchPath=list("."), variables=list(), chapters=list(), ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    .Object@templateSearchPath <- templateSearchPath
    .Object@variables <- variables
    .Object@chapters <- chapters
    .Object
  }
)