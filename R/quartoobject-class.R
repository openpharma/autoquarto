#' An Object Representing a Quarto Object
#' 
#' @slot templateSearchPath a `list` containing the paths to be searched for the 
#' template files that define the `chapters` of this object.
#' @slot variables a list containing the names (and values) of the variables required to
#' create the book
#' @slot chapters a list containing the names of the quarto documents that comprise the book
#' @slot type the type of the object (currently book or website, depending on subclass)
#' @export
setClass(
  "QuartoObject",
  slots=c(
    templateSearchPath = "list",
    variables = "list",
    chapters = "list",
    type = "character"
  ),
  prototype = list(
    templateSearchPath = list("."),
    variables = list(),
    chapters = list(),
    type = NA_character_
  )
)

#' Constructor for the `QuartoObject` class
#' 
#' @param templateSearchPath the initial value of the `templateSearchPath` slot
#' @param variables the initial value of the `variables` slot
#' @param chapters the initial value of the `chapters` slot
#' @export
QuartoObject <- function(templateSearchPath=list("."), variables=list(), chapters=list()) {
  methods::new("QuartoWebsite", templateSearchPath, variables, chapters)
}

setMethod(
  "initialize", 
  "QuartoObject",
  function(.Object, templateSearchPath=list("."), variables=list(), chapters=list(), ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    .Object@templateSearchPath <- templateSearchPath
    .Object@variables <- variables
    .Object@chapters <- chapters
    .Object
  }
)

#' An Object Representing a Quarto Book
#' 
#' @include quartoobject-class.R
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

#' An Object Representing a Quarto Website
#' 
#' @slot templateSearchPath a `list` containing the paths to be searched for the 
#' template files that define the `chapters` of this object.
#' @slot variables a list containing the names (and values) of the variables required to
#' create the book
#' @slot chapters a list containing the names of the quarto documents that comprise the book
#' @include quartoobject-class.R
#' @export
setClass(
  "QuartoWebsite",
  contains="QuartoObject",
  # slots=c(
  #   templateSearchPath = "list",
  #   variables = "list",
  #   chapters = "list",
  #   type = "character"
  # ),
  prototype = list(
    templateSearchPath = list("."),
    variables = list(),
    chapters = list(),
    type = "website"
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
    # .Object@templateSearchPath <- templateSearchPath
    # .Object@variables <- variables
    # .Object@chapters <- chapters
    # .Object@type <- "website"
    .Object
  }
)