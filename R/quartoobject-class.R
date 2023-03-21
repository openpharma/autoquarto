#' Base Class for All Quarto Objects
#'
#' @slot variables a list containing the names (and values) of the variables required to
#' create the book
#' @export
setClass(
  "QuartoObject",
  slots = c(
    variables = "list"
  ),
  prototype = list(
    variables = list()
  )
)

#' Constructor for the `QuartoObject` class
#'
#' @param variables the initial value of the `variables` slot
#' @export
QuartoObject <- function(variables = list()) {
  methods::new("QuartoObject", variables)
}

setMethod(
  "initialize",
  "QuartoObject",
  function(.Object, variables = list(), ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    .Object@variables <- variables
    .Object
  }
)

#' Base Class for Compound Quarto Objects
#'
#' Compound Quarto objects are books and websites: they contain chapters
#' @slot templateSearchPath a `list` containing the paths to be searched for the
#' template files that define the `chapters` of this object.
#' @slot variables a list containing the names (and values) of the variables required to
#' create the book
#' @slot chapters a list containing the names of the quarto documents that comprise the book
#' @slot type the type of the object (currently book or website, depending on subclass)
#' @export
setClass(
  "QuartoCompoundObject",
  contains = "QuartoObject",
  slots = c(
    templateSearchPath = "list",
    chapters = "list",
    type = "character"
  ),
  prototype = list(
    templateSearchPath = list("."),
    chapters = list(),
    type = NA_character_
  )
)

#' Constructor for the `QuartoCompoundObject` class
#'
#' @param templateSearchPath the initial value of the `templateSearchPath` slot
#' @param chapters the initial value of the `chapters` slot
#' @inheritParams QuartoObject
#' @export
QuartoCompoundObject <- function(templateSearchPath = list("."), variables = list(), chapters = list()) {
  methods::new("QuartoCompoundObject", templateSearchPath, variables, chapters)
}

setMethod(
  "initialize",
  "QuartoCompoundObject",
  function(.Object, templateSearchPath = list("."), variables = list(), chapters = list(), ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    .Object@templateSearchPath <- templateSearchPath
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
  contains = "QuartoCompoundObject"
)

#' Constructor for the `QuartoBook` class
#'
#' @inheritParams QuartoCompoundObject
#' @export
QuartoBook <- function(templateSearchPath = list("."), variables = list(), chapters = list()) {
  methods::new("QuartoBook", templateSearchPath, variables, chapters)
}

setMethod(
  "initialize",
  "QuartoBook",
  function(.Object, templateSearchPath = list("."), variables = list(), chapters = list(), ...) {
    .Object <- methods::callNextMethod(.Object, templateSearchPath, variables, chapters, ...)
    .Object@type <- "book"
    .Object
  }
)

#' An Object Representing a Quarto Website
#'
#' @include quartoobject-class.R
#' @export
setClass(
  "QuartoWebsite",
  contains = "QuartoCompoundObject",
)

#' Constructor for the `QuartoWebsite` class
#'
#' @inheritParams QuartoCompoundObject
#' @export
QuartoWebsite <- function(templateSearchPath = list("."), variables = list(), chapters = list()) {
  methods::new("QuartoWebsite", templateSearchPath, variables, chapters)
}

setMethod(
  "initialize",
  "QuartoWebsite",
  function(.Object, templateSearchPath = list("."), variables = list(), chapters = list(), ...) {
    .Object <- methods::callNextMethod(.Object, templateSearchPath, variables, chapters, ...)
    .Object@type <- "website"
    .Object
  }
)


#' An Object Representing a Quarto Document
#'
#' @include quartoobject-class.R
#' @export
setClass(
  "QuartoDocument",
  contains = "QuartoObject",
  slots = c(
    fileName = "character"
  ),
  prototype = list(
    fileName = NA_character_
  )
)

#' Constructor for the `QuartoDocument` class
#'
#' @inheritParams QuartoObject
#' @param fileName  Path the qmd file that this object represents

#' @export
QuartoDocument <- function(fileName, variables = list()) {
  methods::new("QuartoDocument", fileName, variables)
}

setMethod(
  "initialize",
  "QuartoDocument",
  function(.Object, fileName, variables = list(), ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    .Object@fileName <- fileName
    .Object
  }
)
