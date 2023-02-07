# addTemplatePath ----

## generic ----

#' Add an Entry to the Template Search Path list
#' 
#' @param x a QuartoWebsite object
#' @param path the path to be added
#' @param mustExist Boolean must the specified path exist?
#' @param .after At what position should the new path be added?  The default value, `NA`, adds the new path at the end of the existing list
#' @docType methods
#' @export
setGeneric("addTemplatePath", function(x, path, mustExist=FALSE, .after=NA) standardGeneric("addTemplatePath"))

## QuartoWebsite

#' Add a path to the template search path
#' @describeIn addTemplatePath  
#' @aliases addTemplatePath-QuartoWebsite
#' @export
setMethod(
  "addTemplatePath", 
  "QuartoWebsite",
  function(x, path, mustExist, .after) {
    checkmate::assertCharacter(path)
    checkmate::assertLogical(mustExist)
    if (mustExist) {
      checkmate::assertDirectoryExists(path)
    }
    if (all(is.na(.after))) {
      .after = length(x@templateSearchPath)
    } else {
      checkmate::assertNumber(.after, lower=1, upper=length(x@templateSearchPath))
    }
    x@templateSearchPath <- append(x@templateSearchPath, path, .after)
    x
  }
)


# removeTemplatePath ----

## generic ----

#' Remove an Entry from the Template Search Path list
#' 
#' @param x a QuartoWebsite object
#' @param path the path to be removed
#' @param pos the position of the path to be removed
#' @section Usage Notes:
#' Either `path` or `pos` must be provided, but not both
#' @docType methods
#' @export
setGeneric("removeTemplatePath", function(x, path=NA, pos=NA) standardGeneric("removeTemplatePath"))

## QuartoWebsite

#' Remove a path from the template search path
#' @describeIn removeTemplatePath  
#' @aliases removeTemplatePath-QuartoWebsite
#' @export
setMethod(
  "removeTemplatePath", 
  "QuartoWebsite",
  function(x, path, pos) {
    if (!is.na(path) & !is.na(pos)) stop("Only one of `path` and `pos` should be given")
    if (!is.na(path)) {
      checkmate::assertCharacter(path)
      if (length(x@templateSearchPath) == 1 & x@templateSearchPath[[1]] == path) {
        x@templateSearchPath <- list()
      } else {
        if (path %in% x@templateSearchPath) {
          x@templateSearchPath[[which(x@templateSearchPath == path)]] <- NULL
        }
      }
    }
    if (!is.na(pos)) {
      checkmate::assertNumber(pos, lower=1, upper=length(x@templateSearchPath))
      x@templateSearchPath <- as.list(x@templateSearchPath[[-pos]])
    }
    x
  }
)

# addChapter ----

## generic ----

#' Add an Entry from the Chapter list
#' 
#' @param x a QuartoWebsite object
#' @param file the file to be added.  See Usage Notes below.
#' @param .after At what position should the new file be added?  The default value, `NA`, adds the new file at the end of the existing list
#' @section Usage Notes:
#' The locations that are searched to locate `file` at the time the `QuartoWebsite`
#' is `publish`ed depend on the way in which `file` is defined.  If `file` is a path, 
#' whether relative or absolute, then only that location is searched.  If the file 
#' is specified using only a base name, then the elements of `templateSearchPath` 
#' are searched, in order, to locate a file with that base name.  Only the first 
#' match is returned.
#' @docType methods
#' @export
setGeneric("addChapter", function(x, file, .after=NA) standardGeneric("addChapter"))

## QuartoWebsite

#' Add a path to the template search path
#' @describeIn addChapter  
#' @aliases addChapter-QuartoWebsite
#' @export
setMethod(
  "addChapter", 
  "QuartoWebsite",
  function(x, file, .after) {
    checkmate::assertCharacter(file)
    if (all(is.na(.after))) {
      .after = length(x@chapters)
    } else {
      checkmate::assertNumber(.after, lower=1, upper=length(x@chapters))
    }
    x@chapters <- append(x@chapters, file, .after)
    x
  }
)

# removeChapter ----

## generic ----

#' Remove an Entry from the Chapter list
#' 
#' @param x a QuartoWebsite object
#' @param file the file to be removed
#' @param pos the position of the file to be removed
#' @section Usage Notes:
#' Either `path` or `pos` must be provided, but not both
#' @docType methods
#' @export
setGeneric("removeChapter", function(x, file=NA, pos=NA) standardGeneric("removeChapter"))

## QuartoWebsite

#' Remove a chapter from the chapter list
#' @describeIn removeChapter  
#' @aliases removeChapter-QuartoWebsite
#' @export
setMethod(
  "removeChapter", 
  "QuartoWebsite",
  function(x, file, pos) {
    if (!is.na(file) & !is.na(pos)) stop("Only one of `path` and `pos` should be given")
    if (!is.na(file)) {
      checkmate::assertCharacter(file)
      if (length(x@chapters) == 1 & x@chapters[[1]] == file) {
        x@chapters <- list()
      } else {
        if (file %in% x@chapters) {
          x@chapters[[which(x@chapters == file)]] <- NULL
        }
      }
    }
    if (!is.na(pos)) {
      checkmate::assertNumber(pos, lower=1, upper=length(x@chapters))
      x@chapters <- as.list(x@chapters[[-pos]])
    }
    x
  }
)

