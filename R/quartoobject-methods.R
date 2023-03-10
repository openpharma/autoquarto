# addTemplatePath ----

## generic ----

#' Add an Entry to the Template Search Path list
#'
#' @param x a QuartoCompoundObject object
#' @param path the path to be added
#' @param mustExist Boolean must the specified path exist?
#' @param .after At what position should the new path be added?  The default value, `NA`, adds the new path at the end of the existing list
#' @docType methods
#' @export
setGeneric("addTemplatePath", function(x, path, mustExist = FALSE, .after = NA) standardGeneric("addTemplatePath"))

## QuartoCompoundObject

#' Add a path to the template search path
#' @aliases addTemplatePath-QuartoCompoundObject
#' @rdname addTemplatePath
#' @export
setMethod(
  "addTemplatePath",
  "QuartoCompoundObject",
  function(x, path, mustExist, .after) {
    checkmate::assertCharacter(path)
    checkmate::assertLogical(mustExist)
    if (mustExist) {
      checkmate::assertDirectoryExists(path)
    }
    if (all(is.na(.after))) {
      .after <- length(x@templateSearchPath)
    } else {
      checkmate::assertNumber(.after, lower = 1, upper = length(x@templateSearchPath))
    }
    x@templateSearchPath <- append(x@templateSearchPath, path, .after)
    x
  }
)


# removeTemplatePath ----

## generic ----

#' Remove an Entry from the Template Search Path list
#'
#' @param x a QuartoCompoundObject object
#' @param path the path to be removed
#' @param pos the position of the path to be removed
#' @section Usage Notes:
#' Either `path` or `pos` must be provided, but not both
#' @docType methods
#' @export
setGeneric("removeTemplatePath", function(x, path = NA, pos = NA) standardGeneric("removeTemplatePath"))

## QuartoCompoundObject

#' Remove a path from the template search path
#' @aliases removeTemplatePath-QuartoCompoundObject
#' @rdname removeTemplatePath
#' @export
setMethod(
  "removeTemplatePath",
  "QuartoCompoundObject",
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
      checkmate::assertNumber(pos, lower = 1, upper = length(x@templateSearchPath))
      x@templateSearchPath <- as.list(x@templateSearchPath[[-pos]])
    }
    x
  }
)

# addChapter ----

## generic ----

#' Add an Entry from the Chapter list
#'
#' @param x a QuartoCompoundObject object
#' @param file the file to be added.  See Usage Notes below.
#' @param .after At what position should the new file be added?  The default value, `NA`, adds the new file at the end of the existing list
#' @section Usage Notes:
#' The locations that are searched to locate `file` at the time the `QuartoCompoundObject`
#' is `publish`ed depend on the way in which `file` is defined.  If `file` is a path,
#' whether relative or absolute, then only that location is searched.  If the file
#' is specified using only a base name, then the elements of `templateSearchPath`
#' are searched, in order, to locate a file with that base name.  Only the first
#' match is returned.
#' @docType methods
#' @export
setGeneric("addChapter", function(x, file, .after = NA) standardGeneric("addChapter"))

## QuartoCompoundObject

#' Add a chapter to a QuartoCompoundObject
#' @aliases addChapter-QuartoCompoundObject
#' @rdname addChapter
#' @export
setMethod(
  "addChapter",
  "QuartoCompoundObject",
  function(x, file, .after) {
    checkmate::assertCharacter(file, min.len = 1, max.len = 1)
    if (all(is.na(.after))) {
      .after <- length(x@chapters)
    } else {
      checkmate::assertNumber(.after, lower = 1, upper = length(x@chapters))
    }
    x@chapters <- append(x@chapters, file, .after)
    x
  }
)

# removeChapter ----

## generic ----

#' Remove an Entry from the Chapter list
#'
#' @param x a QuartoCompoundObject object
#' @param file the file to be removed
#' @param pos the position of the file to be removed
#' @section Usage Notes:
#' Either `path` or `pos` must be provided, but not both
#' @docType methods
#' @export
setGeneric("removeChapter", function(x, file = NA, pos = NA) standardGeneric("removeChapter"))

## QuartoCompundObject

#' Remove a chapter from the chapter list
#' @aliases removeChapter-QuartoCompoundObject
#' @rdname removeChapter
#' @export
setMethod(
  "removeChapter",
  "QuartoCompoundObject",
  function(x, file, pos) {
    if (all(!is.na(c(file, pos)))) stop("Only one of `path` and `pos` should be given")
    if (any(!is.na(file))) {
      checkmate::assertCharacter(file, max.len=1)
      if (length(x@chapters) == 1 & x@chapters[[1]] == file) {
        x@chapters <- list()
      } else {
        if (file %in% x@chapters) {
          x@chapters[[which(x@chapters == file)]] <- NULL
        }
      }
    }
    if (any(!is.na(pos))) {
      checkmate::assertNumber(pos, lower = 1, upper = length(x@chapters))
      if (length(x@chapters) == 1) {
        x@chapters <- list()
      } else {
        x@chapters[[which(x@chapters == x@chapters[[pos]])]] <- NULL
        # Why does this not work?
        # x@chapters <- as.list(x@chapters[[-pos]])
      }
    }
    x
  }
)

# locateTemplate ----

## generic ----

#' Search the `templateSearchPath` for the First Matching Template File
#'
#' @param x a QuartoCompoundObject object
#' @param fileName the (base) name of the file to be searched for along the
#' `templateSearchPath` defined by `x`.
#' @param ... passed to class methods
#' @section Usage Notes:
#' If `basename(fileName) == fileName` then `x@templateSearchPath` is searched, in order, for
#' a file with the given name.  The first match is returned.  Otherwise,
#' the absolute path to the given file is returned.  This process allows (say) global,
#' project and study level templates to be obtained.
#' @docType methods
#' @export
setGeneric("locateTemplate", function(x, fileName = NA, ...) standardGeneric("locateTemplate"))

## QuartoCompoundObject

#' Search the `templateSearchPath` for the First Matching Template File
#' @param strict Boolean.  Default: TRUE.  Check that the file found (a) exists
#' (b) is readable and (c) has one of the `allowedExtensions`
#' @param allowedExtensions Default: `"qmd"`.  A vector of permitted extensions.
#' Ignored if `strict` is `FALSE`
#' @aliases locateTemplate-QuartoCompoundObject
#' @rdname locateTemplate
#' @export
setMethod(
  "locateTemplate",
  "QuartoCompoundObject",
  function(x, fileName, strict = TRUE, allowedExtensions = c("qmd")) {
    # Validate
    checkmate::assertCharacter(fileName)
    checkmate::assertLogical(strict)
    checkmate::assertCharacter(allowedExtensions)
    checkmate::assertScalar(fileName)
    checkmate::assertScalar(strict)
    foundFile <- NULL
    # Check for simple base name
    if (fileName == basename(fileName)) {
      for (dir in x@templateSearchPath) {
        foundFile <- R.utils::getAbsolutePath(file.path(dir, fileName))
        if (checkmate::testFileExists(foundFile)) break
        foundFile <- NULL
      }
    } else {
      # Cater for relative paths
      foundFile <- R.utils::getAbsolutePath(fileName)
    }
    if (is.null(foundFile)) stop(paste0("Unable to locate ", fileName))
    if (strict) {
      checkmate::assertFileExists(foundFile, access = "r", extension = allowedExtensions)
    }
    foundFile
  }
)

# publish ----

## generic ----

#' Publish a Quarto object
#'
#' @param x a QuartoObject object
#' @param outFile the name of the file containing the published Quarto object
#' @param ... passed to class methods
#' @docType methods
#' @export
setGeneric("publish", function(x, outFile, ...) standardGeneric("publish"))

## QuartoCompoundObject

#' Publish a QuartoCompoundObject object
#' @aliases publish-QuartoCompoundObject
#' @param params Default `list()`.  A list containing the parameters to use when
#' publishing the QuartoCompoundObject
#' @param workDir Default: `NULL`.  The temporary work folder used to generate the report.
#' If `NULL` a temporary work folder with a random name, is used.
#' @param logFile Default: NA.  The name of the log file that documents the publishing process.
#' If `NA`, the name is based on `basename(outFile)`, with the start date/time appended and
#' located in the folder returned by `here::here()` at the time the function was called.
#' If `NULL`, no log file is produced.
#' @param tidyUp Boolean.  Should the workDir be deleted on successful publication?
#' @rdname publish
#' @examples
#' \dontrun{
#' publish(
#'   QuartoBook(
#'     templateSearchPath = list(testthat::test_path("testData", "global")),
#'     chapters = list(
#'       "index.qmd",
#'       testthat::test_path("testData", "testParams", "introParams.qmd")
#'     )
#'   ),
#'   outFile = "testParamsOutput",
#'   workDir = testthat::test_path("testData", "_work"),
#'   params = params
#' )
#' }
#' @export
setMethod(
  "publish",
  "QuartoCompoundObject",
  function(x, outFile, params = list(), workDir = NULL, tidyUp = TRUE, logFile = NA) {
    futile.logger::flog.debug("Entry - QuartoCompoundObject")
    # Validate
    checkmate::assertPathForOutput(outFile)
    checkmate::assertList(params, names = "unique")
    checkmate::assertCharacter(workDir, len = 1)
    if (!is.null(workDir)) checkmate::assertDirectoryExists(workDir, access = "rw")
    checkmate::assertLogical(tidyUp)
    workDir <- .prepareToPublish(workDir, outFile, logFile)
    # Execute
    futile.logger::flog.info("Processing chapter files...")
    invisible(
      lapply(
        x@chapters,
        function(c) {
          futile.logger::flog.info(paste0("Processing ", c, "..."))
          f <- locateTemplate(x, c)
          futile.logger::flog.debug(paste0("Copying '", f, "' to '", workDir, "'..."))
          file.copy(f, workDir)
          futile.logger::flog.debug("Updating parameter YAML block...")
          replaceYamlParams(file.path(workDir, basename(c)), params)
          futile.logger::flog.debug("Done")
        }
      )
    )
    .processProjectYAML(x, workDir, outFile, params)
    .renderQuartoCompoundObject(workDir)
    if (tidyUp) {
      futile.logger::flog.info(paste0("Cleaning ", workDir, "..."))
      workFiles <- list.files(workDir, full.names = TRUE)
      if (length(workFiles) > 0) {
        x <- file.remove(workFiles)
        futile.logger::flog.info("Done")
      }
    } else {
      futile.logger::flog.info("Nothing to do")
    }
  }
)
## QuartoDocument

#' Publish a QuartoDocument object
#' @aliases publish-QuartoDocument
#' @param params Default `list()`.  A list containing the parameters to use when
#' publishing the QuartoDocument
#' @param workDir Default: `NULL`.  The temporary work folder used to generate the report.
#' If `NULL` a temporary work folder with a random name, is used.
#' @param logFile Default: NA.  The name of the log file that documents the publishing process.
#' If `NA`, the name is based on `basename(outFile)`, with the start date/time appended and
#' located in the folder returned by `here::here()` at the time the function was called.
#' If `NULL`, no log file is produced.
#' @param tidyUp Boolean.  Should the workDir be deleted on successful publication?
#' @param ... passed to `quarto::quarto_render`
#' @rdname publish
#' @examples
#' \dontrun{
#' publish(
#'   QuartoDocument(testthat::test_path("testData", "testParams", "introParams.qmd")),
#'   outFile = "testParamsOutputDocument",
#'   workDir = testthat::test_path("testData", "_work"),
#'   params = list(
#'     dataName = "mtcars",
#'     rowCount = 10,
#'     x = "wt",
#'     y = "mpg",
#'     g = "cyl",
#'     plotTitle = "A title for the plot"
#'   )
#' )
#' }
#' @export
setMethod(
  "publish",
  "QuartoDocument",
  function(x, outFile, params = list(), workDir = NULL, tidyUp = FALSE, logFile = NA, ...) {
    futile.logger::flog.debug("Entry - QuartoDocument")
    # Validate
    checkmate::assertPathForOutput(outFile)
    checkmate::assertLogical(tidyUp)
    workDir <- .prepareToPublish(workDir, outFile, logFile)
    futile.logger::flog.debug(paste0("Copying '", x@fileName, "' to '", workDir, "'..."))
    file.copy(x@fileName, workDir)
    futile.logger::flog.debug("Updating parameter YAML block...")
    replaceYamlParams(file.path(workDir, basename(x@fileName)), params)
    quarto::quarto_render(file.path(workDir, basename(x@fileName)), output_file = outFile)
    futile.logger::flog.debug("Done")
  }
)

# quartoYML ----

## generic ----

#' Obtain the _quarto.yml Component of the Quarto Object
#'
#' @param x a QuartoObject object
#' @param ... passed to class methods
#' @docType methods
#' @export
setGeneric("quartoYML", function(x, ...) standardGeneric("quartoYML"))

## QuartoObject

#' Obtain the _quarto.yml Component of the Quarto Object
#' @param x a QuartoObject object
#' @aliases quartoYML-QuartoObject
#' @rdname quartoYAML
#' @export
setMethod(
  "quartoYML",
  "QuartoObject",
  function(x) {
    y <- ymlthis::yml(author = FALSE, date = FALSE) %>%
      ymlthis::yml_toplevel(list("project" = list("type" = x@type)))
    typeList <- list()
    typeList[[x@type]] <- list(
      "title" = "What is the title?",
      "author" = "Who Is The Author",
      "date" = "`r format(Sys.Date())` at `r format(Sys.Time())` on `r Sys.info()[['nodename']]`",
      # Need basename to ensure that stabdalone templates (those not in the
      # templateSearchPath) are handled correctly
      "chapters" = basename(unlist(x@chapters))
    )
    y <- y %>% ymlthis::yml_toplevel(typeList)
    y <- y %>%
      ymlthis::yml_toplevel(list("format" = list("html" = list("theme" = "cosmo"), "pdf" = "default")))
    #
    #   bibliography: references.bib
    #
    y
  }
)
