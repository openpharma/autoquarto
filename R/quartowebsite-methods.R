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

# locateTemplate ----

## generic ----

#' Search the `templateSearchPath` for the First Matching Template File
#' 
#' @param x a QuartoWebsite object
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
setGeneric("locateTemplate", function(x, fileName=NA, ...) standardGeneric("locateTemplate"))

## QuartoWebsite

#' Search the `templateSearchPath` for the First Matching Template File
#' @param strict Boolean.  Default: TRUE.  Check that the file found (a) exists 
#' (b) is readable and (c) has one of the `allowedExtensions`
#' @param allowedExtensions Default: `"qmd"`.  A vector of permitted extensions.  
#' Ignored if `strict` is `FALSE`
#' @describeIn locateTemplate  
#' @aliases locateTemplate-QuartoWebsite
#' @export
setMethod(
  "locateTemplate", 
  "QuartoWebsite",
  function(x, fileName, strict=TRUE, allowedExtensions=c("qmd")) {
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
        foundFile <- file.path(dir, fileName)
        if (checkmate::testFileExists(foundFile)) break
        foundFile <- NULL
      }
    } else {
      #Cater for relative paths
      foundFile <- path.expand(fileName)
    }
    if (is.null(foundFile)) stop(paste0("Unable to locate ", fileName))
    if (strict) {
      checkmate::assertFileExists(foundFile, access="r", extension=allowedExtensions)
    }
    foundFile
  }
)

# publish ----

## generic ----

#' Publish a Quarto object
#' 
#' @param x a QuartoWebsite object
#' @param outFile the name of the file containing the published Quarto object
#' @param ... passed to class methods
#' @docType methods
#' @export
setGeneric("publish", function(x, outFile, ...) standardGeneric("publish"))

## QuartoWebsite

#' Publish a QuartoWebsite object
#' @describeIn publish  
#' @aliases publish-QuartoWebsite
#' @param workDir Default: `NULL`.  The temporary work folder used to generate the report.  
#' If `NULL` a temporary work folder with a random name, is used.
#' @param logFile Default: NA.  The name of the log file that documents the publishing process.
#' If `NA`, the name is based on `basename(outFile)`, with the start date/time appended and
#' located in the folder returned by `here::here()` at the time the function was called.
#' If `NULL`, no log file is produced.  
#' @param tidyUp Boolean.  Should the workDir be deleted on successful publication?
#' @export
setMethod(
  "publish", 
  "QuartoWebsite",
  function(x, outFile, workDir=NULL, tidyUp=FALSE, logFile=NA) {
    # Validate
    if (!is.null(workDir)) {
      checkmate::assertCharacter(workDir, len=1)
      checkmate::assertDirectoryExists(workDir, access="rwx")
    }
    checkmate::assertPathForOutput(outFile)
    checkmate::assertLogical(tidyUp)
    # Prepare
    if (is.na(logFile)) {
      logFile <- file.path(
                   here::here(),
                   paste0(
                     tools::file_path_sans_ext(basename(outFile)), 
                     "_", 
                     format(lubridate::now(), "%Y_%m%b_%d_%H%M%S"),
                     ".log"
                    )
                  )
      print(paste0("logFile is ", logFile))
    }
    if (!is.null(logFile)) {
      futile.logger::flog.layout("~t ~l [~n:~f]: ~m")
      futile.logger::flog.appender(futile.logger::appender.file(logFile))
    }
    # Execute
    if (is.null(workDir)) {
      workDir <- tempdir()
      futile.logger::flog.info(paste0("Using `", workDir, "' as a working folder"))
      checkmate::assertDirectoryExists(workDir, access="rwx")
    }
    futile.logger::flog.info("Writing _quarto.yml")
    y <- ymlthis::yml(author=FALSE, date=FALSE) %>% 
      ymlthis::yml_toplevel(list("project"=list("type"=x@type)))
    typeList <- list()
    typeList[[x@type]] <- list(
      "title"="What is the title?", 
      "author"="Who Is The Author", 
      "date"="`r format(Sys.Date())` at `r format(Sys.Time())` on `r Sys.info[['nodename']]`"
    )
    y <- y %>% ymlthis::yml_toplevel(typeList)
    chapterList <- lapply(x@chapters, function(c) locateTemplate(x, c))
    y <- y %>% ymlthis::yml_toplevel(list("chapters"=chapterList))
  #   chapters:
  #     - index.qmd
  #   - intro.qmd
  #   - summary.qmd
  #   - references.qmd
  #   
  #   bibliography: references.bib
  #   
  #   format:
  #     html:
  #     theme: cosmo
  #   pdf:
  #     documentclass: scrreport
  #   epub:
  #     cover-image: cover.png
    y
  }
)
