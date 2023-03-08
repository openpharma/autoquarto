#' Determine if a path points to a Quarto object
#'
#' @param path the path to be tested
#' @returns `TRUE` if the path is a valid Quarto object, `FALSE` otherwise
#' @export
isQuartoObject <- function(path) {
  isQuartoDocument(path) |
    isQuartoWebsite(path) |
    isQuartoBook(path)
}

#' Determine if a path points to a Quarto book
#'
#' The path is a Quarto book if and only if:
#'  - It is a readable directory
#'  - It contains a readable file named `_project.yml`
#'  - `_project.yml` contains a `project: type: book` entry
#' @param path the path to be tested
#' @returns `TRUE` if the path is a valid Quarto book, `FALSE` otherwise
#' @export
isQuartoBook <- function(path) {
  if (!is.character(path)) {
    print("path is a character")
    return(FALSE)
  }
  exists <- checkmate::checkDirectoryExists(path)
  if (!is.logical(exists)) {
    print("checkDirectoryExists did not return a logical")
    print(path)
    print(checkmate::checkDirectoryExists(path))
    return(FALSE)
  }
  if (!exists) {
    print("Directory does not exist")
    return(FALSE)
  }
  f <- file.path(path, "_quarto.yml")
  if (!checkmate::checkFile(f, "r")) {
    print("_quarto.yml does not exist")
    return(FALSE)
  }
  yml <- yaml::read_yaml(f)
  print(yml)
  print(ymlthis::yml_pluck(yml, "project", "type") == "book")
  ymlthis::yml_pluck(yml, "project", "type") == "book"
}

#' Determine if a path points to a Quarto website
#'
#' The path is a Quarto website if and only if:
#'  - It is a readable directory
#'  - It contains a readable file named `_project.yml`
#'  - `_project.yml` contains a `project: type: website` entry
#' @param path the path to be tested
#' @returns `TRUE` if the path is a valid Quarto book, `FALSE` otherwise
#' @export
isQuartoWebsite <- function(path) {
  if (!is.character(path)) {
    return(FALSE)
  }
  exists <- checkmate::checkDirectoryExists(path)
  if (!is.logical(exists)) {
    return(FALSE)
  }
  if (!exists) {
    return(FALSE)
  }
  f <- file.path(path, "_quarto.yml")
  if (!checkmate::checkFile(f, "r")) {
    return(FALSE)
  }
  yml <- yaml::read_yaml(f)
  ymlthis::yml_pluck(yml, "project", "type") == "website"
}

#' Determine if a path points to a Quarto document
#'
#' The path is a Quarto document if and only if:
#'  - It is a readable file with extension "qmd"
#' @param path the path to be tested
#' @returns `TRUE` if the path is a valid Quarto document, `FALSE` otherwise
#' @export
isQuartoDocument <- function(path) {
  if (!is.character(path)) {
    return(FALSE)
  }
  exists <- checkmate::checkFileExists(path)
  if (!is.logical(exists)) {
    return(FALSE)
  }
  if (!exists) {
    return(FALSE)
  }
  checkmate::checkFile(path, "r", "qmd")
}
