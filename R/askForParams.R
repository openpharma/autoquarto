#' Run a Shiny App to request Parameters for a Parameterised Report
#' 
#' When given a Quarto document, website or book, interrogate the input to determine 
#' if it has a YAML header.  If it does, parse the YAML header to identify what,
#' if any, parameters are required.  Display a GUI to obtain the required information
#' from the user and then update the Quarto object with the information provided.
#' @param path The path to the Quarto document, website or book
#' @section Usage notes:
#' If the function is called from an R session that is not interactive, an error 
#' is thrown. [TODO: Does this affect validation?]
#' @example askForParams(path=testthat::test_path("testData", "global", "intro.qmd"))
#' @export
askForParams <- function(...) {
  # Validate
  
  # Execute
  appDir <- system.file("shiny", "askForParams", package = "autoquarto")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `autoquarto`.", call. = FALSE)
  }
  
  .GlobalEnv$askForParamsInput <- list(...)
  shiny::runApp(appDir, display.mode = "normal")
}

askForParams(path=normalizePath(testthat::test_path("testData", "testBook")))
