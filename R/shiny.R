#' Run a Shiny App to request Parameters for a Parameterised Report
#'
#' When given a Quarto document, website or book, interrogate the input to determine
#' if it has a YAML header.  If it does, parse the YAML header to identify what,
#' if any, parameters are required.  Display a GUI to obtain the required information
#' from the user and then update the Quarto object with the information provided.
#' @param ... Parameters passed to the app via `.GlobalEnv$askForParamsInput`.
#' See Usage Notes below.
#' @section Usage notes:
#' If the function is called from an R session that is not interactive, an error
#' is thrown. (TODO: Does this affect validation?)
#' Currently, the only parameter supported, and which is required, is `path`.
#' This should define the path to the Quarto document, website or book
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
  rm(.GlobalEnv$askForParamsInput)
}

# askForParams(path=normalizePath(testthat::test_path("testData", "testBook")))

#' Provide a GUI to enable editing of the parameters in a YAML header
#'
#' @param id the ID parameter passed to `shiny::NS()`.
#' @export
parameterEditorPanelUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("fileSelection")),
    shiny::textOutput(ns("currentFile")),
    shiny::uiOutput(ns("parameterTable"))
  )
}

#' Provide server functionality required by the Parameter Editor Panel
#'
#' @param id the ID parameter passed to `shiny::NS()`
#' @param path the path of the Quarto book/website/document whose parameters
#' are to be updated
#' @export
parameterEditorPanelServer <- function(id, path) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shinyjs::useShinyjs()
      ns <- session$ns

      v <- shiny::reactiveValues(
        path = path,
        pendingChanges = FALSE,
        rv = list(file = NA, params = NA, update = FALSE)
      )

      shiny::observeEvent(v$pendingChanges, {
        if (v$pendingChanges) {
          shinyjs::enable("save")
        } else {
          shinyjs::disable("save")
        }
      })

      output$fileSelection <- shiny::renderUI({
        print(v$path)
        if (isQuartoBook(v$path)) {
          print("It's a book")
          yml <- yaml::read_yaml(file.path(v$path, "_quarto.yml"))
          print(yml)
          v$chapters <- ymlthis::yml_pluck(yml, "book", "chapters")
          DiagrammeR::grVizOutput(ns("tree"))
          v$selectedFile <- v$path
        } else if (isQuartoWebsite(v$path)) {
          print("It's a website")
          shiny::tags$p("Quarto websites are not yet supported.")
        } else {
          print("It's not a book or website")
        }
      })

      output$tree <- DiagrammeR::renderGrViz({
        print("output$tree")
        root <- data.tree::Node$new("Chapters")
        for (c in v$chapters) root$AddChild(tools::file_path_sans_ext(c))
        data.tree::SetNodeStyle(root, fontcolor = "black", shape = "box", fontname = "helvetica", fontsize = 10)
        if (!is.null(v$currentFile)) {
          if (!is.na(v$currentFile)) {
            data.tree::SetNodeStyle(root[[tools::file_path_sans_ext(v$currentFile)]], style = "filled", fillcolor = "LightBlue", fontcolor = "white")
          }
        }
        plot(root)
      })

      shiny::observeEvent(input$tree_click, {
        shiny::req(input$tree_click)

        v$currentFile <- ifelse(
          input$tree_click$nodeValues[[1]] %in% tools::file_path_sans_ext(v$chapters),
          v$chapters[[which(tools::file_path_sans_ext(v$chapters) == input$tree_click$nodeValues[[1]])]],
          NA
        )
        if (!is.na(v$currentFile)) {
          fName <- file.path(v$path, v$currentFile)
          y <- readYamlFrontMatter(fName, "params")
          v$currentParameters <- tibble::tibble(Parameter = names(y), Value = unlist(y))
          if (v$currentParameters %>% nrow() == 0) {
            return()
          }
          v$currentParameters <- v$currentParameters %>%
            dplyr::mutate(
              Mode = "value",
              IsNULL = FALSE,
              IsNA = FALSE
            )
        } else {
          v$currentParameters <- NULL
        }
      })

      output$parameterTable <- shiny::renderUI({
        if (is.null(v$currentParameters)) {
          shiny::textOutput(ns("noParams"))
        } else if (v$currentParameters %>% nrow() == 0) {
          shiny::textOutput(ns("noParams"))
        } else {
          shiny::wellPanel(
            rhandsontable::rHandsontableOutput(ns("parameters")),
            shiny::tags$div(
              shiny::textOutput(ns("msg")),
              style = "color: #f00; margin-top: 10px; margin-bottom: 10px;"
            ),
            shiny::actionButton(ns("save"), "Save")
          )
        }
      })

      output$noParams <- shiny::renderText({
        if (is.null(v$currentParameters)) {
          "No chapter file selected"
        } else {
          paste0(v$currentFile, " requires no parameters")
        }
      })

      output$parameters <- rhandsontable::renderRHandsontable({
        shiny::req(v$currentParameters)

        if (v$currentParameters %>% nrow() == 0) {
          return()
        }

        rhandsontable::rhandsontable(
          v$currentParameters,
          colHeaders = c("Parameter", "Value", "Mode", "Is NULL?", "Is NA?"),
          hStretch = TRUE
        ) %>%
          rhandsontable::hot_col(
            col = "Mode",
            type = "dropdown",
            source = c("name", "value", "literal")
          ) %>%
          rhandsontable::hot_col(
            col = "Parameter",
            readOnly = TRUE
          ) %>%
          rhandsontable::hot_col(
            col = "Is NULL?",
            colWidths = 75
          ) %>%
          rhandsontable::hot_col(
            col = "Is NA?",
            colWidths = 75
          )
      })

      # Update current parameter values
      shiny::observeEvent(input$parameters$changes$changes, {
        # rhandsontable uses 0-based indices
        row <- 1 + input$parameters$changes$changes[[1]][[1]]
        col <- names(v$currentParameters)[1 + input$parameters$changes$changes[[1]][[2]]]
        value <- input$parameters$changes$changes[[1]][[4]]
        v$currentParameters <- v$currentParameters %>%
          dplyr::mutate(dplyr::across(col, ~ ifelse(dplyr::row_number() == row, value, .x)))
        v$pendingChanges <- TRUE
      })

      output$msg <- shiny::renderText({
        if (v$pendingChanges) "Unsaved changes are pending"
      })

      shiny::observeEvent(input$save, {
        v$rv$file <- v$currentFile
        v$rv$params <- parameterTibbleToYAML(v$currentParameters)
        v$rv$update <- TRUE
        v$pendingChanges <- FALSE
      })

      return(shiny::reactive({
        v$rv
      }))
    }
  )
}
