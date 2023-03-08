ui <- shiny::fluidPage(
  shiny::tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "default.css")),
  shiny::uiOutput("title"),
  shiny::sidebarLayout(
    sidebarPanel(),
    shiny::mainPanel(
      wellPanel("Path:", shiny::htmlOutput("path")),
      # wellPanel("YAML:", shiny::htmlOutput("yaml")),
      parameterEditorPanelUI("askForParams")
    )
  )
)

server <- function(input, output) {
<<<<<<< HEAD
    v <- reactiveValues()
    for(n in names(.GlobalEnv$askForParamsInput)) {
      v[[n]] <- .GlobalEnv$askForParamsInput[[n]]
      if (n == "path") path <- .GlobalEnv$askForParamsInput[[n]]
    }
    
    output$title <- renderUI({
      shiny::titlePanel(paste0("Parameters for ", basename(v$path)))
    })
    
    output$path <- shiny::renderText({ v$path })
    
    params <- parameterEditorPanelServer("askForParams", path)
    
    shiny::observeEvent(params(), {
      .GlobalEnv$askForParamsOutput <- params()
    })
=======
  v <- reactiveValues()
  for (n in names(.GlobalEnv$askForParamsInput)) {
    v[[n]] <- .GlobalEnv$askForParamsInput[[n]]
    if (n == "path") path <- .GlobalEnv$askForParamsInput[[n]]
  }

  output$title <- renderUI({
    shiny::titlePanel(paste0("Parameters for ", basename(v$path)))
  })

  output$path <- shiny::renderText({
    v$path
  })

  params <- parameterEditorPanelServer("askForParams", path)

  shiny::observeEvent(params(), {
    print(params())
  })
>>>>>>> 864b961f7f9033a0a6e14cb8fb56af55921ee00e
}

# Run the application
shinyApp(ui = ui, server = server)
