ui <- shiny::fluidPage(
    shiny::tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "default.css")),
    shiny::uiOutput("title"),
    shiny::sidebarLayout(
        sidebarPanel(
        ),
        shiny::mainPanel(
          wellPanel("Path:", shiny::htmlOutput("path")),
          # wellPanel("YAML:", shiny::htmlOutput("yaml")),
          parameterEditorPanelUI("askForParams")
        )
    )
)

server <- function(input, output) {
    v <- reactiveValues()
    for(n in names(.GlobalEnv$askForParamsInput)) {
      v[[n]] <- .GlobalEnv$askForParamsInput[[n]]
    }
    
    output$title <- renderUI({
      shiny::titlePanel(paste0("Parameters for ", basename(v$path)))
    })
    
    output$path <- shiny::renderText({ v$path })
    
    observe({
      parameterEditorPanelServer("askForParams", v$path)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
