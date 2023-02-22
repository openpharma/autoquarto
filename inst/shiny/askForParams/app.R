ui <- shiny::fluidPage(
  uiOutput("title"),
    shiny::sidebarLayout(
        sidebarPanel(
        ),
        shiny::mainPanel(
          wellPanel("Path:", shiny::htmlOutput("params")),
          wellPanel("File:", shiny::htmlOutput("file")),
          wellPanel("YAML:", shiny::htmlOutput("yaml"))
          
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
    
    output$params <- shiny::renderText({
      HTML(
        paste(
          lapply(
            names(v),
            function(n) paste0(n, ": ", v[[n]])
          ), 
          collapse="<br>")
        )
    })
    
    output$file <- renderText({ v$path })
    
    output$yaml <- renderText({ 
      shiny::validate(
        shiny::need(file.exists(v$path), "Path does not exist"),
        shiny::need(file.access(v$path, 4), "Path does not have read access")
      )
      
      frontMatter <- rmarkdown::yaml_front_matter(v$path)
      paste0(
        lapply(
          names(frontMatter),
          function(n) paste0(n, ": ", frontMatter[[n]])
        ),
        collapse="<br>"
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
