ui <- shiny::fluidPage(
  uiOutput("title"),
    shiny::sidebarLayout(
        sidebarPanel(
        ),
        shiny::mainPanel(
          wellPanel("Path:", shiny::htmlOutput("path")),
          # wellPanel("YAML:", shiny::htmlOutput("yaml")),
          uiOutput("fileSelection"),
          textOutput("currentFile"),
          wellPanel(
            DT::DTOutput("parameters")
          )
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
    
    # output$yaml <- renderText({
    #   shiny::validate(
    #     shiny::need(file.exists(v$path), "Path does not exist"),
    #     shiny::need(file.access(v$path, 4), "Path does not have read access")
    #   )
    #   if (isQuartoBook(v$path)) {
    #     fName <- file.path(v$path, "_quarto.yml")
    #     frontMatter <- yaml::read_yaml(fName)
    #   } else {
    #     frontMatter <- rmarkdown::yaml_front_matter(v$path)
    #   }
    #   paste0(
    #     lapply(
    #       names(frontMatter),
    #       function(n) paste0(n, ": ", frontMatter[[n]])
    #     ),
    #     collapse="<br>"
    #   )
    # })
      
    output$fileSelection <- renderUI({
      if (isQuartoBook(v$path)) {
        yml <- yaml::read_yaml(file.path(v$path, "_quarto.yml"))
        v$chapters <- ymlthis::yml_pluck(yml, "book", "chapters")
        DiagrammeR::grVizOutput("tree")
      } else if (isQuartoBook(v$path)) {
        shiny::tags$p("Quarto websites are not yet supported.")
      } else {
        v$selectedFile <- v$path
      }
    })
    
    output$tree <- DiagrammeR::renderGrViz({
      root <- data.tree::Node$new("Chapters")
      for(c in v$chapters) root$AddChild(tools::file_path_sans_ext(c))
      SetNodeStyle(root, fontcolor = "black", shape="box", fontname = "helvetica", fontsize=10)
      if (!is.null(v$currentFile)) {
        SetNodeStyle(root[[tools::file_path_sans_ext(v$currentFile)]], style="filled", fillcolor = "LightBlue", fontcolor="white")
      }
      plot(root)
    })
    
    observeEvent(input$tree_click, {
      req(input$tree_click)

      v$currentFile <- ifelse(
                         input$tree_click$nodeValues[[1]] %in% tools::file_path_sans_ext(v$chapters), 
                         v$chapters[[which(tools::file_path_sans_ext(v$chapters) == input$tree_click$nodeValues[[1]] )]], 
                         NA
                       )
      fName <- file.path(v$path, v$currentFile)
      # 2023-Feb ymlthis::yml_pluck errors when the value is an integer, so let's roll our own
      x <- rmarkdown::yaml_front_matter(fName)[["params"]]
      y <- lapply(names(x), function(z) x[[z]])
      names(y) <- names(x)
      v$currentParameters <- tibble::tibble(Parameter=names(y), value=unlist(y))
    })
    
    # output$currentFile <- renderPrint({ 
    #   req(v$currentFile)
    #   
    #   v$currentFile 
    # })
    
    output$parameters <- DT::renderDT({
      req(v$currentParameters)
      
      v$currentParameters
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
