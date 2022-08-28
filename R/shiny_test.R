library(shiny)
ui <- function(){
  tagList(
    # Designing an interface that lets the 
    # user select a species from iris, 
    # then display a plot() of this dataset
    selectInput(
      "species", 
      "Choose one or more species",
      choices = unique(iris$Species),
      multiple = TRUE, 
      selected = unique(iris$Species)[1]
    ), 
    plotOutput("plot")
  )
}

server <- function(
    input, 
    output, 
    session
){
  # Taking the species as input, and returning the plot 
  # of the filtered dataset
  output$plot <- renderPlot({
    plot(
      iris[ iris$Species %in% input$species, ]
    )
  })
}

shinyApp(ui, server)