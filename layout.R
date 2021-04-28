setwd("C:/Users/elian/Documents/DSR")

library(shiny)
library(RColorBrewer)
library(ggplot2)
library(tree)

#Load tree model
data("iris")
load("Tree.RData")

#Create a color palette
palette <- brewer.pal(3, "Set2")

#Create user interface code
ui <- fluidPage(
  titlePanel("Iris Species Predictor"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "petal.length",
        label = "Petal Length (cm)",
        min = 1,
        max = 7,
        value = 4
      ),
      sliderInput(
        inputId = "petal.width",
        label = "Petal Width (cm)",
        min = 0.1,
        max = 2.5,
        value = 1.5
      )
    ),
    mainPanel(
      textOutput(
        outputId = "text"
      ),
      plotOutput(
        outputId = "plot"))))

#Create a server code
server <- function(input, output) {
  output$text = renderText({
    
    # Create predictors
    predictors <- data.frame(
      Petal.Length = input$petal.length,
      Petal.Width = input$petal.width,
      Sepal.Length = 0,
      Sepal.Width = 0)
    
    # Make prediction
    prediction = predict(
      object = model,
      newdata = predictors,
      type = "class")
    
    # Create prediction text
    paste(
      "The predicted species is ",
      as.character(prediction))
  })
  
  output$plot = renderPlot({
    
    # Create a scatterplot colored by species
    plot(
      x = iris$Petal.Length, 
      y = iris$Petal.Width,
      pch = 19,
      col = palette[as.numeric(iris$Species)],
      main = "Iris Petal Length vs. Width",
      xlab = "Petal Length (cm)",
      ylab = "Petal Width (cm)")
    
    # Plot the decision boundaries
    ggplot(iris, 
           aes(Petal.Width, Sepal.Width, color=Species)) + 
      geom_point() +
      gg.partition.tree(tree(Species ~ Sepal.Width + Petal.Width, data=iris), 
                        label="Species", color = "purple") 
    
    # Draw predictor on plot
    points(
      x = input$petal.length,
      y = input$petal.width,
      col = "red",
      pch = 4,
      cex = 2,
      lwd = 2)
  })
}
#Create a Shiny app
shinyApp(
  ui = ui,
  server = server
)