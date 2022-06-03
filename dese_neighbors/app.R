library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("Ethnicity", "Choose Ethnicity variables",
                             choices=list("Black"="black", 
                                          "White"="white"))
          ),
        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput("Ethnicity")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Ethnicity <- renderPrint({
    return(paste0("You have chosen the choice ",input$Ethnicity))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
