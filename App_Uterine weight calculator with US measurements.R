


## Online calculator
## Rachel Jin

### Create your Shiny app
library(shiny)

# Define UI for the calculator app
ui <- fluidPage(
  titlePanel("Calculator: using ultrasound measurements to predict uterine weight"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("longitudinal", "Longitudinal (cm):", value = 0),
      numericInput("transverse", "Transverse (cm):", value = 0),
      numericInput("anterior.posterior", "Anterior/posterior (cm):", value = 0),
      selectInput("fibroids", "Does patient has fibroids:", 
                  choices = c("Yes", "No")),
      actionButton("calc", "Calculate")
    ),
    
    mainPanel(
      h3("Estimated uterine weight (gram):"),
      verbatimTextOutput("result")
    )
  )
)
 
# Define server logic for the calculator
server <- function(input, output) {
  observeEvent(input$calc, {
    result <- switch(input$fibroids,
                     "Yes" = ifelse(input$longitudinal==0 | input$transverse==0 | input$anterior.posterior==0,"Fill in the measurements",
                       round(exp(1.52427 + 0.81448 * log(input$longitudinal*input$transverse*input$anterior.posterior)*0.52 - 0.45682))),
                     "No" = ifelse(input$longitudinal==0 | input$transverse==0 | input$anterior.posterior==0,"Fill in the measurements",
                                   round(exp(1.52427 + 0.67483 * log(input$longitudinal*input$transverse*input$anterior.posterior)*0.52))))
    
    output$result <- renderText(result)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




