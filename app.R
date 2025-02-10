library(shiny)

# Store the names of players who pressed the buzzer (as a vector)
reactive_buzzers <- reactiveVal(c())

# Define the UI
ui <- fluidPage(
  titlePanel("Multiplayer Buzzer System"),
  
  textInput("name", "Enter Your Name:", ""),
  actionButton("buzz", "Press the Buzzer!", class = "btn btn-danger btn-lg"),
  
  h3("Players Who Buzzed:"),
  
  # Render the list of players dynamically
  uiOutput("order"),
  
  actionButton("reset", "Reset Buzzer", class = "btn btn-warning")
)

# Define the Server logic
server <- function(input, output, session) {
  
  observeEvent(input$buzz, {
    if (input$name != "") {
      # Append the name to the list of buzzers
      current_buzzers <- reactive_buzzers()
      current_buzzers <- c(current_buzzers, input$name)
      reactive_buzzers(current_buzzers)  # Update the reactive value
    }
  })
  
  output$order <- renderUI({
    # If no one has buzzed, show a message
    if (length(reactive_buzzers()) == 0) {
      return("No one has buzzed yet!")
    } else {
      # Get the current list of buzzers
      buzzers <- reactive_buzzers()
      
      # Create a list of UI elements (tags) for each player
      tagList(
        lapply(1:length(buzzers), function(i) {
          # Create each numbered item as an HTML tag
          div(paste(i, ".", buzzers[i]))
        })
      )
    }
  })
  
  observeEvent(input$reset, {
    reactive_buzzers(c())  # Reset the buzzer list
  })
}

# Run the app
shinyApp(ui, server)
