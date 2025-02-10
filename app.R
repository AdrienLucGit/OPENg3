library(shiny)

# Store the names of players who pressed the buzzer (as a vector)
reactive_buzzers <- reactiveVal(c())

# Define the UI
ui <- fluidPage(
  titlePanel("Application Shiny avec Onglets"),
  
  tabsetPanel(
    
    # Onglet 1 : Accueil
    tabPanel("Accueil", 
             h2("Bienvenue !"),
             p("Ceci est une application Shiny avec plusieurs onglets.")
    ),
    
    # Onglet 2 : Système de Buzzer
    tabPanel("Buzzer", 
             textInput("name", "Enter Your Name:", ""),
             actionButton("buzz", "Press the Buzzer!", class = "btn btn-danger btn-lg"),
             
             h3("Players Who Buzzed:"),
             uiOutput("order"),  # Affiche la liste des joueurs
             
             actionButton("reset", "Reset Buzzer", class = "btn btn-warning")
    ),
    
    # Onglet 3 : À propos
    tabPanel("À propos", 
             h2("Informations"),
             p("Cette application Shiny a été développée pour gérer un système de buzzer multijoueur.")
    )
  )
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
    if (length(reactive_buzzers()) == 0) {
      return("No one has buzzed yet!")
    } else {
      buzzers <- reactive_buzzers()
      
      tagList(
        lapply(1:length(buzzers), function(i) {
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
