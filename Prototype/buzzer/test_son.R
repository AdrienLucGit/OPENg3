library(shiny)
library(beepr)

# Interface utilisateur
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #buzzer {
        font-size: 24px;
        padding: 20px 40px;
        background-color: #ff4136;
        color: white;
        border: none;
        border-radius: 10px;
        cursor: pointer;
      }
      #buzzer:hover {
        background-color: #ff725c;
      }
    "))
  ),
  
  # Sélecteur pour le choix du son
  fluidRow(
    column(12, align = "center",
           selectInput("sound_choice", "Choisissez votre son :", 
                       choices = c("DING !" = 1, "Victoire !" = 5, "Cri" = 9),
                       selected = 1)
    )
  ),
  
  # Bouton centré avec le texte "BUZZER"
  fluidRow(
    column(12, align = "center",
           actionButton("buzzer", "BUZZER"),
           br(),
    )
  )
)

# Serveur
server <- function(input, output, session) {
  observeEvent(input$buzzer, {
    # Jouer le son choisi
    beep(sound = as.numeric(input$sound_choice))
  })
}

# Lancement de l'application
shinyApp(ui, server)
