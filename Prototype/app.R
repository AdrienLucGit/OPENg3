library(shiny)

# Création des variables réactives pour stocker les données du jeu
session_data <- reactiveValues(
  questions = list(),
  current_question_index = 0,
  buzz_list = data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE),
  players = data.frame(name = character(), stringsAsFactors = FALSE)
)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Quiz en ligne avec Buzzer"),
  
  tabsetPanel(
    
    # Accueil
    tabPanel("Accueil", 
             h2("Bienvenue !"),
             p("Rejoignez une session de quiz en entrant votre pseudo.")
    ),
    
    # Quiz et Buzzer
    tabPanel("Buzzer", 
             sidebarLayout(
               sidebarPanel(
                 textInput("player_name", "Entrez votre pseudo :", ""),
                 actionButton("register_player", "S'inscrire"),
                 br(),
                 h4("Question en cours :"),
                 textOutput("display_question"),
                 actionButton("buzz", "Buzzer !", class = "btn btn-danger"),
                 br(),
                 h3("Ordre des buzz :"),
                 tableOutput("buzz_order"),
                 actionButton("reset_buzzers", "Réinitialiser les buzzers")
               ),
               mainPanel(
                 h3("Administration"),
                 textInput("new_question", "Nouvelle question :", ""),
                 actionButton("add_question", "Ajouter la question"),
                 br(),
                 h4("Questions enregistrées :"),
                 tableOutput("question_list"),
                 actionButton("start_game", "Démarrer le jeu"),
                 actionButton("next_question", "Question suivante")
               )
             )
    ),
    
    # À propos
    tabPanel("À propos", 
             h2("Informations"),
             p("Cette application Shiny permet de jouer à un quiz interactif avec un buzzer en ligne.")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Ajouter une question
  observeEvent(input$add_question, {
    new_q <- input$new_question
    if (new_q != "") {
      session_data$questions <- append(session_data$questions, list(new_q))
    }
  })
  
  # Afficher les questions enregistrées
  output$question_list <- renderTable({
    data.frame(Questions = session_data$questions)
  })
  
  # Démarrer le quiz
  observeEvent(input$start_game, {
    if (length(session_data$questions) > 0) {
      session_data$current_question_index <- 1
      session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
    }
  })
  
  # Afficher la question en cours
  output$display_question <- renderText({
    req(session_data$current_question_index > 0)
    session_data$questions[[session_data$current_question_index]]
  })
  
  # Inscription des joueurs
  observeEvent(input$register_player, {
    name <- input$player_name
    if (name != "" && !(name %in% session_data$players$name)) {
      session_data$players <- rbind(session_data$players, data.frame(name = name))
    }
  })
  
  # Buzzer un joueur
  observeEvent(input$buzz, {
    name <- input$player_name
    if (name %in% session_data$players$name) {
      buzz_time <- Sys.time()
      current_buzzers <- session_data$buzz_list
      if (!(name %in% current_buzzers$name)) {
        session_data$buzz_list <- rbind(current_buzzers, data.frame(name = name, time = as.numeric(buzz_time)))
      }
    }
  })
  
  # Afficher l'ordre des buzzers
  output$buzz_order <- renderTable({
    session_data$buzz_list[order(session_data$buzz_list$time), ]
  })
  
  # Passer à la question suivante
  observeEvent(input$next_question, {
    if (session_data$current_question_index < length(session_data$questions)) {
      session_data$current_question_index <- session_data$current_question_index + 1
      session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
    }
  })
  
  # Réinitialiser les buzzers
  observeEvent(input$reset_buzzers, {
    session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
