library(shiny)

# Reactive values for questions and buzzers
questions <- reactiveVal(list())  # Store quiz questions
current_question_index <- reactiveVal(0)  # Track current question
buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))  # Store buzz order
players <- reactiveVal(data.frame(name = character(), buzz_time = numeric(), stringsAsFactors = FALSE))  # Player list

# UI
ui <- fluidPage(
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    
    # Onglet 1 : Accueil avec mode d'emploi Administrateur
    tabPanel("Accueil", 
             h2("Bienvenue dans l'application Quiz !"),
             p("Cette application Shiny permet de gérer un quiz multijoueur avec un système de buzzer."),
             
             # Mode d'emploi Administrateur
             h3("Mode d'emploi Administrateur"),
             p("En tant qu'administrateur, vous avez plusieurs fonctionnalités pour gérer le quiz et les joueurs :"),
             tags$ul(
               tags$li("Ajouter des questions : Vous pouvez ajouter de nouvelles questions à tout moment."),
               tags$li("Démarrer le jeu : Cliquez sur le bouton 'Démarrer le jeu' pour commencer le quiz."),
               tags$li("Passer à la question suivante : Vous pouvez faire défiler les questions en cliquant sur 'Question suivante'."),
               tags$li("Réinitialiser les buzzers : Si nécessaire, vous pouvez réinitialiser l'ordre des buzzers."),
               tags$li("Voir l'ordre des buzzers : L'ordre d'arrivée des joueurs au buzzer est affiché après chaque question."),
               tags$li("Bloquer ou exclure les buzzer : Si nécessaire, vous pouvez bloquer ou exclure un buzzer.")
             ),
             p("Suivez les instructions sur les autres onglets pour gérer le quiz et participer au jeu."),
             br(),
             
             actionButton("go_to_quiz", "Accéder au Quiz et Buzzer", class = "btn-primary")
    ),
    
    # Onglet 2 : Quiz et Buzzer
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
    
    # Onglet 3 : À propos
    tabPanel("À propos", 
             h2("Informations"),
             p("Cette application Shiny a été développée pour gérer un système de quiz avec buzzer multijoueur.")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Ajouter une question
  observeEvent(input$add_question, {
    new_q <- input$new_question
    if (new_q != "") {
      questions(c(questions(), new_q))
    }
  })
  
  # Afficher les questions enregistrées
  output$question_list <- renderTable({
    data.frame(Questions = questions())
  })
  
  # Démarrer le quiz
  observeEvent(input$start_game, {
    current_question_index(1)
    buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))  # Reset buzz list
  })
  
  # Afficher la question en cours
  output$display_question <- renderText({
    q_list <- questions()
    if (length(q_list) >= current_question_index()) {
      q_list[[current_question_index()]]
    } else {
      "Aucune question disponible"
    }
  })
  
  # Inscription des joueurs
  observeEvent(input$register_player, {
    name <- input$player_name
    if (name != "" && !(name %in% players()$name)) {
      players(rbind(players(), data.frame(name = name, buzz_time = Inf)))
    }
  })
  
  # Buzzer un joueur
  observeEvent(input$buzz, {
    name <- input$player_name
    if (name %in% players()$name) {
      buzz_time <- Sys.time()
      current_buzzers <- buzz_list()
      if (!(name %in% current_buzzers$name)) {
        buzz_list(rbind(current_buzzers, data.frame(name = name, time = buzz_time)))
      }
    }
  })
  
  # Afficher l'ordre des buzzers
  output$buzz_order <- renderTable({
    buzz_list()[order(buzz_list()$time), ]
  })
  
  # Passer à la question suivante
  observeEvent(input$next_question, {
    current_question_index(current_question_index() + 1)
    buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))  # Reset buzz list
  })
  
  # Réinitialiser les buzzers
  observeEvent(input$reset_buzzers, {
    buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  })
  
  # Navigation vers l'onglet Quiz et Buzzer
  observeEvent(input$go_to_quiz, {
    updateTabsetPanel(session, "tabs", selected = "Buzzer")
  })
}

shinyApp(ui = ui, server = server)