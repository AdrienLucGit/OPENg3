library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    
    # Onglet 1 : Accueil
    tabPanel("Accueil", 
             h2("Bienvenue !"),
             p("Ceci est une application Shiny avec plusieurs onglets.")
    ),
    
    # Onglet 2 : Quiz et Buzzer
    tabPanel("Buzzer", 
             sidebarLayout(
               sidebarPanel(
                 textInput("session_code", "Code de session :", ""),
                 radioButtons("user_role", "Choisissez votre rôle :", choices = c("Admin", "Joueur")),
                 actionButton("enter_room", "Entrer dans la salle")
               ),
               mainPanel(
                 uiOutput("quiz_ui")
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
  
  # Stockage des données de session
  session_data <- reactiveValues(
    session_code = NULL,
    role = NULL,
    questions = list(),
    players = data.frame(name = character(), stringsAsFactors = FALSE),
    buzz_list = data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE),
    current_question_index = 0,
    buzz_counter = 0
  )
  
  # Interface du quiz
  output$quiz_ui <- renderUI({
    if (is.null(session_data$session_code) || is.null(session_data$role)) {
      return(h3("Veuillez entrer un code de session et un rôle pour accéder au quiz."))
    }
    
    if (session_data$role == "Admin") {
      return(
        fluidPage(
          h2("Interface Admin"),
          textInput("new_question", "Nouvelle question :", ""),
          actionButton("add_question", "Ajouter la question"),
          tableOutput("question_list"),
          actionButton("start_game", "Démarrer le jeu"),
          actionButton("next_question", "Question suivante"),
          actionButton("reset_buzzers", "Réinitialiser les buzzers"),
          h3("Ordre des buzz :"),
          tableOutput("buzz_order"),
          h3("Question en cours :"),
          textOutput("current_question")
        )
      )
    } else if (session_data$role == "Joueur") {
      return(
        fluidPage(
          h2("Interface Joueur"),
          textInput("player_name", "Entrez votre pseudo :", ""),
          actionButton("register_player", "S'inscrire"),
          h3("Question en cours :"),
          textOutput("current_question"),
          actionButton("buzz", "Buzzer !", class = "btn-danger"),
          textOutput("buzz_feedback")
        )
      )
    }
  })
  
  # Gestion de l'entrée dans la salle
  observeEvent(input$enter_room, {
    if (input$session_code != "" && input$user_role %in% c("Admin", "Joueur")) {
      session_data$session_code <- input$session_code
      session_data$role <- input$user_role
    }
  })
  
  # Gestion des questions (Admin)
  observeEvent(input$add_question, {
    new_q <- input$new_question
    if (new_q != "") {
      session_data$questions <- append(session_data$questions, list(new_q))
    }
  })
  
  output$question_list <- renderTable({
    data.frame(Questions = session_data$questions)
  })
  
  observeEvent(input$start_game, {
    if (length(session_data$questions) > 0) {
      session_data$current_question_index <- 1
      session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
    }
  })
  
  output$current_question <- renderText({
    q_list <- session_data$questions
    if (length(q_list) >= session_data$current_question_index && session_data$current_question_index > 0) {
      q_list[[session_data$current_question_index]]
    } else {
      "Aucune question disponible"
    }
  })
  
  # Gestion des joueurs
  observeEvent(input$register_player, {
    name <- input$player_name
    if (name != "" && !(name %in% session_data$players$name)) {
      session_data$players <- rbind(session_data$players, data.frame(name = name))
    }
  })
  
  # Gestion des buzzers
  observeEvent(input$buzz, {
    name <- input$player_name
    if (name %in% session_data$players$name) {
      buzz_time <- session_data$buzz_counter + 1
      session_data$buzz_counter <- buzz_time
      
      current_buzzers <- session_data$buzz_list
      if (!(name %in% current_buzzers$name)) {
        session_data$buzz_list <- rbind(current_buzzers, data.frame(name = name, time = buzz_time))
        output$buzz_feedback <- renderText("Buzz enregistré !")
      } else {
        output$buzz_feedback <- renderText("Vous avez déjà buzzé.")
      }
    }
  })
  
  output$buzz_order <- renderTable({
    session_data$buzz_list[order(session_data$buzz_list$time), ]
  })
  
  observeEvent(input$next_question, {
    if (session_data$current_question_index < length(session_data$questions)) {
      session_data$current_question_index <- session_data$current_question_index + 1
      session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
      session_data$buzz_counter <- 0
    }
  })
  
  observeEvent(input$reset_buzzers, {
    session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
    session_data$buzz_counter <- 0
  })
}

shinyApp(ui = ui, server = server)
