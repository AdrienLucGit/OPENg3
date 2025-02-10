library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Quiz App"),
  
  # Page d'accueil
  uiOutput("home_ui"),
  
  # Interface du quiz (affichée seulement après validation)
  uiOutput("quiz_ui")
)

# Serveur
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
  
  # Interface d'accueil
  output$home_ui <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          textInput("session_code", "Code de session :", ""),
          radioButtons("user_role", "Choisissez votre rôle :", choices = c("Admin", "Joueur")),
          actionButton("enter_room", "Entrer dans la salle")
        ),
        mainPanel(
          h3("Bienvenue sur le Quiz App"),
          p("Entrez un code de session et choisissez un rôle pour rejoindre la salle.")
        )
      )
    )
  })
  
  # Interface du quiz (dynamique selon le rôle)
  output$quiz_ui <- renderUI({
    if (is.null(session_data$session_code) || is.null(session_data$role)) {
      return(NULL)  # Tant que l'utilisateur n'a pas validé, rien n'est affiché
    }
    
    if (session_data$role == "Admin") {
      return(
        fluidPage(
          h2("Interface Admin"),
          sidebarLayout(
            sidebarPanel(
              textInput("new_question", "Nouvelle question :", ""),
              actionButton("add_question", "Ajouter la question"),
              br(),
              h4("Questions enregistrées :"),
              tableOutput("question_list"),
              br(),
              actionButton("start_game", "Démarrer le jeu"),
              h4("Ordre des buzz :"),
              tableOutput("buzz_order"),
              actionButton("next_question", "Question suivante"),
              actionButton("reset_buzzers", "Réinitialiser les buzzers")
            ),
            mainPanel(
              h3("Question en cours :"),
              textOutput("current_question")
            )
          )
        )
      )
    } else {
      return(
        fluidPage(
          h2("Interface Joueur"),
          sidebarLayout(
            sidebarPanel(
              textInput("player_name", "Entrez votre pseudo :", ""),
              actionButton("register_player", "S'inscrire")
            ),
            mainPanel(
              h3("Question affichée :"),
              textOutput("display_question"),
              actionButton("buzz", "Buzzer !", class = "btn-danger"),
              textOutput("buzz_feedback")
            )
          )
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
  
  output$display_question <- renderText({
    q_list <- session_data$questions
    if (length(q_list) >= session_data$current_question_index && session_data$current_question_index > 0) {
      q_list[[session_data$current_question_index]]
    } else {
      "En attente de question..."
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
  
  # Passer à la question suivante
  observeEvent(input$next_question, {
    if (session_data$current_question_index < length(session_data$questions)) {
      session_data$current_question_index <- session_data$current_question_index + 1
      session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
      session_data$buzz_counter <- 0
    }
  })
  
  # Réinitialiser les buzzers
  observeEvent(input$reset_buzzers, {
    session_data$buzz_list <- data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE)
    session_data$buzz_counter <- 0
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
