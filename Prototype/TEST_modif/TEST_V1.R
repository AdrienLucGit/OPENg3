library(shiny)

# Variables globales
session_password <- reactiveVal(NULL)  # Stocke le code de session défini par l'Admin
admin_password <- "admin123"  # Mot de passe fixe pour l'Admin
global_questions <- reactiveVal(list())
global_current_question <- reactiveVal("")
global_buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
global_players <- reactiveVal(data.frame(name = character(), stringsAsFactors = FALSE))
admin_chosen <- reactiveVal(FALSE)  # Indicateur pour bloquer le choix du rôle après sélection de l'Admin
error_message <- reactiveVal("")  # Message d'erreur pour le mot de passe

# UI
ui <- fluidPage(
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    tabPanel("Buzzer", 
             sidebarLayout(
               sidebarPanel(
                 radioButtons("user_role", "Choisissez votre rôle :", choices = c("Admin", "Joueur"),
                              selected = character(0), inline = TRUE),
                 
                 conditionalPanel(
                   condition = "input.user_role == 'Admin' && !input.role_locked",
                   passwordInput("admin_password_input", "Entrez le mot de passe Admin :", ""),
                   textInput("admin_session_code", "Créer un code de session :", ""),
                   actionButton("create_session", "Créer la session"),
                   textOutput("error_message")  # Affichage du message d'erreur
                 ),
                 
                 conditionalPanel(
                   condition = "input.user_role == 'Joueur' && !input.role_locked",
                   textInput("player_name", "Entrez votre pseudo :", ""),
                   textInput("player_session_code", "Entrez le code de session :", ""),
                   actionButton("join_session", "Rejoindre la session")
                 )
               ),
               
               mainPanel(
                 uiOutput("quiz_ui")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  session_data <- reactiveValues(role = NULL, player_name = NULL)
  
  observeEvent(input$user_role, {
    if (input$user_role == "Admin" && !admin_chosen()) {
      admin_chosen(TRUE)
    }
  })
  
  output$quiz_ui <- renderUI({
    if (is.null(session_data$role)) {
      return(h3("Veuillez choisir un rôle et entrer un code de session si nécessaire."))
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
    } else {
      return(
        fluidPage(
          h2(paste("Bienvenue,", session_data$player_name)),
          h3("Question en cours :"),
          textOutput("display_question"),
          actionButton("buzz", "Buzzer !", class = "btn-danger"),
          textOutput("buzz_feedback")
        )
      )
    }
  })
  
  observeEvent(input$create_session, {
    if (input$admin_password_input == admin_password && input$admin_session_code != "") {
      session_password(input$admin_session_code)
      session_data$role <- "Admin"
      error_message("")  # Réinitialisation du message d'erreur
    } else {
      error_message("Le mot de passe renseigné est erroné")
    }
  })
  
  output$error_message <- renderText({
    error_message()
  })
  
  observeEvent(input$join_session, {
    if (input$player_name != "" && input$player_session_code == session_password()) {
      global_players(rbind(global_players(), data.frame(name = input$player_name)))
      session_data$role <- "Joueur"
      session_data$player_name <- input$player_name
    }
  })
  
  observeEvent(input$add_question, {
    new_q <- input$new_question
    if (new_q != "") {
      global_questions(c(global_questions(), new_q))
    }
  })
  
  output$question_list <- renderTable({
    data.frame(Questions = global_questions())
  })
  
  observeEvent(input$start_game, {
    if (length(global_questions()) > 0) {
      global_current_question(global_questions()[[1]])
      global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
    }
  })
  
  output$current_question <- renderText({
    global_current_question()
  })
  
  output$display_question <- renderText({
    global_current_question()
  })
  
  observeEvent(input$buzz, {
    name <- session_data$player_name
    if (name %in% global_players()$name) {
      buzz_time <- Sys.time()
      current_buzzers <- global_buzz_list()
      if (!(name %in% current_buzzers$name)) {
        global_buzz_list(rbind(current_buzzers, data.frame(name = name, time = buzz_time)))
        output$buzz_feedback <- renderText("Buzz enregistré !")
      } else {
        output$buzz_feedback <- renderText("Vous avez déjà buzzé.")
      }
    }
  })
  
  output$buzz_order <- renderTable({
    global_buzz_list()[order(global_buzz_list()$time), ]
  })
  
  observeEvent(input$next_question, {
    q_list <- global_questions()  # Récupère la liste des questions
    current_index <- match(global_current_question(), q_list)  # Trouve la question actuelle
    
    if (!is.na(current_index) && current_index < length(q_list)) {
      global_current_question(q_list[[current_index + 1]])  # Passe à la question suivante
      
      # Réinitialisation des buzzers
      global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
      
      # Réinitialisation du message du buzzer
      output$buzz_feedback <- renderText({ "Vous pouvez buzzer !" })
    }
  })

  observeEvent(input$reset_buzzers, {
    req(input$reset_buzzers)  # Vérifie que le bouton a bien été pressé
    # Réinitialisation de la liste des buzzers
    global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
    # Suppression du message
    output$buzz_feedback <- renderText({ "Vous pouvez buzzer !" })
  })
}

shinyApp(ui = ui, server = server)
