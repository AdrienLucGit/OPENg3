library(shiny)

# Variables globales pour assurer la synchronisation entre l'admin et les joueurs
global_questions <- reactiveVal(list())  # Liste des questions globales
global_current_question <- reactiveVal("")  # Question actuelle
global_buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))  # Liste des buzzers
global_players <- reactiveVal(data.frame(name = character(), stringsAsFactors = FALSE))

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
                 actionButton("enter_room", "Entrer dans la salle"),
                 conditionalPanel(
                   condition = "input.user_role == 'Joueur'",
                   textInput("player_name", "Entrez votre pseudo :", ""),
                   actionButton("register_player", "S'inscrire"),
                   h3("Question en cours :"),
                   textOutput("display_question"),
                   actionButton("buzz", "Buzzer !", class = "btn-danger"),
                   textOutput("buzz_feedback")
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
  session_data <- reactiveValues(session_code = NULL, role = NULL)
  
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
    }
  })
  
  observeEvent(input$enter_room, {
    if (input$session_code != "" && input$user_role %in% c("Admin", "Joueur")) {
      session_data$session_code <- input$session_code
      session_data$role <- input$user_role
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
  
  observeEvent(input$register_player, {
    name <- input$player_name
    if (name != "" && !(name %in% global_players()$name)) {
      global_players(rbind(global_players(), data.frame(name = name)))
    }
  })
  
  observeEvent(input$buzz, {
    name <- input$player_name
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
    q_list <- global_questions()
    current_index <- match(global_current_question(), q_list)
    if (!is.na(current_index) && current_index < length(q_list)) {
      global_current_question(q_list[[current_index + 1]])
      global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
    }
  })
  observeEvent(input$reset_buzzers, {
    button_state(TRUE)  
    global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  })
  
  observe({
    if (button_state()) {
      updateActionButton(session, "buzzer", style = "background-color: green; color: white;")
    } else {
      updateActionButton(session, "buzzer", style = "background-color: red; color: white;")
    }
  })
}

  

shinyApp(ui = ui, server = server)