library(shiny)

# Variables globales pour partager les données entre sessions
questions <- reactiveVal(list())
current_question_index <- reactiveVal(0)
buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
players <- reactiveVal(data.frame(name = character(), stringsAsFactors = FALSE))

# UI
ui <- fluidPage(
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    tabPanel("Accueil", 
             h2("Bienvenue !"),
             p("Ceci est une application Shiny avec plusieurs onglets.")
    ),
    
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
                   textOutput("current_question"),
                   actionButton("buzz", "Buzzer !", class = "btn btn-danger"),
                   textOutput("buzz_feedback")
                 )
               ),
               mainPanel(
                 uiOutput("quiz_ui")
               )
             )
    ),
    
    tabPanel("À propos", 
             h2("Informations"),
             p("Cette application Shiny a été développée pour gérer un système de quiz avec buzzer multijoueur.")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Interface dynamique
  output$quiz_ui <- renderUI({
    if (input$user_role == "Admin") {
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
  
  # Gestion des questions (Admin)
  observeEvent(input$add_question, {
    new_q <- input$new_question
    if (new_q != "") {
      questions(append(questions(), list(new_q)))
    }
  })
  
  output$question_list <- renderTable({
    data.frame(Questions = questions())
  })
  
  observeEvent(input$start_game, {
    if (length(questions()) > 0) {
      current_question_index(1)
      buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
    }
  })
  
  output$current_question <- renderText({
    q_list <- questions()
    if (length(q_list) >= current_question_index() && current_question_index() > 0) {
      q_list[[current_question_index()]]
    } else {
      "Aucune question disponible"
    }
  })
  
  # Gestion des joueurs
  observeEvent(input$register_player, {
    name <- input$player_name
    if (name != "" && !(name %in% players()$name)) {
      players(rbind(players(), data.frame(name = name)))
    }
  })
  
  # Gestion des buzzers
  observeEvent(input$buzz, {
    name <- input$player_name
    if (name %in% players()$name) {
      buzz_time <- Sys.time()
      current_buzzers <- buzz_list()
      if (!(name %in% current_buzzers$name)) {
        buzz_list(rbind(current_buzzers, data.frame(name = name, time = buzz_time)))
        output$buzz_feedback <- renderText("Buzz enregistré !")
      } else {
        output$buzz_feedback <- renderText("Vous avez déjà buzzé.")
      }
    }
  })
  
  output$buzz_order <- renderTable({
    buzz_list()[order(buzz_list()$time), ]
  })
  
  observeEvent(input$next_question, {
    if (current_question_index() < length(questions())) {
      current_question_index(current_question_index() + 1)
      buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
    }
  })
  
  observeEvent(input$reset_buzzers, {
    buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  })
}

shinyApp(ui = ui, server = server)
