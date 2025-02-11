library(shiny)

# Variables globales pour assurer la synchronisation entre l'admin et les joueurs
global_questions <- reactiveVal(list())  # Liste des questions globales
global_current_question <- reactiveVal("")  # Question actuelle
global_buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))  # Liste des buzzers
global_players <- reactiveVal(data.frame(name = character(), stringsAsFactors = FALSE))

# UI
ui <- fluidPage(
  style = "background-color: #ffe4b5; padding: 20px; border-radius: 10px;",
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    
    # Onglet 1 : Accueil
    tabPanel("Accueil", 
             style = "background-color: #fefee3; padding: 20px; border-radius: 10px;",
             h2("Bienvenue dans l'application Quiz !"),
             p("Notre application", strong(em("Buzzer")), "est une solution simple et interactive conçue pour faciliter et dynamiser vos jeux, quiz et compétitions. Elle permet aux utilisateurs d’appuyer sur un bouton virtuel pour déclencher un signal sonore, indiquant ainsi qui a répondu."),
             p("Idéale pour des soirées entre amis, des événements ludiques ou des sessions de formation, cette application garantit une expérience fluide et équitable. Grâce à son interface intuitive et sa réactivité instantanée, elle transforme n'importe quel appareil en un véritable buzzer de jeu. 🚀"),
             
             # Mode d'emploi Administrateur
             h3("Mode d'emploi Administrateur"),
             p("En tant qu'administrateur, vous avez plusieurs fonctionnalités pour gérer le quiz et les joueurs :"),
             tags$ul(
               tags$li(strong("Ajouter des questions :"),
                       "Vous pouvez ajouter de nouvelles questions à tout moment."),
               tags$li(strong("Démarrer le jeu :"),
                       "Cliquez sur le bouton 'Démarrer le jeu' pour commencer le quiz."),
               tags$li(strong("Passer à la question suivante :"),
                       "Vous pouvez faire défiler les questions en cliquant sur 'Question suivante'."),
               tags$li(strong("Réinitialiser les buzzers :"),
                       "Si nécessaire, vous pouvez réinitialiser l'ordre des buzzers."),
               tags$li(strong("Voir l'ordre des buzzers :"),
                       "L'ordre d'arrivée des joueurs au buzzer est affiché après chaque question."),
               tags$li(strong("Bloquer ou exclure les buzzer :"),
                       "Si nécessaire, vous pouvez bloquer ou exclure un buzzer.")
             ),
             
             #Mode d'emploi Joueur 
             h3("Mode d'emploi joueur"),
             p("L'interface du mode joueur comprend une zone de texte pour entrer son nom, Une icône ", em("s'enregistrer")," & Un bouton", em(" buzzer.")),
             h4("étapes"),
             tags$ul(
               tags$li(strong("Inscription :"),
                       "Le joueur entre son nom dans la zone de texte et clique sur l’icône S’enregistrer pour valider sa participation. Sans cette étape, il ne pourra pas utiliser le buzzer."),
               tags$li(strong("Utilisation du buzzer :"),
                       "Une fois la question posée par le maître du jeu, le joueur peut appuyer sur le buzzer s’il connaît la réponse."),
               tags$li(strong("Priorité au plus rapide :"),
                       
                       " Le premier joueur à appuyer est invité à répondre."),
               tags$li(strong("Réinitialisation :"),
                       "Seul le maître du jeu peut réinitialiser le buzzer.")
             ),
             h2("Bon jeux!!!", style = "text-align: center;")
    ),
    # Onglet 2 : Quiz et Buzzer
    tabPanel("Buzzer",
             style = "background-color: #fefee3; padding: 20px; border-radius: 10px;",
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
    global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  })
}

shinyApp(ui = ui, server = server)
