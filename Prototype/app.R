library(shiny)
library("writexl")
library("readxl")

# Reactive values for questions and buzzers
questions <- reactiveVal(list())  # Store quiz questions
current_question_index <- reactiveVal(0)  # Track current question
buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))  # Store buzz order
players <- reactiveVal(data.frame(name = character(), buzz_time = numeric(), stringsAsFactors = FALSE))  # Player list

# UI
ui <- fluidPage(
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    
    # Onglet 1 : Accueil
    tabPanel("Accueil", 
             h2("Bienvenue !"),
             p("Ceci est une application Shiny avec plusieurs onglets."),
             # Bouton de téléchargement 
             downloadButton("download_excel", "Télécharger un questionnaire vierge")
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
                 actionButton("reset_buzzers", "Réinitialiser les buzzers"),
                 br(), hr(),
                 
                 # Téléversement du fichier Excel
                 fileInput("file_upload", "Téléverser un questionnaire", accept = c(".xlsx")),
                 actionButton("load_questions", "Charger les questions")
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
  
  # Charger un questionnaire depuis un fichier Excel
  observeEvent(input$load_questions, {
    req(input$file_upload)  # Vérifie que le fichier a bien été téléversé
    
    df <- read_xlsx(input$file_upload$datapath)  # Lecture du fichier
    
    # Vérifier que la colonne "Questions" existe
    if (!"Questions" %in% colnames(df)) {
      showNotification("Erreur : le fichier doit contenir une colonne 'Questions'.", type = "error")
      return(NULL)
    }
    
    # Mise à jour des questions avec celles du fichier
    questions(as.list(df$Questions))  # Stocke les questions sous forme de liste
  })
  
  # Afficher les questions enregistrées
  output$question_list <- renderTable({
    q_list <- unlist(questions())  # Convertit en vecteur pour éviter les erreurs
    if (length(q_list) > 0) {
      data.frame(Numéro = seq_along(q_list), Questions = q_list)
    } else {
      data.frame(Numéro = integer(), Questions = character())  # Évite une erreur si vide
    }
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
  
  # Excel prérempli
  output$download_excel <- downloadHandler(
    filename = function() {
      "questionnaires.xlsx"
    },
    content = function(file) {
      # Création d'un dataframe d'exemple
      df <- data.frame(Questions = "", stringsAsFactors = FALSE)
      # Sauvegarde en fichier Excel
      writexl::write_xlsx(df, file)
    }
  )
}

shinyApp(ui = ui, server = server)
