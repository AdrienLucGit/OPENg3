library(shiny)
library("writexl")
library("readxl")
library(beepr)

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
  style = "background-color: #ffe4b5; padding: 20px; border-radius: 10px;",
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    # Onglet 1 : Accueil
    tabPanel("Accueil", 
             style = "background-color: #fefee3; padding: 20px; border-radius: 10px;",
             h2("Bienvenue dans l'application Quiz !"),
             p("Notre application", strong(em("Buzzer")), 
               "est une solution simple et interactive conçue pour faciliter et dynamiser vos jeux, quiz et compétitions. 
               Elle permet aux utilisateurs d’appuyer sur un bouton virtuel pour déclencher un signal sonore, 
               indiquant ainsi qui a répondu."),
             p("Idéale pour des soirées entre amis, des événements ludiques ou des sessions de formation, 
               cette application garantit une expérience fluide et équitable. Grâce à son interface intuitive et 
               sa réactivité instantanée, elle transforme n'importe quel appareil en un véritable buzzer de jeu. 🚀"),
             
             # Mode d'emploi Administrateur
             h3("Mode d'emploi Administrateur"),
             p("En tant qu'administrateur, vous avez plusieurs fonctionnalités pour gérer le quiz et les joueurs :"),
             tags$ul(
               tags$li(strong("Ajouter des questions :"), " Vous pouvez ajouter de nouvelles questions à tout moment."),
               tags$li(strong("Démarrer le jeu :"), " Cliquez sur le bouton 'Démarrer le jeu' pour commencer le quiz."),
               tags$li(strong("Passer à la question suivante :"), " Vous pouvez faire défiler les questions en cliquant sur 'Question suivante'."),
               tags$li(strong("Réinitialiser les buzzers :"), " Si nécessaire, vous pouvez réinitialiser l'ordre des buzzers."),
               tags$li(strong("Voir l'ordre des buzzers :"), " L'ordre d'arrivée des joueurs au buzzer est affiché après chaque question."),
               tags$li(strong("Bloquer ou exclure les buzzers :"), " Si nécessaire, vous pouvez bloquer ou exclure un buzzer."),
               tags$li(strong("Télécharger votre questionnaire :"), " Vous pouvez obtenir un questionnaire vierge à remplir en cliquant sur le bouton ci-dessous.")
             ),
             
             # Bouton de téléchargement
             downloadButton("download_excel", "Télécharger un questionnaire vierge"),
             
             # Mode d'emploi Joueur 
             h3("Mode d'emploi Joueur"),
             p("L'interface du mode joueur comprend une zone de texte pour entrer son nom, une icône ", em("s'enregistrer"), " et un bouton ", em("buzzer.")),
             h4("Étapes"),
             tags$ul(
               tags$li(strong("Inscription :"), " Le joueur entre son nom dans la zone de texte et clique sur l’icône S’enregistrer pour valider sa participation. Sans cette étape, il ne pourra pas utiliser le buzzer."),
               tags$li(strong("Utilisation du buzzer :"), " Une fois la question posée par le maître du jeu, le joueur peut appuyer sur le buzzer s’il connaît la réponse."),
               tags$li(strong("Priorité au plus rapide :"), " Le premier joueur à appuyer est invité à répondre."),
               tags$li(strong("Réinitialisation :"), " Seul le maître du jeu peut réinitialiser le buzzer.")
             ),
             h2("Bon jeu !!!", style = "text-align: center;")
    ),
    
    # Onglet 2 : Quiz et Buzzer
    tabPanel("Buzzer", 
             sidebarLayout(
               sidebarPanel(
                 radioButtons("user_role", "Choisissez votre rôle :", 
                              choices = c("Admin", "Joueur"), 
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
    ),
    
    # Onglet 3 : À propos
    tabPanel("À propos", 
             style = "background-color: #fefee3; padding: 20px; border-radius: 10px;",
             h2("À propos de notre application"),
             p("Merci d'avoir utilisé notre application !"),
             h3("Auteurs :"),
             p("- BARRET Anthony"),
             p("- BLANPAIN Chloé"),
             p("- BLIGUET Typhaine"),
             p("- CHI ACHERE Desmond"),
             p("- FAURE Marie"),
             p("- FAYAD Adib"),
             p("- LUC Adrien"),
             p("- MEFFRE ALEXANDRE Simon"),
             p("- POTTIAU Zoé"),
             h3("ISARA Lyon - Février 2025"),
             tags$div(style = "text-align: center;", 
                      tags$img(src = "logo.isara.png", width = "30%"))
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
          fileInput("file_upload", "Téléverser un questionnaire", accept = c(".xlsx")),
          actionButton("load_questions", "Charger les questions"),
          textInput("new_question", "Nouvelle question :", ""),
          actionButton("add_question", "Ajouter la question"),
          tableOutput("question_list"),
          actionButton("start_game", "Démarrer le jeu"),
          actionButton("next_question", "Question suivante"),
          actionButton("reset_buzzers", "Réinitialiser les buzzers"),
          actionButton("clear_questions", "Supprimer toutes les questions", class = "btn-danger"),
          h3("Ordre des buzz :"),
          tableOutput("buzz_order"),
          h3("Question en cours :"),
          textOutput("current_question"),
          #LISTE DES JOUEURS
          h3("Liste des joueurs enregistrés :"),
          tableOutput("player_list")
        )
      )
    } else {
      return(
        fluidPage(
          h2(paste("Bienvenue,", session_data$player_name)),
          h3("Question en cours :"),
          textOutput("display_question"),
          selectInput("sound_choice", "Choisissez votre son :", 
                      choices = c("DING !" = 1, "Victoire !" = 5, "Cri" = 9), selected = 1),
          actionButton("buzz", "Buzzer !", class = "btn-danger"),
          textOutput("buzz_feedback")
        )
      )
    }
  })
  
  
  #MAJ JOUEUR LISTE
  output$player_list <- renderTable({
    players <- global_players()
    
    if (nrow(players)>0) {
      return(players) #AFFICHAGE NOM JOUEURS
    }
    else {
      return(data.frame(name ="Aucun joueur enregistré", stringsAsFactors = FALSE))
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
  #FONCTION DE CHARGEMENT DES QUESTIONS AVEC TXT ERREUR 
  observeEvent(input$load_questions, {
    req(input$file_upload)
    df <- readxl::read_xlsx(input$file_upload$datapath)
    if (!"Questions" %in% colnames(df)) {
      showNotification("Erreur : le fichier doit contenir une colonne 'Questions'.", type = "error")
      return(NULL)
    }
    global_questions(as.list(df$Questions))
  })
  #FONCTION DOWNLOAD EXCEL
  output$download_excel <- downloadHandler(
    filename = function() { "questionnaires.xlsx" },
    content = function(file) {
      df <- data.frame(Questions = "", stringsAsFactors = FALSE)
      writexl::write_xlsx(df, file)
    }
  )
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
  
  observeEvent(input$clear_questions, {
    global_questions(list())
  })
  
  #PERMET D'AVOIR LE NUMERO DES QUESTIONS AVEC LA LISTE
  output$question_list <- renderTable({
    q_list <- unlist(global_questions())
    if (length(q_list) > 0) {
      data.frame(Numéro = seq_along(q_list), Questions = q_list)
    } else {
      data.frame(Numéro = integer(), Questions = character())
    }
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
        beep(sound = as.numeric(input$sound_choice))  # Jouer le son choisi
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
    current_question <- global_current_question()  # Récupère la question actuelle
    current_index <- match(current_question, q_list)  # Trouve l'index de la question actuelle
    
    if (!is.na(current_index) && current_index < length(q_list)) {
      global_current_question(q_list[[current_index + 1]])  # Passe à la question suivante
      
      # Réinitialisation des buzzers
      global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
      
      # Réinitialisation du message du buzzer
      output$buzz_feedback <- renderText("Vous pouvez buzzer !")
    }
  })
  

  observeEvent(input$reset_buzzers, {
    req(input$reset_buzzers)  # Vérifie que le bouton a bien été pressé
    # Réinitialisation de la liste des buzzers
    global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
    # Suppression du message
    output$buzz_feedback <- renderText({"Vous pouvez buzzer !"})
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