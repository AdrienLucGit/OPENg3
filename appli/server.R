source("global.R")

# Server
function(input, output, session) {
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
          actionButton("reset_partiel", "Réinitialiser le jeux", class = "btn-warning"),
          actionButton("delete_selected", "Supprimer les questions sélectionnées", class = "btn-danger"),
          h3("Ordre des buzzers :"),
          tableOutput("player_buzz_order"),
          h3("Question en cours :"),
          textOutput("current_question"),
          ##points
          h3("Points des joueurs :"),
          selectInput("player_for_points", "Choisir un joueur :", choices = NULL),  # Liste déroulante des joueurs
          numericInput("points_to_add", "nombre de points :", min = 1, value = 1),  # Nombre de points à ajouter ou soustraire
          actionButton("add_points", "Ajouter des points"),
          actionButton("remove_points","Enlever des points"),
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
          textOutput("buzz_feedback"),
          h3("Ordre des buzzers :"),
          tableOutput("player_buzz_order")
        )
      )
    }
  })
  
  output$player_buzz_order <- renderTable({
    buzz_list <- global_buzz_list()
    
    # Si la liste de buzzers n'est pas vide
    if (nrow(buzz_list) > 0) {
      # Trier les buzzers par le temps de buzz
      sorted_buzz_list <- buzz_list[order(buzz_list$time), ]
      
      # Ajouter une colonne pour la position (1er, 2ème, etc.)
      sorted_buzz_list$position <- paste0(seq_along(sorted_buzz_list$name))
      
      # Créer un data.frame pour afficher le nom du joueur et sa position
      display_buzz_order <- data.frame(
        Position = sorted_buzz_list$position,
        Joueur = sorted_buzz_list$name
      )
      
      return(display_buzz_order)
    } else {
      return(data.frame(Position = character(), Joueur = character()))
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
  output$question_list <- renderUI({
    q_list <- unlist(global_questions())
    if (length(q_list) > 0) {
      checkboxGroupInput("selected_questions", "Sélectionner les questions à supprimer :", 
                         choices = setNames(seq_along(q_list), q_list))
    } else {
      h3("Aucune question disponible.")
    }
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
  
  # Dans la partie serveur
  observeEvent(input$reset_partiel, {
    # Réinitialisation de toutes les questions
    global_questions(list())  # Vider toutes les questions
    
    # Réinitialisation de la question en cours
    global_current_question(NULL)
    
    # Réinitialisation de la liste des joueurs
    global_players(data.frame(name = character(), stringsAsFactors = FALSE))
    
    # Réinitialisation des buzzers
    global_buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
    
    # Mise à jour des sorties
    output$current_question <- renderText("")
    output$display_question <- renderText("")
    output$buzz_feedback <- renderText("Vous pouvez buzzer !")
    output$buzz_order <- renderTable(data.frame())
  })
  
  observeEvent(input$delete_selected, {
    req(input$selected_questions)  # Vérifie qu'au moins une question est sélectionnée
    q_list <- unlist(global_questions())
    q_list <- q_list[-as.numeric(input$selected_questions)]  # Supprime les questions sélectionnées
    global_questions(as.list(q_list))  # Met à jour la liste
  })
  
  #COMPTAGE DES POINTS
  #ajouter des points
  observeEvent(input$add_points, {
    player_name <- input$player_for_points
    points_to_add <- input$points_to_add
    if (player_name != "" && points_to_add > 0) {
      # Récupérer les scores actuels
      scores <- global_scores()
      
      
      # Vérifier si le joueur existe déjà dans le tableau des scores
      if (player_name %in% scores$name) {
        # Si oui, ajouter des points au joueur
        scores$points[scores$name == player_name] <- scores$points[scores$name == player_name] + points_to_add
      } else {
        # Sinon, ajouter le joueur avec les points
        scores <- rbind(scores, data.frame(name = player_name, points = points_to_add, stringsAsFactors = FALSE))
      }
      
      # Mettre à jour les scores globaux
      global_scores(scores)
    }
  })
  
  #enlever des points
  observeEvent(input$remove_points, {
    player_name <- input$player_for_points
    points_to_add <- input$points_to_add
    if (player_name != "" && points_to_add > 0) {
      # Récupérer les scores actuels
      scores <- global_scores()
      
      # Vérifier si le joueur existe dans le tableau des scores
      if (player_name %in% scores$name) {
        # Si oui, enlever des points au joueur, mais on ne permet pas d'avoir un score négatif
        new_points <- scores$points[scores$name == player_name] - points_to_add
        scores$points[scores$name == player_name] <- max(new_points, 0)  # Empêche que le score devienne négatif
      } else {
        # Si le joueur n'existe pas dans le tableau des scores, il n'y a rien à faire
        showNotification("Le joueur n'existe pas dans les scores.", type = "error")
        return(NULL)
      }
      
      # Mettre à jour les scores globaux
      global_scores(scores)
    }
  })
  
  ##Maj liste joueur
  observe({
    players <- global_players()
    updateSelectInput(session, "player_for_points", choices = players$name)
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
