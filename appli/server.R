source("global.R")

# Server
function(input, output, session) {
  session_data <- reactiveValues(role = NULL, player_name = NULL)
  
  ##Podium
  get_top_3_scores <- reactive({
    scores <- global_scores()
    if(nrow(scores) > 0) {
      top_3 <- head(scores[order(-scores$points), ], 3)
      return(top_3)
    } else {
      return(data.frame(name = character(0), points = numeric(0)))
    }
  })
  
  output$podium_content <- renderUI({
    if(show_podium()) {
      top_3 <- get_top_3_scores()
      if(nrow(top_3) > 0) {
        tagList(
          h2("Podium", style = "text-align: center;"),
          div(style = "display: flex; justify-content: center; align-items: flex-end; height: 300px;",
              if(nrow(top_3) >= 2) {
                div(style = "text-align: center; margin: 0 10px;",
                    div(style = "background-color: silver; width: 100px; height: 180px;"),
                    h3(top_3$name[2]),
                    p(paste(top_3$points[2], "points"))
                )
              },
              if(nrow(top_3) >= 1) {
                div(style = "text-align: center; margin: 0 10px;",
                    div(style = "background-color: gold; width: 100px; height: 200px;"),
                    h3(top_3$name[1]),
                    p(paste(top_3$points[1], "points"))
                )
              },
              if(nrow(top_3) >= 3) {
                div(style = "text-align: center; margin: 0 10px;",
                    div(style = "background-color: #cd7f32; width: 100px; height: 160px;"),
                    h3(top_3$name[3]),
                    p(paste(top_3$points[3], "points"))
                )
              }
          )
        )
      } else {
        h3("Pas encore de scores disponibles", style = "text-align: center;")
      }
    }
  })
  
  observeEvent(input$show_podium, {
    show_podium(TRUE)
    updateTabsetPanel(session, "tabsetPanel", selected = "Podium")
    session$sendCustomMessage(type = 'selectTab', message = 'Podium')
  })

  ###################choix buzzer#############
  observeEvent(input$confirm_sound, { 
    req(input$custom_sound)  # Vérifie que l'utilisateur a bien sélectionné un fichier avant de continuer
    
    tryCatch({  # Utilisation de tryCatch pour gérer les erreurs éventuelles
      tmp_file <- input$custom_sound$datapath  # Récupère le chemin temporaire du fichier téléchargé
      final_path <- file.path("www", "buzz.mp3")  # Définit l'emplacement final du fichier (dans le dossier "www")
      
      file.copy(tmp_file, final_path, overwrite = TRUE)  # Copie le fichier vers le nouvel emplacement en écrasant s'il existe déjà
      
      runjs("document.getElementById('buzz_sound').load();")  # Recharge l'élément audio sur la page pour prendre en compte le nouveau fichier
      
      output$file_name <- renderText({  # Met à jour un élément texte dans l'interface utilisateur
        paste("buzz.mp3")  # Affiche un message confirmant l'enregistrement du fichier
      })
    },
    error = function(e) {  # Capture et gère les erreurs qui pourraient survenir
      output$file_name <- renderText({  # Met à jour l'élément texte avec un message d'erreur
        paste("Error saving file:", e$message)  # Affiche l'erreur rencontrée
      })
    })
  })
  
  ##################################################
  
  
  
  #timer <- reactiveVal(10) #chrono
  #active <- reactiveVal(FALSE) # chrono
  
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
          fluidRow(
            column(8, fileInput("file_upload", "Téléverser un questionnaire", accept = c(".xlsx"))),
            column(8, actionButton("load_questions", "Charger les questions", 
                                   style = "font-size: 14px; padding: 6px; width: 100%;"))
          ),
          
          br(),
          useShinyjs(),
          fluidRow(
            column(3, actionButton("start", "START", 
                                   style = "font-size: 12px; padding: 6px; width: 90%;")),
            column(3, actionButton("stop", "STOP", 
                                   style = "font-size: 12px; padding: 6px; width: 90%;")),
            column(3, actionButton("reset", "RESET", 
                                   style = "font-size: 12px; padding: 6px; width: 90%;")),
          ),
          br(),
          
          numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1), #chrono
          output$timeleft <- renderText({ #chrono
            paste("Time left: ", seconds_to_period(timer()))#chrono
          }), #chrono
          textInput("new_question", "Nouvelle question :", ""),
          actionButton("add_question", "Ajouter la question"),
          tableOutput("question_list"),
          fluidRow(
            column(6, actionButton("start_game", "Démarrer le jeu", 
                                   style = "font-size: 12px; padding: 6px; width: 90%;")),
            column(6, actionButton("next_question", "Question suivante", 
                                   style = "font-size: 12px; padding: 6px; width: 90%;"))
          ),
          br(),
          fluidRow(
            column(6, actionButton("reset_buzzers", "Réinitialiser les buzzers", 
                                   style = "font-size: 12px; padding: 6px; width: 90%;")),
            column(6, actionButton("reset_partiel", "Réinitialiser le jeu", 
                                   style = "font-size: 12px; padding: 6px; width: 90%;"))
          ),
          
          br(),
          fluidRow(
            column(6,actionButton("clear_questions", "Supprimer toutes les questions", style = "font-size: 12px; padding: 6px; width: 90%;")),
            column(6,actionButton("delete_selected", "Supprimer les questions sélectionnées", style = "font-size: 12px; padding: 6px; width: 100%;")),
          ),
          
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
          tableOutput("player_list"),
          tags$audio(id = "fin_sound", src = "fin.mp3", type = "audio/mp3", 
                     controls = FALSE, style = "display:none;")
        )
      )
    } else {
      return(
        fluidPage(
          h2(paste("Bienvenue,", session_data$player_name)),
          h3("Question en cours :"),
          textOutput("display_question"),
          useShinyjs(),  # Active shinyjs pour exécuter du JavaScript
          # Ajout du fichier son avec précautions pour éviter le déclenchement automatique
          tags$audio(id = "buzz_sound", src = "buzz.mp3", type = "audio/mp3",
                     controls = FALSE, style = "display:none;"),
          
          # Interface avec le bouton
          actionButton("buzz", "Buzzer !", class = "btn btn-primary", style = "font-size: 40px; padding: 20px 40px; width: 300px; display: block; margin: 50px auto;"),
          textOutput("buzz_feedback"),
          h3("Ordre des buzzers :"),
          tableOutput("player_buzz_order"),
          output$timeleft <- renderText({ #chrono
            paste("Time left: ", seconds_to_period(timer()))#chrono
          }), #chrono
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
  ###############################chrono###############################
  observe({
    invalidateLater(2000, session)
    isolate({
      if(active()) {
        timer(timer()-1)
        if(timer() < 1) {
          active(FALSE)
          
          runjs("
            var audio = document.getElementById('fin_sound');
            if (audio.paused || audio.ended) {
              audio.currentTime = 0;
              audio.play();
            }
          ")
          
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })
  
  observeEvent(input$start, {active(TRUE)}) #chrono
  observeEvent(input$stop, {active(FALSE)}) #chrono
  observeEvent(input$reset, {timer(input$seconds)}) #chrono
  
  #MAJ JOUEUR LISTE
  output$player_list <- renderTable({
    players <- global_players()
    
    if (nrow(players) > 0) {
      scores <- global_scores()
      players_with_scores <- merge(players, scores, by = "name", all.x = TRUE)
      players_with_scores[is.na(players_with_scores$points), "points"] <- 0  # Initialiser les points à 0 s'ils sont NA
      
      # Tri par points (du plus grand au plus petit)
      players_with_scores <- players_with_scores[order(-players_with_scores$points), ]
      
      return(players_with_scores) # Affichage du nom des joueurs avec leurs points
    } else {
      return(data.frame(name = "Aucun joueur enregistré", points = 0, stringsAsFactors = FALSE))
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
    players <- global_players()
    
    if (name %in% players$name) {
      buzz_time <- Sys.time()
      current_buzzers <- global_buzz_list()
      if (!(name %in% current_buzzers$name)) {
        new_buzz <- data.frame(name = name, time = buzz_time)
        global_buzz_list(rbind(current_buzzers, new_buzz))
        output$buzz_feedback <- renderText("Buzz enregistré !")
        runjs("
          var audio = document.getElementById('buzz_sound');
          if (audio.paused || audio.ended) {
            audio.currentTime = 0;
            audio.play();
          }
        ")
      }else {
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
    
    global_scores((data.frame(name = character(0), points = numeric(0))))
    
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
    req(input$player_for_points)  # Vérifie qu'un joueur est sélectionné
    scores <- global_scores()
    
    # Vérifier si le joueur existe déjà dans le tableau des scores
    if (player_name %in% scores$name) {
      # Si oui, ajouter des points au joueur
      scores[scores$name == input$player_for_points, "points"] <- 
        scores[scores$name == input$player_for_points, "points"] + input$points_to_add
    } else {
      # Sinon, ajouter le joueur avec les points
      scores <- rbind(scores, data.frame(name = player_name, points = points_to_add, stringsAsFactors = FALSE))
    }
    
    # Mettre à jour les scores globaux
    global_scores(scores)
    # Forcer la mise à jour de la liste triée
    output$player_list <- renderTable({
      players_with_scores <- merge(global_players(), global_scores(), by = "name", all.x = TRUE)
      players_with_scores[is.na(players_with_scores$points), "points"] <- 0
      players_with_scores <- players_with_scores[order(-players_with_scores$points), ]
      return(players_with_scores)
    })
  })
  
  #enlever des points
  observeEvent(input$remove_points, {
    player_name <- input$player_for_points
    points_to_add <- input$points_to_add
    req(input$player_for_points)  # Vérifie qu'un joueur est sélectionné
    scores <- global_scores()
    
    
    
    # Vérifier si le joueur existe dans le tableau des scores
    if (player_name %in% scores$name) {
      # Si oui, enlever des points au joueur, mais on ne permet pas d'avoir un score négatif
      scores[scores$name == input$player_for_points, "points"] <- 
        max(0, scores[scores$name == input$player_for_points, "points"] - input$points_to_add)
    } else {
      # Si le joueur n'existe pas dans le tableau des scores, il n'y a rien à faire
      showNotification("Le joueur n'existe pas dans les scores.", type = "error")
      return(NULL)
    }
    
    # Mettre à jour les scores globaux
    global_scores(scores)
    
    # Forcer la mise à jour de la liste triée
    output$player_list <- renderTable({
      players_with_scores <- merge(global_players(), global_scores(), by = "name", all.x = TRUE)
      players_with_scores[is.na(players_with_scores$points), "points"] <- 0
      players_with_scores <- players_with_scores[order(-players_with_scores$points), ]
      return(players_with_scores)
      
    })
  })
  
  ##Maj liste joueur
  observe({
    players <- global_players()
    updateSelectInput(session, "player_for_points", choices = players$name)
  })
  
  ######################################
  
  output$player_list <- renderUI({
    players <- as.data.frame(global_players())  # Liste des joueurs
    scores <- as.data.frame(global_scores())   # Liste des scores
    
    if (nrow(players) > 0) {
      tagList(
        lapply(1:nrow(players), function(i) {
          player_name <- players$name[i]
          player_score <- if (player_name %in% scores$name) {
            scores$points[scores$name == player_name]
          } else {
            0  # Si le joueur n’a pas encore de score, on affiche 0
          }
          
          div(
            style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 5px;",
            span(paste0(player_name, " - ", player_score, " points"), 
                 style = "margin-right: 10px; font-size: 16px;"),
            actionButton(inputId = paste0("remove_", i), label = "❌", 
                         style = "color: red; background: none; border: none; font-size: 18px; cursor: pointer;")
          )
        })
      )
    } else {
      h3("Aucun joueur disponible.")
    }
  })
  
  observe({
    players <- as.data.frame(global_players())  # Récupère la liste actuelle
    
    if (nrow(players) > 0) {
      lapply(1:nrow(players), function(i) {
        observeEvent(input[[paste0("remove_", i)]], {
          if (i <= nrow(players)) {
            updated_players <- players[-i, , drop = FALSE]  # Supprime la ligne du joueur
            global_players(updated_players)  # Mise à jour immédiate
          }
        }, ignoreInit = TRUE)  # Empêche l'exécution au démarrage
      })
    }
  })
  
  observe({
    if(show_podium()) {
      session$sendCustomMessage(type = 'selectTab', message = 'Podium')
    }
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
    } )
}

