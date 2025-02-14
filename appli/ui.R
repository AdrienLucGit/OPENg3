source("global.R")  
# UI
fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  titlePanel(div(
    tags$img(src = "logobookisara.png", height="100px", style = "margin-right: 10px;"),
    "Buzzer by BOOKISARA")
  ),
  
  tabsetPanel(
    # Onglet 1 : Accueil
    tabPanel("Accueil", 
             h2("Bienvenue dans notre application ! 🎲 "),
             p("Notre application", strong(em("Buzzer💥")), 
               "est une solution simple et interactive conçue pour faciliter et dynamiser vos jeux, quiz et compétitions. 
               Elle permet aux utilisateurs d’appuyer sur un bouton virtuel pour déclencher un signal sonore, 
               indiquant ainsi qui a répondu."),
             p("Idéale pour des soirées entre amis, des événements ludiques ou des sessions de formation. 
               Cette application garantit une expérience fluide et équitable. Grâce à son interface intuitive et 
               sa réactivité instantanée, elle transforme n'importe quel appareil en un véritable buzzer de jeu. 🚀"),
             
             # Mode d'emploi Administrateur
             h3("Mode d'emploi Administrateur"),
             p("En tant qu'administrateur, vous avez plusieurs fonctionnalités pour gérer le quiz et les joueurs :"),
             tags$ul(
               tags$li(strong("créé une session:"),"rentrer le code admin et créer un code de session à partager au joueurs."),
               tags$li(strong("Télécharger votre questionnaire :"), "rentrer votre questionnaire avec vos questions préremplies. Vous pouvez obtenir un questionnaire vierge à remplir en cliquant sur le bouton ci-dessous.")
              ),
             # Bouton de téléchargement
             downloadButton("download_excel", "Télécharger un questionnaire vierge"),
             tags$ul(
               tags$li(strong("lancer un minuteur :"), " Vous pouvez lancer un minuteur avec le temps de votre choix."),
               tags$li(strong("Ajouter des questions :"), " Vous pouvez ajouter de nouvelles questions manuellement à tout moment."),
               tags$li(strong("Démarrer le jeu :"), " Cliquez sur le bouton 'Démarrer le jeu' pour commencer le quiz."),
               tags$li(strong("Passer à la question suivante :"), " Vous pouvez faire défiler les questions en cliquant sur 'Question suivante'."),
               tags$li(strong("Réinitialiser les buzzers :"), " Si nécessaire, vous pouvez réinitialiser l'ordre des buzzers."),
               tags$li(strong("Gérer les question:"), " Vous pouvez supprimer une plusieurs ou toute les questions rentré."),
               tags$li(strong("Voir l'ordre des buzzers :"), " L'ordre d'arrivée des buzzers de chacun des joueurs est affiché après chaque question."),
               tags$li(strong("Gérer des points:"), " Vous pouvez gérer l'avancement des joueurs en ajoutant et enlevant des points."),
               tags$li(strong("Gérer les joueurs :"), " Si nécessaire, vous pouvez exclure des joueurs en les supprimant de la partie.")
             ),

             
             # Mode d'emploi Joueur 
             h3("Mode d'emploi Joueur"),
             p("L'interface du mode joueur est plus restrainte."),
             tags$ul(
               tags$li(strong("Inscription :"), " le joueur entre son nom dans la zone de texte, rentre le code de session donné par l’administrateur et clique sur l’icône 's’enregistrer' pour valider sa participation. Sans cette étape, il ne pourra pas utiliser le buzzer."),
               tags$li(strong("choix du son du buzzer :"), " Le joueur peut importer un son de son choix pour son buzzer (.MP3 uniquement), sinon le son de base sera utilisé."),
               tags$li(strong("Utilisation du buzzer :"), " Une fois la question posée par le maître du jeu, celle-ci s’affiche et le joueur peut appuyer sur le buzzer s’il connaît la réponse."),
               tags$li(strong("Priorité au plus rapide :"), " Le premier joueur à appuyer est invité à répondre."),
               tags$li(strong("Réinitialisation :"), " Seul le maître du jeu peut réinitialiser le buzzer.")
             ),
             h2("Bon jeu !!! 👊", style = "text-align: center;")
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
                   actionButton("join_session", "Rejoindre la session"),
                   fileInput("custom_sound", "Uploadez un son de buzz si vous le voulez (MP3 uniquement)", accept = c(".mp3")),
                   actionButton("confirm_sound", "confirmer le buzz", class = "btn-primary"),
                   textOutput("file_name"),
                   tags$audio(
                     id = "buzz_sound",
                     src = "buzz.mp3",
                     type = "audio/mp3",
                     controls = FALSE,
                     style = "display:none;"
                   )
                 )
               ),
               mainPanel(
                 uiOutput("quiz_ui"),
                 h3(textOutput("time_left"))
               )
             )
    ),
    
    # Onglet 3 : À propos
    tabPanel("À propos", 
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
                      tags$img(src = "logo.isara.png", width = "30%")),
             tags$a(href = "CGU/CGU.pdf", "Conditions générales d'utilisation", target = "_blank")
    )
  )
)