source("global.R") 
# UI
fluidPage(
  style = "background-color: #9370DB ; padding: 20px; border-radius: 10px;",
  titlePanel("Application Shiny avec Quiz et Buzzer"),
  
  tabsetPanel(
    # Onglet 1 : Accueil
    tabPanel("Accueil", 
             style = "background-color: #D8BFD8 ; padding: 20px; border-radius: 10px;",
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
             style = "background-color: #D8BFD8; padding: 20px; border-radius: 10px;",
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
             style = "background-color: #D8BFD8; padding: 20px; border-radius: 10px;",
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
             tags$a(href = "CGU/CGU.pdf", "Conditions générale d'utilisation", target = "_blank")
    )
  )
)