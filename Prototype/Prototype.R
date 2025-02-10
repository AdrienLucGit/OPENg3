library(shiny)

# UI 
ui <- navbarPage("Quiz App",
                 tabPanel("Admin",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("new_question", "Nouvelle question :", ""),
                              actionButton("add_question", "Ajouter la question"),
                              br(),
                              h4("Questions enregistrées :"),
                              tableOutput("question_list"),
                              br(),
                              actionButton("start_game", "Démarrer le jeu"),
                              h4("Ordre des buzz :"),
                              tableOutput("buzz_order"),
                              actionButton("next_question", "Question suivante"),
                              actionButton("reset_buzzers", "Réinitialiser les buzzers")
                            ),
                            mainPanel(
                              h3("Question en cours :"),
                              textOutput("current_question")
                            )
                          )
                 ),
                 tabPanel("Joueur",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("player_name", "Entrez votre pseudo :", ""),
                              actionButton("register_player", "S'inscrire")
                            ),
                            mainPanel(
                              h3("Question affichée :"),
                              textOutput("display_question"),
                              actionButton("buzz", "Buzzer !", class = "btn-danger")
                            )
                          )
                 )
)

# Serveur
server <- function(input, output, session) {
  questions <- reactiveVal(list())
  players <- reactiveVal(data.frame(name = character(), buzz_time = numeric(), stringsAsFactors = FALSE))
  current_question_index <- reactiveVal(0)
  buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  
  observeEvent(input$add_question, {
    new_q <- input$new_question
    if (new_q != "") {
      questions(c(questions(), new_q))
    }
  })
  
  output$question_list <- renderTable({
    data.frame(Questions = questions())
  })
  
  observeEvent(input$start_game, {
    current_question_index(1)
    buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  })
  
  output$current_question <- renderText({
    q_list <- questions()
    if (length(q_list) >= current_question_index()) {
      q_list[[current_question_index()]]
    } else {
      "Aucune question disponible"
    }
  })
  
  output$display_question <- renderText({
    q_list <- questions()
    if (length(q_list) >= current_question_index()) {
      q_list[[current_question_index()]]
    } else {
      "En attente de question..."
    }
  })
  
  observeEvent(input$register_player, {
    name <- input$player_name
    if (name != "" && !(name %in% players()$name)) {
      players(rbind(players(), data.frame(name = name, buzz_time = Inf)))
    }
  })
  
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
  
  output$buzz_order <- renderTable({
    buzz_list()[order(buzz_list()$time), ]
  })
  
  observeEvent(input$next_question, {
    current_question_index(current_question_index() + 1)
    buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  })
  
  observeEvent(input$reset_buzzers, {
    buzz_list(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
