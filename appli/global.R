library(shiny)
library(writexl)
library(readxl)
library(beepr)
library(shinyjs)  
library(lubridate)

# Variables globales
session_password <- reactiveVal(NULL)  # Stocke le code de session défini par l'Admin
admin_password <- "admin123"  # Mot de passe fixe pour l'Admin
global_questions <- reactiveVal(list())
global_current_question <- reactiveVal("")
global_buzz_list <- reactiveVal(data.frame(name = character(), time = numeric(), stringsAsFactors = FALSE))
global_players <- reactiveVal(data.frame(name = character(), stringsAsFactors = FALSE))
admin_chosen <- reactiveVal(FALSE)  # Indicateur pour bloquer le choix du rôle après sélection de l'Admin
error_message <- reactiveVal("")  # Message d'erreur pour le mot de passe
global_scores <- reactiveVal(data.frame(name = character(), points = numeric(), stringsAsFactors = FALSE))#stock le score des joueurs
timer <- reactiveVal(10) #chrono
active <- reactiveVal(FALSE) # chrono
show_podium <- reactiveVal(FALSE)
