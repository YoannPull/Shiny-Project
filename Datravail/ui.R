#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Exploration des Offres d'Emploi"),
  
  # Filtres en haut
  fluidRow(
    column(2, selectInput("secteurInput", "Secteur:", 
                          choices = c("Tous", unique(job_data$SecteurEntreprise)))),
    column(2, selectInput("posteInput", "Intitulé de Poste:", 
                          choices = c("Tous", unique(job_data$IntituléPoste)))),
    column(2, selectInput("lieuInput", "Lieu d'Exercice:", 
                          choices = c("Tous", unique(job_data$LieuExercice)))),
    column(2, selectInput("salaireInput", "Fourchette Salariale:", 
                          choices = c("Tous", unique(job_data$FourchetteSalaire)))),
    column(2, selectInput("typeEmploiInput", "Type d'Emploi:", 
                          choices = c("Tous", unique(job_data$TypeEmploi))))
  ),
  
  # Tableau principal
  mainPanel(
    dataTableOutput("tableAnnonces")
  )
)