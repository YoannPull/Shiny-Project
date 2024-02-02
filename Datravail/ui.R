#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(leaflet)
library(tmaptools)
library(ggmap)
library(pdftools)
library(tesseract)
library(stringr)
library(bslib)

job_data <- read.csv2("../data/job_data.csv")
setDT(job_data)


fluidPage(
  
  theme =bs_theme(
    preset = "sandstone",fg = "#000",bg = "#FFFFFF", primary = "#000", secondary = "#77798A", 
    font_scale = 1),
  
  
  
  titlePanel("Exploration des Offres d'Emploi"),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("Welcome Page",
             fluidRow(
               column(12,
                      h3("Bienvenue dans le Guide d'Utilisation !"),
                      p("Ce guide vous aidera à comprendre comment utiliser l'application afin de pouvoir explorer les différentes offres d'emplois,
                        "),
                      h4("Tableau des offres"),
                      p("Cette page permet.")
                      # Ajoutez plus d'étapes ici
               )
             )
    ),
    
    ################ Code de la page "Tableau des offres" ################
    
    tabPanel("Tableau des offres",
             
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
             fluidRow(
               column(6, textInput("competenceInput", "Compétences:", placeholder = "Tapez des compétences ici"))
             ),
             # Tableau principal
             mainPanel(
               DT::dataTableOutput("tableAnnonces")
             )
    ),
    ################ Code de la page "MAP" ################
    tabPanel("Map",
      mainPanel(
             leafletOutput("mymap", width = "80%",
                           height = "600px"),
             p()
    )),
    ################ Code de la page "Chargez CV" ################
    
    tabPanel("Charger CV",
             fluidRow(
               column(6, fileInput("fileInput", "Charger son CV (format PDF uniquement)", 
                                   accept = c(".pdf"))),
               column(6, actionButton("btnAnalyse", "Analyser les Compétences"))
             ),
             DT::dataTableOutput("tableCorrespondances")
    )
  ) 
  
  

)