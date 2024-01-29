#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # load data 
  
  job_data <- read.csv2("../data/job_data.csv")
  setDT(job_data)
  
  filteredData <- reactive({
        dataFiltered <- job_data
        if (input$secteurInput != "Tous") {
            dataFiltered <- subset(dataFiltered, SecteurEntreprise == input$secteurInput)
        }
        if (input$posteInput != "Tous") {
            dataFiltered <- subset(dataFiltered, IntituléPoste == input$posteInput)
        }
        if (input$lieuInput != "Tous") {
            dataFiltered <- subset(dataFiltered, LieuExercice == input$lieuInput)
        }
        if (input$salaireInput != "Tous") {
            dataFiltered <- subset(dataFiltered, FourchetteSalaire == input$salaireInput)
        }
        if (input$typeEmploiInput != "Tous") {
            dataFiltered <- subset(dataFiltered, TypeEmploi == input$typeEmploiInput)
        }
        dataFiltered[,.(RechercheEffectuée,IntituléPoste,FourchetteSalaire,
                        Entreprise,LieuExercice,TypeEmploi)] 
    })

    # Affichage du tableau filtré
    output$tableAnnonces <- renderDataTable({
        filteredData()
    }, options = list(lengthChange = FALSE, pageLength = 10))

    # Réaction au clic sur une ligne du tableau
    observeEvent(input$tableAnnonces_rows_selected, {
        selectedRow <- input$tableAnnonces_rows_selected
        if(length(selectedRow) > 0) {
            annonceDetails <- filteredData()[selectedRow, ]
            showModal(modalDialog(
                title = "Détails de l'Annonce",
                h3(annonceDetails$IntituléPoste),
                p(annonceDetails$DescriptionPoste),
                p(annonceDetails$FourchetteSalaire),
                footer = tagList(modalButton("Fermer"))
            ))
        }
    })
}
