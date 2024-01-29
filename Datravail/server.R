library(shiny)
library(data.table)
job_data <- read.csv2("../data/job_data.csv")
setDT(job_data)

function(input, output, session) {

  
  filteredData <- reactive({
    dataFiltered <- job_data
    filters <- list(
      SecteurEntreprise = input$secteurInput,
      IntituléPoste = input$posteInput,
      LieuExercice = input$lieuInput,
      FourchetteSalaire = input$salaireInput,
      TypeEmploi = input$typeEmploiInput
    )
    for (filterName in names(filters)) {
      if (filters[[filterName]] != "Tous") {
        dataFiltered <- dataFiltered[get(filterName) == filters[[filterName]], ]
      }
    }
    dataFiltered
  })
  
  output$tableAnnonces <- DT::renderDataTable({
    filteredData()
  }, options = list(lengthChange = FALSE, pageLength = 10, autoWidth = TRUE),
  selection = "single")
  
  observeEvent(input$tableAnnonces_rows_selected, {
    print("Event Triggered")
    selectedRow <- input$tableAnnonces_rows_selected
    
    # Imprimer la valeur de selectedRow pour le débogage
    print(selectedRow)
    
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
