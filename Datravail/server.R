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
    dataFiltered[,.(RechercheEffectuée,IntituléPoste,FourchetteSalaire,
                    Entreprise,LieuExercice,TypeEmploi)] 
  })
  
  output$tableAnnonces <- DT::renderDataTable({
    filteredData()
  }, options = list(lengthChange = FALSE, pageLength = 10,
                    autoWidth = TRUE, dom = 't'),
  selection = "single")
  
  observeEvent(input$tableAnnonces_rows_selected, {
    print("Event Triggered")
    selectedRow <- input$tableAnnonces_rows_selected
    
    # Imprimer la valeur de selectedRow pour debug mon codeeeee
    print(selectedRow)
    
    if(length(selectedRow) > 0) {
      annonceDetails <- job_data[selectedRow, ]
      showModal(modalDialog(
        title = "Détails de l'Annonce",
        h3(annonceDetails$IntituléPoste),
        
        tags$style(HTML("
    .bold-underline {
      font-weight: bold;
      text-decoration: underline;
    }
  ")),
        p(tags$p(class = "bold-underline", "Description du Poste : "), annonceDetails$DescriptionPoste),
        p(tags$p(class = "bold-underline", "Fourchette de Salaire :"), annonceDetails$FourchetteSalaire),
        p(tags$p(class = "bold-underline", "Durée de l'emploi : "), annonceDetails$DuréeEmploi),
        p(tags$p(class = "bold-underline", "Site de l'annonce :"), annonceDetails$SiteSourceAnnonce),
        p(tags$p(class = "bold-underline", "Lien de l'annonce :"), annonceDetails$LienAnnonce),
        footer = tagList(modalButton("Fermer"))
      ))
    }
  })
}
