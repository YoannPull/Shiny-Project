library(shiny)
library(data.table)

job_data <- read.csv2("../data/job_data.csv")
setDT(job_data)

function(input, output, session) {
  
  
  filteredData <- reactive({
    dataFiltered <- job_data
    
    filters <- list(
      
      # Différents Filtres
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
    
    # Filtre par saisie de compétences
    if (input$competenceInput != "") {
      # Convertir les entrées en minuscules et les diviser en mots
      competencesSaisies <- unlist(strsplit(tolower(input$competenceInput), split = "\\s*,\\s*"))
      print(paste("Compétences saisies:", competencesSaisies))
      
      # Filtrer les données pour des correspondances partielles
      dataFiltered <- dataFiltered[sapply(dataFiltered$CompétencesDemandées, function(x) {
        print(paste("Compétences pour l'offre:", x))
        
        # Vérifier si au moins un mot-clé est présent dans les compétences demandées
        matchFound <- any(sapply(competencesSaisies, function(motCle) {
          matchResult <- grepl(motCle, tolower(x))
          print(paste("Vérification de", motCle, "dans", x, ":", matchResult))
          matchResult
        }))
        
        print(paste("Correspondance trouvée:", matchFound))
        matchFound
      }), ]
    }
    dataFiltered
  })
  
  
  output$tableAnnonces <- DT::renderDataTable({
    filteredData()[, .(RechercheEffectuée, IntituléPoste, FourchetteSalaire,
                       Entreprise, LieuExercice, TypeEmploi, DuréeEmploi, SiteSourceAnnonce, LienAnnonce)]
  }, options = list(lengthChange = FALSE, pageLength = 10,
                    autoWidth = TRUE, dom = 't'),
  selection = "single")
  
  observeEvent(input$tableAnnonces_rows_selected, {
    print("Event Triggered")
    selectedRow <- input$tableAnnonces_rows_selected
    
    # print la valeur de selectedRow pour debug mon codeeeee
    print(selectedRow)
    
    if(length(selectedRow) > 0) {
      annonceDetails <- filteredData()[selectedRow, ]
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
        p(tags$p(class = "bold-underline", "Type d'emploi :"), annonceDetails$TypeEmploi),
        p(tags$p(class = "bold-underline", "Durée de l'emploi : "), annonceDetails$DuréeEmploi),
        p(tags$p(class = "bold-underline", "Site de l'annonce :"), annonceDetails$SiteSourceAnnonce),
        p(tags$p(class = "bold-underline", "Lien de l'annonce :"), annonceDetails$LienAnnonce),
        p(tags$p(class = "bold-underline", "Compétences Demandées :"), annonceDetails$CompétencesDemandées),
        
        footer = tagList(modalButton("Fermer"))
      ))
    }
  })
}
