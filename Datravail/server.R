library(shiny)
library(data.table)
library(tmaptools)

job_data <- read.csv2("../data/job_data.csv")
setDT(job_data)


function(input, output, session) {
  
  
  
  ################ Code de la page "Tableau des offres" ################
  filteredData <- reactive({
    
    dataFiltered <- job_data
    filters <- list(
      
      # Création des listes déroulantes pour filtrer
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
  
  # Affiche la bdd avec seulement quelques colonnes importante et filtré
  output$tableAnnonces <- DT::renderDataTable({
    filteredData()[, .(RechercheEffectuée, IntituléPoste, FourchetteSalaire,
                       Entreprise, LieuExercice, TypeEmploi, DuréeEmploi,
                       SiteSourceAnnonce, LienAnnonce)]
  }, options = list(lengthChange = FALSE, pageLength = 10,
                    autoWidth = TRUE, dom = 't'),
  selection = "single")
  
  
  # Permet de cliquer sur une ligne pour afficher plus de détail
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
  
  ################ Code de la page "Chargez CV" ################
  
  
  verifier_competences <- function(competences_offre, competences_cv) {
    competences_offre <- str_to_lower(competences_offre)
    competences_offre_liste <- unlist(str_split(competences_offre, ",\\s*"))
    any(competences_offre_liste %in% competences_cv)
  }
  
  
  # Chargement du pdf et extraction de tous les mots unique pour match avec les
  # compétences de la bdd
  
  observeEvent(input$btnAnalyse, {
    req(input$fileInput)
    
    # Lecture du fichier PDF
    chemin_pdf <- input$fileInput$datapath
    texte_cv <- tolower(pdftools::pdf_text(chemin_pdf))
    
    # Extraction des compétences du CV
    competences_cv <- str_extract_all(texte_cv, "\\b([A-Za-z]+)\\b")[[1]]
    competences_cv <- unique(competences_cv)
    job_data$CorrespondanceCV <- sapply(job_data$CompétencesDemandées,
                                        function(x) verifier_competences(x, competences_cv))
    offres_correspondantes <- job_data[job_data$CorrespondanceCV, ]
    
    # Affichage des résultats
    output$tableCorrespondances <- DT::renderDataTable({
      offres_correspondantes[, .(IntituléPoste, Entreprise,
                                 LieuExercice, CompétencesDemandées)]
    }, options = list(lengthChange = FALSE, pageLength = 10))
  })
  
  
  # Permet de cliquer sur une ligne pour afficher plus de détail
  observeEvent(input$tableCorrespondances_rows_selected, {
    selectedRow <- input$tableCorrespondances_rows_selected
    
    if(length(selectedRow) > 0) {
      offreDetails <- offres_correspondantes[selectedRow, ]
      showModal(modalDialog(
        title = "Détails de l'Offre Correspondante",
        h3(offreDetails$IntituléPoste),
        
        tags$style(HTML("
        .bold-underline {
          font-weight: bold;
          text-decoration: underline;
        }
      ")),
        p(tags$p(class = "bold-underline", "Description du Poste : "), offreDetails$DescriptionPoste),
        p(tags$p(class = "bold-underline", "Fourchette de Salaire :"), offreDetails$FourchetteSalaire),
        p(tags$p(class = "bold-underline", "Type d'emploi :"), offreDetails$TypeEmploi),
        p(tags$p(class = "bold-underline", "Durée de l'emploi : "), offreDetails$DuréeEmploi),
        p(tags$p(class = "bold-underline", "Site de l'annonce :"), offreDetails$SiteSourceAnnonce),
        p(tags$p(class = "bold-underline", "Lien de l'annonce :"), offreDetails$LienAnnonce),
        p(tags$p(class = "bold-underline", "Compétences Demandées :"), offreDetails$CompétencesDemandées),
        
        footer = tagList(modalButton("Fermer"))
      ))
    }
  })
  
  
}
