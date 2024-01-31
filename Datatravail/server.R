library(shiny)
library(data.table)
library(tmaptools)
library(dplyr)
library(httr)

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
  ################ Code de la page "Map" ################
  
  # fonction pour avoir la latitude et la longitude d'une ville
  get_lat_long <- function(city_name) {
    base_url <- "https://nominatim.openstreetmap.org/search"
    params <- list(
      format = "json",
      q = city_name
    )
    
    response <- GET(url = base_url, query = params)
    data <- content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(data)
    
    if (length(data) > 0) {
      location <- data[1, ]
      latitude <- as.numeric(location$lat)
      longitude <- as.numeric(location$lon)
      return(c(latitude, longitude))
    } else {
      cat("Erreur lors de la récupération des coordonnées.\n")
      return(NULL)
    }
  }
  
  job_data$lat <- NA
  job_data$lon <- NA
  
  # ajout de latitude et longitude dans notre DT
  for (i in seq_along(job_data$LieuExercice)) {
    ville <- job_data$LieuExercice[i]
    coordinates <- get_lat_long(ville)
    job_data$lat[i] <- coordinates[1]
    job_data$lon[i] <- coordinates[2]
  }
  
  counts_per_ville <- table(job_data$LieuExercice) # compte le nombre d'offre par ville
  
  output$mymap <- renderLeaflet({
    leaflet(job_data) %>%
      setView(lng = 2.2137, lat = 46.6031, zoom = 5) %>%  #on set la view sur la france
      
      addProviderTiles(providers$Stadia.StamenTonerLite,
                       options = providerTileOptions(noWrap = TRUE) # carte de fond
      ) %>%
      addCircleMarkers(data = job_data,
                       lng = ~lon,
                       lat = ~lat,
                       radius = 8,  # Rayon des cercles
                       fillOpacity = 0.8,  # Opacité de remplissage
                       color = "salmon",  # Couleur des cercles
                       popup = ~paste0("<strong>", LieuExercice, "</strong>: ", counts_per_ville[LieuExercice], " offre(s)", "<br>",
                                       "<a href=\"#\" onclick=\"Shiny.setInputValue('selectedCity', '", LieuExercice, "', {priority: 'event'});\">Voir les offres</a>"),  # Popup avec le nombre d'offres correspondantes
                       group = "markers")  # Ajout d'un groupe pour une gestion plus facile
  
  })
  
  observeEvent(input$selectedCity, {
    updateTabsetPanel(session, "tabs", selected = "Tableau des offres")
    updateTextInput(session, "lieuInput", value = input$selectedCity)
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
    
    #j'ecris un prout
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