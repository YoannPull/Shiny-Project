library(shiny)
library(data.table)
library(tmaptools)
library(dplyr)
library(httr)
library(shinyWidgets)

job_data <- read.csv2("../data/data2.csv")

setDT(job_data)


function(input, output, session) {
  
  ################ Code de la page "Accueil" ################
  
  observeEvent(input$ButtonTableau, {
    # Actions à effectuer pour l'étape 1
    # Par exemple, vous pouvez afficher des instructions spécifiques ou naviguer vers une autre partie de l'application.
    updateTabItems(session, "tabs", selected = "tableau")
  })

  observeEvent(input$ButtonCarte, {
    # Actions à effectuer pour l'étape 1
    # Par exemple, vous pouvez afficher des instructions spécifiques ou naviguer vers une autre partie de l'application.
    updateTabItems(session, "tabs", selected = "map")
  })
  
  observeEvent(input$ButtonCV, {
    # Actions à effectuer pour l'étape 1
    # Par exemple, vous pouvez afficher des instructions spécifiques ou naviguer vers une autre partie de l'application.
    updateTabItems(session, "tabs", selected = "cv")
  })
  
  #bs_themer()
  
  ################ Code de la page "Tableau des offres" ################
  
  filteredData <- reactive({
    
    dataFiltered <- job_data
    filters <- list(
      
      # Création des listes déroulantes pour filtrer
      SecteurActivité = input$secteurInput,
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
  }, style = 'bootstrap',
  options = list(
    pageLength = 5,  # Définit le nombre d'entrées par page
    autoWidth = TRUE,  # Ajuste automatiquement la largeur des colonnes
    dom = 'ftpi',  # Configure les éléments de contrôle à afficher (longueur, filtrage, table, informations, pagination)
    language = list(
      search = '<i class="fa fa-search" aria-hidden="true"></i>',
      searchPlaceholder = 'Cherchez un job de la data, une entreprise, une ville...'
    )
  ),
  selection = "single",
  callback = JS(
    "table.on('init.dt', function() {
        $('.dataTables_filter input').css('width', '500px'); // Ajustez la largeur comme vous le souhaitez
      });"
  )
  )
  
  
  
  
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
        
        footer = tagList(modalButton("Fermer")),
        # JS pour fermer la page si on clique en dehors de la fenêtre
        easyClose = TRUE,
        tags$script(HTML("
        $(document).on('click', '.modal-backdrop', function(){
          $('.modal').modal('hide');
        });
      "))
      ))
    }
  })
  
  ################ Code de la page "Map" ################
  
  counts_per_ville <- table(job_data$LieuExercice) # compte le nombre d'offre par ville
  
  output$mymap <- renderLeaflet({
    leaflet(job_data) %>%
      setView(lng = 7.098110651792706, lat = 42.92920993114715, zoom = 5) %>%  # Vue centrée sur la France
      
      addProviderTiles(providers$Stadia.AlidadeSmooth,  # Changement du fond de carte pour un style plus esthétique
                       options = providerTileOptions(noWrap = TRUE)) %>%
      
      addCircleMarkers(data = job_data,
                       ~lon, ~lat,  # Coordonnées des cercles
                       radius = 8,  # Rayon des cercles
                       fillOpacity = 0.8,  # Opacité de remplissage
                       color = "#D7BDE2",  # Couleur des cercles en violet très clair
                       fillColor = "#D7BDE2",  # Couleur de remplissage des cercles
                       popup = ~paste0("<strong>", LieuExercice, "</strong>: ", counts_per_ville[LieuExercice], " offre(s)", "<br>",
                                       "<a href=\"#\" onclick=\"Shiny.setInputValue('selectedCity', '", LieuExercice, "', {priority: 'event'});\">Voir les offres</a>"),
                       group = "markers")  
  })
  
  
  observeEvent(input$selectedCity, {
    updateTabItems(session,inputId = "tabs", selected = "tableau")
    updateTextInput(session, "lieuInput", value = input$selectedCity)
  })
  
  
  ################ Code de la page "Chargez CV" ################
  
  
  verifier_competences <- function(competences_offre, competences_cv) {
    competences_offre <- tolower(competences_offre)
    competences_offre_liste <- unlist(str_split(competences_offre, ",\\s*"))
    nb_correspondances <- sum(competences_offre_liste %in% competences_cv)
    proportion_correspondances <- round(100 * nb_correspondances / length(competences_offre_liste),2)
    return(proportion_correspondances) # Retourne la proportion de compétences qui matchent
  }
  
  offres_correspondantes <- reactiveVal()
  
  observeEvent(input$btnAnalyse, {
    req(input$fileInput)
    
    # Lecture du fichier PDF
    chemin_pdf <- input$fileInput$datapath
    texte_cv <- tolower(pdftools::pdf_text(chemin_pdf))
    
    # Extraction des compétences du CV
    competences_cv <- str_extract_all(texte_cv, "\\b([A-Za-z]+)\\b")[[1]]
    competences_cv <- unique(competences_cv)
    
    # Calcul de la proportion de correspondance pour chaque offre
    job_data$ProportionCompetencesCorrespondantes <- sapply(job_data$CompétencesDemandées,
                                                            function(x) verifier_competences(x, competences_cv))
    
    # Filtrer pour garder seulement les offres avec au moins une compétence correspondante (proportion > 0)
    job_data_filtré <- job_data[job_data$ProportionCompetencesCorrespondantes > 0, ]
    
    # Trier les offres par la proportion de compétences correspondantes en ordre décroissant
    offres_triees <- job_data_filtré[order(-job_data_filtré$ProportionCompetencesCorrespondantes), ]
    
    offres_correspondantes(offres_triees)
    
    # Affichage des résultats avec la proportion de compétences qui matchent
    output$tableCorrespondances <- DT::renderDataTable({
      offres_correspondantes()[, .(IntituléPoste,
                                   Entreprise,
                                   LieuExercice,
                                   CompétencesDemandées,
                                   "Proportion Competences Correspondantes" = paste0(ProportionCompetencesCorrespondantes,"%"))]
    }, options = list(lengthChange = FALSE, pageLength = 10, autoWidth = TRUE, dom = 'tpi'),
    selection = "single")
  })
  
  
  
  # Permet de cliquer sur une ligne pour afficher plus de détail
  observeEvent(input$tableCorrespondances_rows_selected, {
    selectedRow <- input$tableCorrespondances_rows_selected
    print(selectedRow)
    
    if(length(selectedRow) == 1) {
      offreDetails <- offres_correspondantes()[selectedRow, ]
      showModal(modalDialog(
        title = "Détails de l'Offre Correspondante",
        h3(as.character(offreDetails$IntituléPoste)),
        
        tags$style(HTML("
      .bold-underline {
        font-weight: bold;
        text-decoration: underline;
      }
      ")),
        p(tags$p(class = "bold-underline", "Description du Poste : "), as.character(offreDetails$DescriptionPoste)),
        p(tags$p(class = "bold-underline", "Fourchette de Salaire :"), as.character(offreDetails$FourchetteSalaire)),
        p(tags$p(class = "bold-underline", "Type d'emploi :"), as.character(offreDetails$TypeEmploi)),
        p(tags$p(class = "bold-underline", "Durée de l'emploi : "), as.character(offreDetails$DuréeEmploi)),
        p(tags$p(class = "bold-underline", "Site de l'annonce :"), as.character(offreDetails$SiteSourceAnnonce)),
        p(tags$p(class = "bold-underline", "Lien de l'annonce :"), as.character(offreDetails$LienAnnonce)),
        p(tags$p(class = "bold-underline", "Compétences Demandées :"), as.character(offreDetails$CompétencesDemandées)),
        
        footer = tagList(modalButton("Fermer")),
        
        # JS pour fermer la page si on clique en dehors de la fenêtre
        easyClose = TRUE,
        tags$script(HTML("
        $(document).on('click', '.modal-backdrop', function(){
          $('.modal').modal('hide');
        });
      "))
      ))
    }
  })
}