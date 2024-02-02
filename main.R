library(data.table)
library(httr)
library(jsonlite) # Assurez-vous d'avoir jsonlite pour la fonction fromJSON

data <- fread("data/full_data.csv", header = TRUE)
data$Département <- as.character(data$Département)

#https://www.data.gouv.fr/fr/datasets/departements-de-france/
data_dep <- fread("data/departements-france.csv")


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

# Create lat, lon, and NomLieu columns
data$lat <- NA
data$lon <- NA


data[LieuExercice == "Case", LieuExercice := "NC"]
data[LieuExercice == "", LieuExercice := "NC"]

data[LieuExercice == "Paris - La Défense", Département := "92"]
data[LieuExercice == "Suresnes", Département := "92"]
data[LieuExercice == "Île-de-France", Département := "75"]
data[LieuExercice == "Île-de-France - Auvergne-Rhône-Alpes", Département := "75"]
data[LieuExercice == "Paris, Pessac", Département := "75"]
data[LieuExercice == "Bordeaux, Montpellier", Département := "33"]
data[LieuExercice == "Paris, Bordeaux", Département := "75"]
data[LieuExercice == "Ajaccio - 2A", Département := "2A"]
data[LieuExercice == "Bastia - 2B", Département := "2B"]
data[LieuExercice == "Normandie", Département := "76"]
data[LieuExercice == "Grand Est", Département := "67"]
data[LieuExercice == "Rhône-Alpes", Département := "69"]
data[LieuExercice == "Bretagne", Département := "35"]
data[LieuExercice == "Languedoc-Roussillon - Sud-Ouest -", Département := "31"]
data[LieuExercice == "Sainte-Luce-sur-Loire, Paris", Département := "75"]
data[LieuExercice == "Champagne-Ardenne - Nord -", Département := "51"]
data[LieuExercice == "NC", Département := NA]
data[LieuExercice == "France", Département := NA ]



data <- merge(data, data_dep, by.x = "Département" ,by.y = "code_departement", 
              all.x = TRUE,allow.cartesian=TRUE)

data$LieuExerciceCorr <- data$LieuExercice
data[LieuExercice == "Paris - La Défense", LieuExerciceCorr := "Puteaux"]
data[LieuExercice == "Ajaccio - 2A", LieuExerciceCorr := "Ajaccio"]
data[LieuExercice == "Bastia - 2B", LieuExerciceCorr := "Bastia"]
data[LieuExercice == "Paris, Pessac", LieuExerciceCorr := "Paris"]
data[LieuExercice == "Bordeaux, Montpellier", LieuExerciceCorr := "Bordeaux"]
data[LieuExercice == "Paris, Bordeaux", LieuExerciceCorr := "Paris"]
data[LieuExercice == "Île-de-France", LieuExerciceCorr := "Paris"]
data[LieuExercice == "Île-de-France - Auvergne-Rhône-Alpes", LieuExerciceCorr := "Paris"]

data[LieuExercice == "NC", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Normandie", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Grand Est", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Rhône-Alpes", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Bretagne", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Languedoc-Roussillon - Sud-Ouest -", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Sainte-Luce-sur-Loire, Paris", LieuExerciceCorr := nom_departement]
data[LieuExercice == "France", LieuExerciceCorr := nom_departement]
data[LieuExercice == "Champagne-Ardenne - Nord -", LieuExerciceCorr := nom_departement]

data[is.na(LieuExerciceCorr), LieuExerciceCorr := ""]

data[is.na(Département), .(NA_Count = .N), by = .(LieuExercice)][,LieuExercice]


for (i in seq_along(data$LieuExerciceCorr)) {
  ville <- data$LieuExerciceCorr[i]
  cat("\014")  
  cat(i/nrow(data)*100,"% \n")
  if (ville != ""){
    coordinates <- get_lat_long(ville)
    data$lat[i] <- coordinates[1]
    data$lon[i] <- coordinates[2]
    if (is.na(data$lat[i])){
      cat(i, ville, data$lat[i], "\n")
    }
  }
}

# # if the code stop due to my internet connection
# j <- i
# for (i in j:nrow(data)) {
#   ville <- data$LieuExerciceCorr[i]
#   cat("\014")  
#   cat(i/nrow(data)*100,"% \n")
#   if (ville != ""){
#     coordinates <- get_lat_long(ville)
#     data$lat[i] <- coordinates[1]
#     data$lon[i] <- coordinates[2]
#     if (is.na(data$lat[i])){
#       cat(i, ville, data$lat[i], "\n")
#     }
#   }
# }
# 
# 

# Renomage des variables :

job_data <- fread("data/job_data.csv", header = T)
data$old_LieuExercice <- data$LieuExercice
data$LieuExercice <- data$LieuExerciceCorr
data[,LieuExerciceCorr := NULL]
colnames(data)[4] <- "DuréeEmploi"
colnames(data)[15] <- "CompétencesDemandées"
data <- fread("data/data.csv")
# write.csv2(data,"data/data.csv")
