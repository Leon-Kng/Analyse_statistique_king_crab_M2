rm(list=ls())
setwd("C:/Users/WIN7/Desktop/Projet MOST/Data")

library(dplyr) # pour modifier les données
library(ggplot2) # graphiques
library(plotly)
library(gridExtra)
library(gganimate)

##### OUVERTURE ET MODIFICATION DES DONNEES #########################################

### Chargement des données ###
survey <- read.table("survey", stringsAsFactors=T)
survey <-rename(survey,year = V1, fishing_district = V2,Station_ID=V3,pots_fished=V4,latitude_halfway_pot=V5,longitude_halfway_pot=V6,pre_recruit_4=V7,pre_recruit_3=V8,pre_recruit_2=V9,pre_recruit_1=V10,recruit_males=V11,post_recruit=V12,juv_fem=V13,adu_fem=V14)

kodiak <- read.table("kodiak",stringsAsFactors=T)
kodiak <-rename(kodiak, latitude= V1, longitude = V2)

### Modification des données
survey$year <- paste0("19", survey$year) # ajout de 19 devant les nb pour avoir une année
survey$year <- as.factor(survey$year)
survey$legal_males <- survey$post_recruit + survey$recruit_males
survey$Total_crabs <- rowSums(survey[, 7:14], na.rm = TRUE) # calcul du nb total de crabes attrapés
survey_count <- survey %>%
  group_by(year) %>%
  summarise(Nb_legal_males = sum(legal_males))
survey$fishing_district <- as.factor(survey$fishing_district)




##### METHODE PLOTLY ######################################################################################################

# Définir les limites x et y pour conserver les mêmes échelles sur chaque graphique
x_limits <- range(-kodiak$longitude)
y_limits <- range(kodiak$latitude)

# Créer un graphique de base avec Plotly
base_plot <- plot_ly() %>%
  add_trace(
    data = kodiak,
    x = ~-longitude,
    y = ~latitude,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'black'),
    showlegend = FALSE
  ) %>%
  layout(
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    title = "Carte interactive des îles de Kodiak"
  )

# Créer une liste pour stocker les données de chaque année
data_by_year <- lapply(unique(survey$year), function(yr) {
  survey_year <- subset(survey, year == yr)
  list(
    x = -survey_year$longitude_halfway_pot,
    y = survey_year$latitude_halfway_pot,
    size = survey_year$Total_crabs,
    color = survey_year$fishing_district,
    text = paste("Année: ", yr, "<br>Total Crabs: ", survey_year$Total_crabs)
  )
})

# Ajouter les données de chaque année au graphique de base pour chaque frame
for (i in seq_along(data_by_year)) {
  base_plot <- base_plot %>%
    add_trace(
      data = data_by_year[[i]],
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      size = ~size,
      color = ~color,
      text = ~text,
      frame = unique(survey$year)[i]
    )
}

base_plot %>%
  animation_opts(frame = 500, redraw = TRUE, transition = 0) %>%
  animation_slider(
    currentvalue = list(prefix = "Année: "),
    steps = lapply(
      seq_along(data_by_year),
      function(i) list(
        label = unique(survey$year)[i],
        method = "animate",
        args = list(list("frame", list(duration = 0)), list("transition", list(duration = 0)))
      )
    )
  ) %>%
  layout(
    hovermode = "closest", # Afficher les informations au survol du point le plus proche
    hoverinfo = "text"     # Afficher les informations définies dans 'text' au survol
  )

# ajouter ")" ici si problème

##### METHODE MOSAIQUE ######################################################


plots <- list() # Liste pour stocker les graphiques
num_plots <- 0 # Compteur pour le nombre de graphiques

for (année in years) {
  survey_year <- filter(survey, year == année) # Filtrez les données pour l'année en cour
  
  # Création du graphique
  p <- ggplot() +
    geom_polygon(data = kodiak, aes(x = -longitude, y = latitude)) +
    labs(x = "Longitude", y = "Latitude", title = paste("Carte des îles de Kodiak - Année", année),
         size= "Nombre total de crabes \n échantillonnés à cet \n emplacement", color="Fishing district") +
    geom_point(data = survey_year, aes(x=-longitude_halfway_pot, y=latitude_halfway_pot, size=Total_crabs, color=fishing_district))
  
  
  plots[[année]] <- p # Ajout du graphique à la liste
  num_plots <- num_plots + 1
}


ncol <- ceiling(sqrt(num_plots)) # Calcul du nb de colonnes et de lignes pour la mosaïque
nrow <- ceiling(num_plots / ncol)
grid.arrange(grobs = plots, ncol = ncol, nrow = nrow) # Combine tous les graphiques en un seul plot



##### TOUTES LES ANNEES #############################################

ggplot() +
  geom_polygon(data = kodiak, aes(x = -longitude, y = latitude)) +
  coord_fixed(ratio = 1/1.8) +  # Aspect ratio 1:1.8
  labs(x = "Longitude", y = "Latitude", title = "Carte de l'île Kodiak") +
  geom_point(data = survey, aes(x=-longitude_halfway_pot, y=latitude_halfway_pot, size=Total_crabs, color=fishing_district))

ggplot() + # sans correction du ratio et avec les fishing district
  geom_polygon(data = kodiak, aes(x = -longitude, y = latitude)) +
  labs(x = "Longitude", y = "Latitude", title = "Carte des îles de Kodiak", size= "Nombre total de crabes \n échantillonnés à cet \n emplacement", color="Fishing district")+
  geom_point(data = survey, aes(x=-longitude_halfway_pot, y=latitude_halfway_pot, size=Total_crabs, color=fishing_district))

##### PLOT DE CHAQUE ANNEE ########################
plots <- list() # Liste pour stocker les graphiques

for (année in years) {
  survey_year <- filter(survey, year == année) # Filtrez les données pour l'année en cour
  
  # Création du graphique
  p <- ggplot() +
    geom_polygon(data = kodiak, aes(x = -longitude, y = latitude)) +
    labs(x = "Longitude", y = "Latitude", title = paste("Carte des îles de Kodiak - Année", année),
         size= "Nombre total de crabes \n échantillonnés à cet \n emplacement", color="Fishing district") +
    geom_point(data = survey_year, aes(x=-longitude_halfway_pot, y=latitude_halfway_pot, size=Total_crabs, color=fishing_district))
  
  plots[[année]] <- p # Ajout du graphique à la liste
}

# Afficher ou enregistrer chaque graphique séparément
for (année in names(plots)) {
  print(plots[[année]]) # Afficher dans la console R
  # OU enregistrer dans un fichier
  # ggsave(paste("plot_", année, ".png", sep = ""), plot = plots[[année]], width = 10, height = 8)
}




