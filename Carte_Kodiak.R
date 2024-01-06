### INITIALISATION ########
rm(list=ls())
setwd("C:/Users/Nomade01/Desktop/M2 ECOEVO/MOST/Projet MOST/data")

### Librairies ###
library(tidyverse)
library(bestNormalize)

### CHARGEMENT DES DONNEES ##################
survey <- read.table("survey", stringsAsFactors=T)
survey <-rename(survey,year = V1, fishing_district = V2,Station_ID=V3,pots_fished=V4,latitude_halfway_pot=V5,longitude_halfway_pot=V6,pre_recruit_4=V7,pre_recruit_3=V8,pre_recruit_2=V9,pre_recruit_1=V10,recruit_males=V11,post_recruit=V12,juv_fem=V13,adu_fem=V14)

kodiak <- read.table("kodiak",stringsAsFactors=T)
kodiak <-rename(kodiak, latitude= V1, longitude = V2)

dstns <- read.table("dstns",stringsAsFactors=T)
dstns <-rename(dstns,year = V1, length_mm = V2 , count_juv_f=V3,count_adu_f=V4 ,count_males=V5)

fleet <- read.table("fleet",stringsAsFactors=T)
fleet <-rename(fleet,year= V1, nbr_vessels = V2, crabs_caught=V3,total_weight_caught=V4,total_pot_lifts=V5,price_pound=V6)

catch <- read.table("catch", stringsAsFactors=T)
catch <-rename(catch,year = V1,  district= V2, total_count=V3, total_kg=V4)

eggs <- read.table("eggs",stringsAsFactors=T)
eggs <-rename(eggs, year= V1, estim_eggs_per_adu_f=V2)

salinity <- read.table("salinity",stringsAsFactors=T)
salinity<-rename(salinity,year = V1, month = V2,salinity=V3)

celsius <- read.table("celsius",stringsAsFactors=T)
celsius <-rename(celsius,year = V1, month = V2,temp=V3)

fullness <- read.table("fullness",stringsAsFactors=T)
fullness <-rename(fullness, year= V1, size_mm= V2,fullness0=V3,fullness1_29=V4,fullness30_59=V5,fullness60_89=V6,fullness90_100=V7)


### MODIFICATION DES DONNEES ##################

# Survey
survey$year <- paste0("19", survey$year) # ajout de 19 devant les nb pour avoir une année
survey$year <- as.numeric(survey$year)
survey$fishing_district <- as.factor(survey$fishing_district)
survey <- survey |> # on divise les données par le nb de casiers utilisés afin de tenir compte de l'effort d'échantillonnage
  mutate(year = year,
         fishing_district = fishing_district,
         Station_ID = Station_ID,
         pots_fished = pots_fished,
         latitude_halfway_pot = latitude_halfway_pot,
         longitude_halfway_pot = longitude_halfway_pot,
         pre_recruit_4_per_pot = pre_recruit_4/pots_fished,
         pre_recruit_3_per_pot = pre_recruit_3/pots_fished,
         pre_recruit_2_per_pot = pre_recruit_2/pots_fished,
         pre_recruit_1_per_pot = pre_recruit_1/pots_fished,
         recruit_males_per_pot = recruit_males/pots_fished,
         post_recruit_per_pot = post_recruit/pots_fished,
         juv_fem_per_pot = juv_fem/pots_fished,
         adu_fem_per_pot = adu_fem/pots_fished) |>  
  mutate(legal_males_per_pot = post_recruit_per_pot + recruit_males_per_pot,
         juv_males_per_pot = pre_recruit_1_per_pot + pre_recruit_2_per_pot + pre_recruit_3_per_pot + pre_recruit_4_per_pot,
         total_crabs_per_pot = juv_fem_per_pot + juv_males_per_pot + legal_males_per_pot + adu_fem_per_pot)

survey_simplified <- survey |> 
  group_by(year) |>
  summarise(legal_males_per_pot = mean(legal_males_per_pot), 
            adu_fem_per_pot = mean(adu_fem_per_pot),
            juv_fem_per_pot = mean(juv_fem_per_pot),
            juv_males_per_pot = mean(juv_males_per_pot),
            pre_recruit_1_per_pot = mean(pre_recruit_1_per_pot),
            pre_recruit_2_per_pot = mean(pre_recruit_2_per_pot),
            pre_recruit_3_per_pot = mean(pre_recruit_3_per_pot),
            pre_recruit_4_per_pot = mean(pre_recruit_4_per_pot),
            total_crabs_per_pot = mean(total_crabs_per_pot)) 


# Celsius
celsius$year <- paste0("19", celsius$year) # ajout de 19 devant les nb pour avoir une année
#celsius$date <-  paste0(celsius$month, "/", celsius$year)

celsius_simplified <- celsius |>
  mutate(
    saison = case_when(
      month %in% c(12, 1, 2) ~ "hiver",
      month %in% c(3, 4, 5) ~ "printemps",
      month %in% c(6, 7, 8) ~ "été",
      month %in% c(9, 10, 11) ~ "automne",
    )
  ) |>
  group_by(year, saison) |>
  summarize(
    temp_moy = mean(temp)
  )

celsius_simplified$year<- as.numeric(celsius_simplified$year)
celsius_simplified$saison <- as.factor(celsius_simplified$saison)

celsius_more_simplified <- celsius |> 
  group_by(year) |>
  summarise(temp_moy = mean(temp, na.rm = TRUE)) # calcul de la température moyenne pour chaque année

celsius_more_simplified$year <- as.numeric(celsius_more_simplified$year)


# Salinity
salinity$year <- paste0("19", salinity$year)
salinity$year<- as.numeric(salinity$year)
salinity$month<- as.factor(salinity$month)
salinity$salinity<- as.numeric(salinity$salinity)

for (n in c(33,36,38,46)){ # on enlève les données en double 
  salinity[n,3]<-(salinity[n,3]+salinity[n+1,3])/2 # et on les remplace par une moyenne 
  salinity<-salinity[-(n+1),]
}

salinity_simplified <- salinity |>
  mutate(
    saison = case_when(
      month %in% c(12, 1, 2) ~ "hiver",
      month %in% c(3, 4, 5) ~ "printemps",
      month %in% c(6, 7, 8) ~ "été",
      month %in% c(9, 10, 11) ~ "automne",
    )
  ) |>
  group_by(year, saison) |>
  summarize(
    sal_moy = mean(salinity) # calcul de la salinité moyenne pour chaque mois de chaque année
  )


salinity_more_simplified <- salinity |> 
  group_by(year) |>
  summarise(sal_moy = mean(salinity, na.rm = TRUE)) # calcul de la salinité moyenne pour chaque année



# Catch
catch$year <- paste0("19", catch$year)
catch$year <- as.numeric(catch$year)
catch_simplified <- group_by(catch, year) |>
  summarize(total_count = sum(total_count), total_kg = sum(total_kg))

# Fleet 
fleet$year <- paste0("19", fleet$year)
fleet$year <- as.numeric(fleet$year)
fleet$prev_year <- fleet$year - 1
fleet_simplified <- fleet[,c(1,3)]
fleet_simplified$year <- fleet_simplified$year+1 # ajout d'une année puis on modifiera le nom de la colonne pour donner le nb de crabes pêchés l'année précédente
fleet_simplified <- rename(fleet_simplified, crabs_caught_last_year = crabs_caught)
fleet_simplified$year <- as.numeric(fleet_simplified$year)


# Catch
catch$year <- paste0("19", catch$year)
catch$year <- as.numeric(catch$year)

diff_catch_fleet <- data.frame(year = fleet$year, delta_count = (fleet$crabs_caught - catch_simplified$total_count), delta_kg = fleet$total_weight_caught - catch_simplified$total_kg) # on compare fleet et catch pour être sûr que l'on a pas d'erreur
# on remarque que pour 1974 on a 2 valeurs pour le districts 1 mais différentes, pour corriger cela on peut faire une moyenne des 2
catch[15,3:4] <- colSums(catch[c(15,16),3:4])/2
catch[39,3:4] <- colSums(catch[c(39,40),3:4])/2
catch[63,3:4] <- colSums(catch[c(63,64),3:4])/2
catch[87,3:4] <- colSums(catch[c(87,88),3:4])/2
catch <- catch[-c(16,40,64,88),]

catch_simplified <- group_by(catch, year) |>
  summarize(total_count = sum(total_count), total_kg = sum(total_kg))

# Eggs
eggs$year <- paste0("19", eggs$year)
eggs$year <- as.numeric(eggs$year)
survey_eggs <- left_join(survey_simplified, eggs, by = "year")



# Dstns
dstns$year <- paste0("19", dstns$year)
dstns$year <- as.numeric(dstns$year)

dstns_simplified <- dstns |>
  group_by(year) |>
  summarize(
    length_moy_juv = sum(length_mm * count_juv_f) / sum(count_juv_f),
    length_moy_adu_F = sum(length_mm * count_adu_f) / sum(count_adu_f),
    length_moy_adu_M = sum(length_mm * count_males) / sum(count_males)
    )



# Fullness
fullness_simplified <- fullness |>
  group_by(year) |>
  summarize(
    size_moy_fullness0 = sum(size_mm*fullness0)/sum(fullness0),
    size_moy_fullness1_29 = sum(size_mm*fullness1_29)/sum(fullness1_29),
    size_moy_fullness30_59 = sum(size_mm*fullness30_59)/sum(fullness30_59),
    size_moy_fullness60_89 = sum(size_mm*fullness60_89)/sum(fullness60_89),
    size_moy_fullness90_100 = sum(size_mm*fullness90_100)/sum(fullness90_100),
    count_fullness0 = sum(fullness0),
    count_fullness1_29 = sum(fullness1_29),
    count_fullness30_59 = sum(fullness30_59),
    count_fullness60_89 = sum(fullness60_89),
    count_fullness90_100 = sum(fullness90_100)
  )


### ANALYSE DES DONNEES ###################

## SURVEY SIMPLIFIED
survey_simplified_long <- pivot_longer(survey_simplified, cols = c(legal_males_per_pot, adu_fem_per_pot, juv_fem_per_pot, juv_males_per_pot), names_to = "crab_category", values_to = "count") # on passe au format long, préférable pour ggplot
ggplot(survey_simplified_long, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_smooth(method = "lm", se =F) + 
  theme_bw()


## SURVEY
ggplot(survey_simplified, aes(x=year, y=total_crabs))+
  geom_col()


## DSTNS 
survey_dstns <- left_join(survey_simplified, dstns_simplified, by = "year") 

## FLEET & CATCH
ggplot(fleet, aes(x = crabs_caught, y = price_pound)) + # $ per pound
  geom_point() +
  geom_smooth()

ggplot(fleet, aes(x = total_weight_caught, y = price_pound)) + # kg of crabs caught
  geom_point() +
  geom_smooth()

ggplot(fleet, aes(x = year, y = crabs_caught)) +
  geom_col()+
  labs(title = "fleet")

ggplot(catch_simplified, aes(x = year, y = total_count)) +
  geom_col() +
  labs(title = "catch_simplified")

ggplot(fleet, aes(x = year, y = total_weight_caught)) +
  geom_col()+
  labs(title = "fleet")

ggplot(catch_simplified, aes(x = year, y = total_kg)) +
  geom_col() +
  labs(title = "catch_simplified")

ggplot(fleet) + 
  aes(x = year, y = nbr_vessels) +
  geom_point(shape = "circle", size = 2.55, colour = "#112446") +
  geom_smooth(method = "lm") +
  theme_minimal()
mod_vessels <- lm(nbr_vessels~year, data = fleet)
summary(mod_vessels) 


ggplot(fleet) + # étude de l'effort de pêche
  aes(x = year, y = total_pot_lifts) +
  geom_point(shape = "circle", size = 2.55, colour = "#112446") +
  geom_smooth(method = "lm") + 
  theme_minimal() 
mod_lifts <- lm(total_pot_lifts~year, data = fleet)
summary(mod_lifts)


## EGGS
ggplot(eggs)+
  aes(x=year, y=estim_eggs_per_adu_f) +
  labs(x = "Années", y = "Nombre d'oeufs estimés par femelle adulte") +
  geom_line() +
  geom_point(fill = "#FF8B8B") 



## CELSIUS
ggplot(celsius_simplified, aes(x= year, y = temp_moy, color = saison)) +
  geom_point(aes(color = saison)) +
  geom_line() +
  geom_smooth(method = "lm", se = F)

modele <- lm(temp_moy ~ year, data = celsius_more_simplified)
par(mfrow=c(2,2))
plot(modele)
summary(modele)

ggplot(celsius_more_simplified) +
  aes(x = year, y = temp_moy) +
  geom_point(colour = "#494AFF") +
  theme_minimal() +
  geom_smooth(method="lm", color = "red")


## Test si température et salinité de l'eau a un effet sur l'effectif de crabe
survey_temp_sal <- left_join(survey_simplified, celsius_more_simplified, by = "year") # ajout de la température moyenne pour chaque année
survey_temp_sal <- left_join(survey_temp_sal, salinity_more_simplified, by = "year") # ajout de la salinité moyenne pour chaque année
survey_temp_sal <- survey_temp_sal[1:11,] # car on a des NA pour 1984 à 1986 pour la salinité et la quantité de crabes pêchés


mod_eau <- lm(legal_males~temp_moy+sal_moy, data=survey_temp_sal)
par(mfrow=c(2,2))
plot(mod_eau)
summary(mod_eau)
par(mfrow=c(1,1))

ggplot(survey_temp_sal) +
  aes(x = temp_moy, y = legal_males) +
  geom_point()

ggplot(survey_temp_sal) +
  aes(x = sal_moy, y = legal_males) +
  geom_point()


# Test de l'effet de m'activité de pêche de l'année précédente
survey_fleet <- left_join(survey_simplified, fleet_simplified, by = "year") # ajout du nombre de crabes pêchés l'année précédente
survey_fleet <- left_join(survey_fleet, eggs, by = "year") # ajout du nombre d'oeufs par femelle moyen 

ggplot(survey_fleet) +
  aes(x = crabs_caught_last_year, y = legal_males) +
  geom_point() +
  geom_smooth(method = "lm")

survey_fleet_short <- survey_fleet[1:11,]
mod_peche_nb_crabs <- lm(legal_males~crabs_caught_last_year, data = survey_fleet_short)
par(mfrow=c(2,2))
plot(mod_peche_nb_crabs)
summary(mod_peche_nb_crabs)


mod_peche_eggs <- lm(estim_eggs_per_adu_f~crabs_caught_last_year, data = survey_fleet)
par(mfrow=c(2,2))
plot(mod_peche_eggs)
summary(mod_peche_eggs)





