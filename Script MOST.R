rm(list=ls())
setwd("C:/Users/Nomade01/Desktop/M2 ECOEVO/MOST/Projet MOST/data")

### Librairies ###
library(dplyr) # pour modifier les données
library(ggplot2) # graphiques
library(tidyr)
library(esquisse)
library(bestNormalize)
#library(reshape)

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
survey$legal_males <- survey$post_recruit + survey$recruit_males
survey$Total_crabs <- rowSums(survey[, 7:14], na.rm = TRUE) # calcul du nb total de crabes attrapés
df_global <- survey %>%  # on met dans un df global le comptage des crabes par années, plus exploitable pour la suite
  group_by(year) %>%
  summarise(legal_males = sum(legal_males), 
            adu_fem = sum(adu_fem),
            juv_fem = sum(juv_fem),
            pre_recruit_1 = sum(pre_recruit_1),
            pre_recruit_2 = sum(pre_recruit_2),
            pre_recruit_3 = sum(pre_recruit_3),
            pre_recruit_4 = sum(pre_recruit_4),
            Total_crabs = sum(Total_crabs)) 
df_global$juv_male <- rowSums(df_global[,5:8])
survey$fishing_district <- as.factor(survey$fishing_district)

# Celsius
celsius$year <- paste0("19", celsius$year) # ajout de 19 devant les nb pour avoir une année
celsius$month <- ifelse(nchar(celsius$month) == 1, paste0("0", celsius$month), celsius$month) # ajout de 0 devant les mois à 1 chiffre
date_celsius <- unite(celsius, col = "date", month, year, sep = "/")
celsius$date <- as.Date(paste0("01/", date_celsius$date), format = "%d/%m/%Y")

celsius$year<- as.numeric(celsius$year)
celsius$month<- as.factor(celsius$month)

celsius_simplified <- celsius %>% 
  group_by(year) %>%
  summarise(temp_moy = mean(temp, na.rm = TRUE)) # calcul de la température moyenne pour chaque année


df_global <- left_join(df_global, celsius_simplified, by = "year") # ajout de la température moyenne pour chaque année à df_global

# Salinity
salinity$year <- paste0("19", salinity$year)
salinity$year<- as.numeric(salinity$year)
salinity$month<- as.factor(salinity$month)
salinity$salinity<- as.numeric(salinity$salinity)

for (n in c(33,36,38,46)){ # on enlève les données en double 
  salinity[n,3]<-(salinity[n,3]+salinity[n+1,3])/2 # et on les remplace par une moyenne 
  salinity<-salinity[-(n+1),]
}

salinity_simplified <- salinity %>% 
  group_by(year) %>%
  summarise(sal_moy = mean(salinity, na.rm = TRUE)) # calcul de la salinité moyenne pour chaque année
df_global <- left_join(df_global, salinity_simplified, by = "year") # ajout de la salinité moyenne pour chaque année à df_global


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
df_global <- left_join(df_global, fleet_simplified, by = "year") # ajout du nombre de crabes pêchés l'année précédente

diff_catch_fleet <- data.frame(year = fleet$year, delta_count = (fleet$crabs_caught - catch_simplified$total_count), delta_kg = fleet$total_weight_caught - catch_simplified$total_kg) # on compare fleet et catch pour être sûr que l'on a pas d'erreur
# on remarque que pour 1974 on a 2 valeurs pour le districts 1 mais différentes, pour corriger cela on peut faire une moyenne des 2
catch[15,3] <- (catch[15,3] + catch[16,3])/2
catch[15,4] <- (catch[15,4] + catch[16,4])/2
catch <- catch[-16,]


# Eggs
eggs$year <- paste0("19", eggs$year)
eggs$year <- as.numeric(eggs$year)
df_global <- left_join(df_global, eggs, by = "year")



# Dstns
dstns$year <- paste0("19", dstns$year)
dstns$year <- as.numeric(dstns$year)

dstns_simplified <- dstns %>%
  group_by(year) %>%
  summarize(length_moy_juv = sum(length_mm * count_juv_f) / sum(count_juv_f),
    length_moy_adu_F = sum(length_mm * count_adu_f) / sum(count_adu_f),
    length_moy_adu_M = sum(length_mm * count_males) / sum(count_males))

df_global <- left_join(df_global, dstns_simplified, by = "year") 

# Fullness




### ANALYSE DES DONNEES ###################

## Df_global
df_global_long <- pivot_longer(df_global, cols = c(legal_males, adu_fem, juv_fem, juv_male), names_to = "crab_category", values_to = "count") # on passe au format long, préférable pour ggplot
ggplot(df_global_long, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_bw()


## SURVEY
ggplot(df_global, aes(x=year, y=Total_crabs))+
  geom_col()


## FLEET & CATCH
ggplot(fleet, aes(x = crabs_caught, y = price_pound)) + # $ per pound
  geom_point()

ggplot(fleet, aes(x = total_weight_caught, y = price_pound)) + # kg of crabs caught
  geom_point()

ggplot(fleet, aes(x = year, y = crabs_caught)) +
  geom_col()+
  labs(title = "fleet")

ggplot(catch_simplified, aes(x = year, y = total_count)) +
  geom_col() +
  labs(title = "catch_simplified")

## EGGS
ggplot(eggs)+
  aes(x=year, y=estim_eggs_per_adu_f)+
  geom_col()+
  labs(x = "Années", y = "Nombre d'oeufs estimés par femelle adulte")+
  geom_col(fill = "#FF8B8B") 



## CELSIUS
modele <- lm(temp ~ date, data = celsius)
par(mfrow=c(2,2))
plot(modele)
summary(modele)

ggplot(celsius) +
  aes(x = date, y = temp) +
  geom_line(colour = "#494AFF") +
  theme_minimal() +
  geom_smooth(method="lm", color = "red")


## Test si température et salinité de l'eau a un effet sur l'effectif de crabe


mod_eau <- lm(legal_males~temp_moy+sal_moy, data=df_global)
par(mfrow=c(2,2))
plot(mod_eau)
summary(mod_eau)
par(mfrow=c(1,1))

ggplot(df_global) +
  aes(x = temp_moy, y = legal_males) +
  geom_point()

ggplot(df_global) +
  aes(x = sal_moy, y = legal_males) +
  geom_point()


# Test de l'effet de m'activité de pêche de l'année précédente
mod_peche_nb_crabs <- lm(legal_males~crabs_caught_last_year, data = df_global)
par(mfrow=c(2,2))
plot(mod_peche_nb_crabs)
summary(mod_peche_nb_crabs)

mod_peche_eggs <- lm(estim_eggs_per_adu_f~crabs_caught_last_year, data = df_global)
par(mfrow=c(2,2))
plot(mod_peche_eggs)
summary(mod_peche_eggs)

ggplot(df_global) +
  aes(x = crabs_caught_last_year, y = legal_males) +
  geom_point()


# Test de la taille moyenne des crabes en fonction de ...

mod_length_X <- lm(length_moy_adu_M~sal_moy, data = df_global)
par(mfrow=c(2,2))
plot(mod_length_X)
summary(mod_length_X)

ggplot(df_global) +
  aes(x = temp_moy, y = length_moy_adu_M) +
  geom_point()



# Test de tous les facteurs quantitatifs sur toutes les variabels dépendantes (régressions multiples)
df_global_short <- df_global[1:11,] # car on a des NA pour 1984 à 1986 pour la salinité et la quantité de crabes pêchés donc si on n'utilise pas ces facteurs alors utiliser df_global plutôt

mod_legal_males <- lm(legal_males ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
mod_legal_males <- lm(legal_males ~ year + temp_moy + crabs_caught_last_year, data = df_global_short) # on retire l'un après l'autre chaque facteur 
mod_legal_males <- lm(legal_males ~ year + crabs_caught_last_year, data = df_global_short)
mod_legal_males <- lm(legal_males ~ year, data = df_global)
summary(mod_legal_males) # année très significatives 

mod_legal_adu_F <- lm(adu_fem ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_legal_adu_F)
mod_juv_fem <- lm(juv_fem ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_juv_fem)
mod_pre_1 <- lm(pre_recruit_1 ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_pre_1)
mod_pre_2 <- lm(pre_recruit_2 ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short) # salinité très significative !! temp et année presque significative
summary(mod_pre_2)
mod_pre_3 <- lm(pre_recruit_3 ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short) # salinité presque significative
summary(mod_pre_3)
mod_pre_4 <- lm(pre_recruit_4 ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_pre_4)
mod_tot_crabs <- lm(Total_crabs ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_tot_crabs)

mod_eggs <- lm(estim_eggs_per_adu_f ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_eggs)

mod_len_juv_F <- lm(length_moy_juv_F ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_len_juv_F)
mod_len_adu_F <- lm(length_moy_adu_F ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_len_adu_F)
mod_len_adu_M <- lm(length_moy_adu_M ~ year + temp_moy + sal_moy + crabs_caught_last_year, data = df_global_short)
summary(mod_len_adu_M)

