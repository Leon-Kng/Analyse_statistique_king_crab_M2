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
  summarise(legal_males = sum(legal_males_per_pot), 
            adu_fem = sum(adu_fem_per_pot),
            juv_fem = sum(juv_fem_per_pot),
            juv_males = sum(juv_males_per_pot),
            pre_recruit_1 = sum(pre_recruit_1_per_pot),
            pre_recruit_2 = sum(pre_recruit_2_per_pot),
            pre_recruit_3 = sum(pre_recruit_3_per_pot),
            pre_recruit_4 = sum(pre_recruit_4_per_pot),
            total_crabs = sum(total_crabs_per_pot),
            
            legal_males_pp = mean(legal_males_per_pot), 
            adu_fem_pp = mean(adu_fem_per_pot),
            juv_fem_pp = mean(juv_fem_per_pot),
            juv_males_pp = mean(juv_males_per_pot),
            pre_recruit_1_pp = mean(pre_recruit_1_per_pot),
            pre_recruit_2_pp = mean(pre_recruit_2_per_pot),
            pre_recruit_3_pp = mean(pre_recruit_3_per_pot),
            pre_recruit_4_pp = mean(pre_recruit_4_per_pot),
            total_crabs_pp = mean(total_crabs_per_pot)
            ) 


# Celsius
celsius$year <- paste0("19", celsius$year) # ajout de 19 devant les nb pour avoir une année

celsius_simplified <- celsius |>
  mutate(
    saison = case_when(
      month %in% c(12, 1, 2) ~ "hiver",
      month %in% c(3, 4, 5) ~ "printemps",
      month %in% c(6, 7, 8) ~ "été",
      month %in% c(9, 10, 11) ~ "automne",)) |>
  group_by(year, saison) |>
  summarize(temp_moy = mean(temp)) |> 
  pivot_wider(names_from = saison, values_from = temp_moy) |>
  rename(temp_moy_hiver = hiver,
         temp_moy_automne = automne,
         temp_moy_été = été,
         temp_moy_printemps = printemps)
  

celsius_simplified$year<- as.numeric(celsius_simplified$year)
df_global <- left_join(survey_simplified, celsius_simplified, by = "year")

celsius_more_simplified <- celsius |> # pas sûr de l'utiliser
  group_by(year) |>
  summarise(temp_moy = mean(temp, na.rm = TRUE)) # calcul de la température moyenne pour chaque année

celsius_more_simplified$year <- as.numeric(celsius_more_simplified$year)


# Salinity
salinity$year <- paste0("19", salinity$year)
salinity$year<- as.numeric(salinity$year)
salinity$month<- as.factor(salinity$month)
salinity$salinity<- as.numeric(salinity$salinity)

salinity_simplified <- salinity |>
  mutate(
    saison = case_when(
      month %in% c(12, 1, 2) ~ "hiver",
      month %in% c(3, 4, 5) ~ "printemps",
      month %in% c(6, 7, 8) ~ "été",
      month %in% c(9, 10, 11) ~ "automne",)) |>
  group_by(year, saison) |>
  summarize(sal_moy = mean(salinity)) |> 
  pivot_wider(names_from = saison, values_from = sal_moy) |>
  rename(sal_moy_hiver = hiver,
         sal_moy_automne = automne,
         sal_moy_été = été,
         sal_moy_printemps = printemps)

df_global <- left_join(df_global, salinity_simplified, by = "year")

salinity_more_simplified <- salinity |> 
  group_by(year) |>
  summarise(sal_moy = mean(salinity, na.rm = TRUE)) # calcul de la salinité moyenne pour chaque année



# Catch
catch$year <- paste0("19", catch$year)
catch$year <- as.numeric(catch$year)
catch_simplified <- catch |> 
  group_by(year) |>
  summarize(total_count = sum(total_count), 
            total_kg = sum(total_kg))


# Fleet 
fleet$year <- paste0("19", fleet$year)
fleet$year <- as.numeric(fleet$year)
fleet$prev_year <- fleet$year - 1
fleet_simplified <- fleet[,c(1,3)]
fleet_simplified$year <- fleet_simplified$year+1 # ajout d'une année puis on modifiera le nom de la colonne pour donner le nb de crabes pêchés l'année précédente
fleet_simplified <- rename(fleet_simplified, crabs_caught_last_year = crabs_caught)
fleet_simplified$year <- as.numeric(fleet_simplified$year)

df_global <- left_join(df_global, fleet_simplified, by = "year")

# Catch

diff_catch_fleet <- data.frame(year = fleet$year, delta_count = (fleet$crabs_caught - catch_simplified$total_count), delta_kg = fleet$total_weight_caught - catch_simplified$total_kg) # on compare fleet et catch pour être sûr que l'on a pas d'erreur
# on remarque que pour 1974 on a 2 valeurs pour le districts 1 mais différentes, pour corriger cela on peut faire une moyenne des 2 mais cela reste imparfait
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
df_global <- left_join(df_global, eggs, by = "year")



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

df_global <- left_join(df_global, dstns_simplified, by = "year")

# Fullness
fullness$year <- paste0("19", fullness$year)
fullness$year <- as.numeric(fullness$year)
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

df_global <- left_join(df_global, fullness_simplified, by = "year")


### ANALYSE DES DONNEES ###################

## SURVEY
ggplot(df_global, aes(x = legal_males, y = legal_males_pp))+ # on regarde si on a une différence entre le nb de crabes corrigés et le nb de crabes moyen par piège
  geom_point()+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Nombre de mâles pouvant être légalement pêchés", y = "Nombre de mâles pouvant être légalement pêchés moyen par piège")
test <- lm(legal_males~legal_males_pp, data = df_global)
summary(test) # on remarque que c'est très fortement correlé donc pas de grosse différence si on choisit l'un ou l'autre

df_global_long1 <- pivot_longer(df_global, cols = c(legal_males, adu_fem, juv_fem, juv_males), names_to = "crab_category", values_to = "count") # on passe au format long, préférable pour ggplot
ggplot(df_global_long1, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_smooth(method = "lm", se =F) + 
  labs(title = "Dynamique du nombre de crabes de chaque classe d'âge", x = "Année", y = "Nombre de crabes corrigé par l'effort d'échantillonnage", color = "Classe d'âge") +
  theme(axis.title.y = element_text(size = 8)) +
  theme_bw()

df_global_long2 <- pivot_longer(df_global, cols = c(legal_males_pp, adu_fem_pp, juv_fem_pp, juv_males_pp), names_to = "crab_category", values_to = "count")
ggplot(df_global_long2, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_smooth(method = "lm", se =F) + 
  labs(title = "Dynamique du nombre moyen de crabes de chaque classe d'âge par piège", x = "Année", y = "Nombre de crabes moyen par piège", color = "Classe d'âge") +
  theme(axis.title.y = element_text(size = 8)) +
theme_bw()

ggplot(df_global, aes(x = year, y = total_crabs))+
  labs(title = "Nombre de crabes corrigé par l'effort d'échantillonnage par année", x = "Années", y = "Nombre de crabes corrigé par l'effort d'échantillonnage")+
  theme(axis.title.y = element_text(size = 8)) +
  geom_col()


## DSTNS 


## FLEET & CATCH
ggplot(fleet, aes(x = crabs_caught, y = price_pound)) + # pour étudier l'évolution du prix (en $ per pound) en fonction du nb de crabes pêchés
  geom_point() +
  geom_smooth()

ggplot(fleet, aes(x = total_weight_caught, y = price_pound)) + # total_weight_caught en kg of crabs caught
  geom_point() +
  geom_smooth()

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
ggplot(eggs) +
  aes(x=year, y=estim_eggs_per_adu_f) +
  labs(x = "Années", y = "Nombre d'oeufs estimés par femelle adulte") +
  geom_point(color = "#FF8B8B") +
  geom_smooth(color = "#FF8B8B", se = F) +
  geom_smooth(method = "lm", color = "deepskyblue", se = F)



## CELSIUS
printemps <- ggplot(celsius_simplified, aes(x = year, y = temp_moy_printemps)) + geom_point(color = "forestgreen") + geom_smooth(method = "lm", color = "forestgreen")
été <- ggplot(celsius_simplified, aes(x = year, y = temp_moy_été)) + geom_point(color = "darkorchid") + geom_smooth(method = "lm", color = "darkorchid")
automne <- ggplot(celsius_simplified, aes(x = year, y = temp_moy_automne)) + geom_point(color = "darkorange") + geom_smooth(method = "lm", color = "darkorange")
hiver <- ggplot(celsius_simplified, aes(x = year, y = temp_moy_hiver)) + geom_point(color = "darkslategray3") + geom_smooth(method = "lm", color = "darkslategray3")
grid.arrange(printemps, été, automne, hiver, nrow = 2, ncol = 2)

mod_printemps <- lm(temp_moy_printemps ~ year, data = celsius_simplified) # température moyenne pour chaque saison
mod_été <- lm(temp_moy_été ~ year, data = celsius_simplified)
mod_automne <- lm(temp_moy_automne ~ year, data = celsius_simplified)
mod_hiver <- lm(temp_moy_hiver ~ year, data = celsius_simplified)
par(mfrow=c(2,2))
plot(mod_printemps)
plot(mod_été)
plot(mod_automne)
plot(mod_hiver)
summary(mod_printemps) # **
summary(mod_été) # **
summary(mod_automne) # pas significatif
summary(mod_hiver) # pas significatif

mod_temp_year <- lm(temp_moy ~ year, data = celsius_more_simplified) # température moyenne par année
par(mfrow=c(2,2))
plot(mod_temp_year)
summary(mod_temp_year)

ggplot(celsius_more_simplified) +
  aes(x = year, y = temp_moy) +
  geom_point(colour = "#494AFF") +
  theme_minimal() +
  geom_smooth(method="lm", color = "red")



## Régressions linéaire multiples
df_global_short <- df_global[1:11,] # car on a des NA pour 1984 à 1986 pour la salinité et la quantité de crabes pêchés


