### INITIALISATION ########
rm(list=ls())
setwd("C:/Users/Nomade01/Desktop/M2 ECOEVO/MOST/Projet MOST/data")

### Librairies ###
library(tidyverse)
library(bestNormalize)
library(lme4)
library(report)

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
  ) |> 
  mutate(sex_ratio = (juv_males + legal_males)/(juv_fem + adu_fem))

survey_long_category <- survey |> 
  select(year, legal_males_per_pot, adu_fem_per_pot, juv_fem_per_pot, juv_males_per_pot) |> 
  pivot_longer(cols = c(legal_males_per_pot, adu_fem_per_pot, juv_fem_per_pot, juv_males_per_pot), names_to = "crab_category", values_to = "nb_crabs_per_pot")

survey_long_category$crab_category <- str_remove(survey_long_category$crab_category, "_per_pot")

# Celsius
celsius$year <- paste0("19", celsius$year) # ajout de 19 devant les nb pour avoir une année

celsius <- celsius |>
  mutate(
    saison = case_when(
      month %in% c(12, 1, 2) ~ "hiver",
      month %in% c(3, 4, 5) ~ "printemps",
      month %in% c(6, 7, 8) ~ "été",
      month %in% c(9, 10, 11) ~ "automne",)) |>
  select(-month)
celsius$saison <- as.factor(celsius$saison)
celsius$year <- as.numeric(celsius$year)

celsius_simplified <- celsius |> 
  group_by(year, saison) |>
  summarize(temp_moy = mean(temp)) |> 
  pivot_wider(names_from = saison, values_from = temp_moy) |>
  rename(temp_moy_hiver = hiver,
         temp_moy_automne = automne,
         temp_moy_été = été,
         temp_moy_printemps = printemps)


celsius_simplified$year<- as.numeric(celsius_simplified$year)
df_global <- left_join(survey_simplified, celsius_simplified, by = "year")

celsius_more_simplified <- celsius |> 
  group_by(year) |>
  summarise(temp_moy = mean(temp, na.rm = TRUE)) # calcul de la température moyenne pour chaque année

celsius_more_simplified$year <- as.numeric(celsius_more_simplified$year)
df_global <- left_join(df_global, celsius_more_simplified, by = "year")


# Salinity
salinity$year <- paste0("19", salinity$year)
salinity$year<- as.numeric(salinity$year)
salinity$month<- as.factor(salinity$month)
salinity$salinity<- as.numeric(salinity$salinity)

salinity <- salinity |>
  mutate(saison = case_when(
    month %in% c(12, 1, 2) ~ "hiver",
    month %in% c(3, 4, 5) ~ "printemps",
    month %in% c(6, 7, 8) ~ "été",
    month %in% c(9, 10, 11) ~ "automne")) |> 
  select(-month)

salinity_simplified <- salinity |> 
  group_by(year, saison) |>
  summarize(sal_moy = mean(salinity)) |> 
  pivot_wider(names_from = saison, values_from = sal_moy) |>
  rename(sal_moy_hiver = hiver,
         sal_moy_automne = automne,
         sal_moy_été = été,
         sal_moy_printemps = printemps)
salinity$saison<- as.factor(salinity$saison)

df_global <- left_join(df_global, salinity_simplified, by = "year")

salinity_more_simplified <- salinity |> 
  group_by(year) |>
  summarise(sal_moy = mean(salinity, na.rm = TRUE)) # calcul de la salinité moyenne pour chaque année

salinity_more_simplified$year <- as.numeric(salinity_more_simplified$year)
df_global <- left_join(df_global, salinity_more_simplified, by = "year")


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
fleet_simplified <- fleet[,c(1,2,3)]
fleet_simplified$year <- fleet_simplified$year+1 # ajout d'une année puis on modifiera le nom de la colonne pour donner le nb de crabes pêchés l'année précédente
fleet_simplified <- rename(fleet_simplified, crabs_caught_last_year = crabs_caught)
fleet_simplified$year <- as.numeric(fleet_simplified$year)

df_global <- left_join(df_global, fleet_simplified, by = "year")
df_global <- left_join(df_global, fleet[,c(1,6)], by = "year") # ajout du prix à df_global

# Catch

diff_catch_fleet <- data.frame(year = fleet$year, delta_count = (fleet$crabs_caught - catch_simplified$total_count), delta_kg = fleet$total_weight_caught - catch_simplified$total_kg) # on compare fleet et catch pour être sûr que l'on a pas d'erreur
# on remarque que pour 1974 on a 2 valeurs pour le districts 1 mais différentes, pour corriger cela on peut faire une moyenne des 2 mais cela reste imparfait
catch[15,3:4] <- colSums(catch[c(15,16),3:4])/2
catch[39,3:4] <- colSums(catch[c(39,40),3:4])/2
catch[63,3:4] <- colSums(catch[c(63,64),3:4])/2
catch[87,3:4] <- colSums(catch[c(87,88),3:4])/2
catch <- catch[-c(16,40,64,88),]
catch$district <- as.factor(catch$district)

catch_simplified <- group_by(catch, year) |>
  summarize(total_count = sum(total_count), total_kg = sum(total_kg))

# Eggs
eggs$year <- paste0("19", eggs$year)
eggs$year <- as.numeric(eggs$year)
df_global <- left_join(df_global, eggs, by = "year")



# Dstns
dstns$year <- paste0("19", dstns$year)
dstns$year <- as.numeric(dstns$year)

dstns_long <- dstns |>
  mutate(count_F = count_juv_f + count_adu_f,
         count_M = count_males) |> 
  select(-count_juv_f, -count_adu_f, -count_males) |> 
  pivot_longer(
    cols = starts_with("count"),
    names_to = "sex",
    values_to = "count"
  ) %>%
  mutate(sex = ifelse(sex == "count_F", "F", "M")) %>%
  uncount(count)

dstns_long <- left_join(dstns_long, celsius_more_simplified, by="year")
dstns_long <- left_join(dstns_long, salinity_more_simplified, by="year")

dstns_sum <- dstns |>
  group_by(year) |>
  mutate(count_f = count_juv_f + count_adu_f)

dstns_simplified <- dstns_sum|> 
  summarize(
    length_moy_juv_F = sum(length_mm * count_juv_f) / sum(count_juv_f),
    length_moy_adu_F = sum(length_mm * count_adu_f) / sum(count_adu_f),
    length_moy_M = sum(length_mm * count_males) / sum(count_males),
    length_moy_F = sum(length_mm * count_f) / sum(count_f))

dstns_simplified_long <- dstns_simplified |> 
  select(-length_moy_adu_F, - length_moy_juv_F) |> 
  pivot_longer(cols = -year, 
               names_to = "category", 
               names_prefix = "length_moy_",
               values_to = "length_moy")

dstns_simplified_long <- left_join(dstns_simplified_long, celsius_more_simplified, by="year")
dstns_simplified_long <- left_join(dstns_simplified_long, salinity_more_simplified, by="year")

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
    count_fullness90_100 = sum(fullness90_100)) 

df_global <- left_join(df_global, fullness_simplified, by = "year")


### ANALYSE DES DONNEES ###################

## SURVEY
# Test de la méthode de correction de l'effort de pêche
ggplot(df_global, aes(x = legal_males, y = legal_males_pp))+ # on regarde si on a une différence entre le nb de crabes corrigés et le nb de crabes moyen par piège
  geom_point()+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Nombre de mâles pouvant être légalement pêchés", y = "Nombre de mâles pouvant être légalement pêchés moyen par piège")
test <- lm(legal_males~legal_males_pp, data = df_global)
summary(test) # on remarque que c'est très fortement correlé donc pas de grosse différence si on choisit l'un ou l'autre

# Nb de crabes par casier en fonction du temps pour chaque catégorie
ggplot(survey_long_category, aes(x = as.factor(year), y = nb_crabs_per_pot, color = crab_category)) +
  geom_boxplot() +
  theme_bw()

ggplot(survey_long_category, aes(x = year, y = nb_crabs_per_pot, color = crab_category))+
  geom_point()+
  geom_smooth(method = "lm")

nb_crabs_per_pot_norm <- bestNormalize(survey_long_category$nb_crabs_per_pot)

mod_crab_year <- lm(nb_crabs_per_pot_norm$x.t ~ year + crab_category, data = survey_long_category) # ne fonctionne pas !! on doit utiliser la moyenne par année même si pas ouf
par(mfrow=c(2,2))
shapiro.test(mod_crab_year$residuals)
plot(mod_crab_year)
summary(mod_crab_year)



# Nb de crabes moyen par année de chaque catégorie en fonction du temps (corrigé par l'effort de pêche)
df_global_long1 <- pivot_longer(df_global, cols = c(legal_males, adu_fem, juv_fem, juv_males,total_crabs), names_to = "crab_category", values_to = "count") # on passe au format long, préférable pour ggplot
ggplot(df_global_long1, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title = "Dynamique du nombre de crabes de chaque classe d'âge", x = "Année", y = "Nombre de crabes corrigé par l'effort d'échantillonnage", color = "Classe d'âge") +
  theme(axis.title.y = element_text(size = 8)) +
  theme_bw()



df_global_long2 <- pivot_longer(df_global, cols = c(legal_males_pp, adu_fem_pp, juv_fem_pp, juv_males_pp, total_crabs_pp), names_to = "crab_category", values_to = "count")
ggplot(df_global_long2, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_line() +
  labs(title = "Dynamique du nombre moyen de crabes de chaque classe d'âge par piège", x = "Année", y = "Nombre de crabes moyen par piège", color = "Classe d'âge") +
  theme(axis.title.y = element_text(size = 8)) +
  theme_bw()

ggplot(df_global, aes(x = year, y = total_crabs))+
  labs(title = "Nombre de crabes corrigé par l'effort d'échantillonnage par année", x = "Années", y = "Nombre de crabes corrigé par l'effort d'échantillonnage")+
  theme(axis.title.y = element_text(size = 8)) +
  geom_col()


## CELSIUS

ggplot(celsius, aes(x = year, y = temp, color = saison))+
  geom_point()+
  labs(x = "Année", y = "Température de l'eau à 100m de profondeur (en °C)", color = "Saison")+
  geom_smooth(method="lm", se=F)

# avec interaction
ancova_temp_inter <- lm(temp ~ year*saison, data = celsius)
shapiro.test(ancova_temp_inter$residuals)
par(mfrow=c(2,2))
plot(ancova_temp_inter)
summary(ancova_temp_inter)
anova(ancova_temp_inter) # *** year et saison

# sans interaction
ancova_temp <- lm(temp ~ year + saison, data = celsius)
shapiro.test(ancova_temp$residuals)
par(mfrow=c(2,2))
plot(ancova_temp)
summary(ancova_temp)
anova(ancova_temp)

# comparaison des 2 modèles
AIC(ancova_temp_inter, ancova_temp) # + AIC faible, mieux c'est : sans interaction mieux
BIC(ancova_temp_inter, ancova_temp) # + BIC faible, mieux c'est : sans interaction mieux
anova(ancova_temp_inter, ancova_temp) # + RSS faible, mieux c'est : avec interaction mieux
# meilleure p-value pour le modèle sans interaction
# On utilise donc le modèle sans interaction

## SALINITE
ggplot(salinity, aes(x = year, y = salinity, color = saison))+
  geom_point()+
  labs(x = "Année", y = "Salinité de l'eau à 100m de profondeur (en parties par milliers)", color = "Saison")+
  geom_smooth(method="lm", se=F) # faire une ancova avec la salinité en fonction de l'année et de la saison puis tester l'effet de la salinté sur les variables de survey

# avec interaction
ancova_salinity_inter <- lm(salinity ~ year*saison, data = salinity)
shapiro.test(ancova_salinity_inter$residuals)
par(mfrow=c(2,2))
plot(ancova_salinity_inter) # résidus non normaux donc on va normaliser la salinité pour chaque saison

# avec interaction normalisé
salinity_normalized <- salinity %>%
  group_by(saison) %>%
  mutate(salinity_normalized = bestNormalize(salinity, out_of_sample = FALSE)$x.t)

ancova_salinity_inter_normalized <- lm(salinity_normalized ~ year*saison, data = salinity_normalized)
shapiro.test(ancova_salinity_inter_normalized$residuals)
par(mfrow=c(2,2))
plot(ancova_salinity_inter_normalized)
summary(ancova_salinity_inter_normalized)
anova(ancova_salinity_inter_normalized) # year ** et salinité pas significatif

# sans interaction normalisé
ancova_salinity_normalized <- lm(salinity_normalized ~ year + saison , data = salinity_normalized)
shapiro.test(ancova_salinity_normalized$residuals)
par(mfrow=c(2,2))
plot(ancova_salinity_normalized)
summary(ancova_salinity_normalized)
anova(ancova_salinity_normalized)

# Test des 2 modèles
AIC(ancova_salinity_normalized, ancova_salinity_inter_normalized) # + AIC faible, mieux c'est : sans interaction mieux
BIC(ancova_salinity_normalized, ancova_salinity_inter_normalized) # + BIC faible, mieux c'est : sans interaction mieux
anova(ancova_salinity_normalized, ancova_salinity_inter_normalized) # RSS + faible avec interaction, donc mieux
# p-value plus faible pour le modèle sans interaction
# on choisit donc le modèle sans interaction


## SALINITY & TEMP sur SURVEY, pas sûr de l'utiliser car compliqué
# avec la moyenne de crabes par année
mod_temp_sal_crabs <- lm(legal_males ~ temp_moy + sal_moy, data = df_global) 
plot(mod_temp_sal_crabs)
shapiro.test(mod_temp_sal_crabs$residuals)
summary(mod_temp_sal_crabs)

# avec toutes les valeurs pour chaque année
mod_temp_sal_survey <- lm(legal_males_per_pot ~ temp_moy_printemps + temp_moy_automne + temp_moy_été + temp_moy_hiver, data = survey)
plot(mod_temp_sal_survey)
shapiro.test(mod_temp_sal_survey$residuals)
summary(mod_temp_sal_survey)

# et sur les oeufs
df_global_short <- df_global[1:11,]
mod_temp_sal_eggs <- lm(estim_eggs_per_adu_f ~ sal_moy + temp_moy_été, data = df_global_short) # * quand que salinité sinon rien quand aussi température
par(mfrow=c(2,2))
plot(mod_temp_sal_eggs)
shapiro.test(mod_temp_sal_eggs$residuals)
summary(mod_temp_sal_eggs) # temp_été * ou sal_moy


## SEX-RATIO
ggplot(df_global, aes(x = year, y = sex_ratio))+ # sex-ratio dimiunue au cours du temps = de moins en moins de mâles par rapport aux femelles
  geom_point()+
  geom_smooth(method = "lm")

mod_sex_ratio_temp_sal <- lm(sex_ratio ~ temp_moy + sal_moy, data = df_global)
par(mfrow=c(2,2))
plot(mod_sex_ratio_temp_sal)
shapiro.test(mod_sex_ratio_temp_sal$residuals)

# normalisation des données
sex_ratio_norm <- bestNormalize(df_global$sex_ratio, out_of_sample = FALSE)
mod_sex_ratio_temp_sal <- lm(sex_ratio_norm$x.t ~ temp_moy + sal_moy data = df_global)
par(mfrow=c(2,2))
plot(mod_sex_ratio_temp_sal)
shapiro.test(mod_sex_ratio_temp_sal$residuals)
summary(mod_sex_ratio_temp_sal) # sex-ratio ne dépend pas de la salinité, ni de la température


## TAILLE DES CRABES EN FONCTION DE SAL ET TEMP
mod_taille_temp_sal <- lm(length_mm ~ temp_moy + sal_moy + sex, data = dstns_long)
par(mfrow=c(2,2))
plot(mod_taille_temp_sal)
shapiro.test(mod_taille_temp_sal$residuals)
summary(mod_taille_temp_sal)


## FLEET 
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

ggplot(fleet, aes(x = total_pot_lifts, y = total_weight_caught))+ # test si plus on pêche plus on attrape de crabes 
  geom_point()

summary(lm(total_weight_caught ~ total_pot_lifts, data = fleet))

ggplot(fleet) + # étude de l'effort de pêche
  aes(x = year, y = total_pot_lifts) +
  geom_point(shape = "circle", size = 2.55, colour = "#112446") +
  geom_smooth(method = "lm") + 
  theme_minimal() 
mod_lifts <- lm(total_pot_lifts~year, data = fleet)
summary(mod_lifts)



## CATCH
ggplot(catch, aes(x = year, y = total_kg, color = district))+
  geom_point()

## DSTNS
# trouver un moyen d'avoir un df avec l'année, la longueur et le sexe pour fait un boxplot pour voir si la distribution change (taille des boites)

ggplot(dstns, aes(x = as.factor(year), y = count_males))+
  geom_boxplot()

ggplot(dstns_simplified_long, aes(x = year, y = length_moy, color = category))+ # Longueur moyenne augmente de manière significative avec le temps
  geom_point()+
  geom_smooth(method = "lm", se = F)
summary(lm(length_moy_M ~ year, data = dstns_simplified)) # *
summary(lm(length_moy_F ~ year, data = dstns_simplified)) # **

ggplot(df_global, aes(x = sal_moy, y = length_moy_F))+
  geom_point()+
  geom_smooth(method = "lm", se = F)
summary(lm(length_moy_F ~ sal_moy, data = df_global)) # taille des F ne dépend pas de la salinité


## EGGS
ggplot(df_global, aes(x=year, y=estim_eggs_per_adu_f)) +
  labs(x = "Années", y = "Nombre d'oeufs estimés par femelle adulte") +
  geom_point(color = "#FF8B8B") +
  geom_smooth(method = "lm", color = "#FF8B8B", se = F)

ggplot(df_global, aes(x = legal_males, y = estim_eggs_per_adu_f))+
  geom_point(color = "#FF8B8B")+
  geom_smooth(method = "lm", se = F, color = "#FF8B8B")+
  labs(x = "Longueur moyenne des femelles", y = "Nombre d'oeufs moyen par femelle")
summary(lm(estim_eggs_per_adu_f ~ length_moy_F, data = df_global))

ggplot(df_global, aes(x = sal_moy, y = estim_eggs_per_adu_f))+
  geom_point(color = "#FF8B8B")+
  geom_smooth(method = "lm", se = F, color = "#FF8B8B")
summary(lm(estim_eggs_per_adu_f ~ sal_moy, data = df_global)) # *

