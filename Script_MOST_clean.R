### INITIALISATION ########
rm(list=ls())
setwd("C:/Users/Nomade01/Desktop/M2 ECOEVO/MOST/Projet MOST/data")

# Librairies
library(tidyverse)
library(bestNormalize)
library(RColorBrewer)
library(report)


### CHARGEMENT DES DONNEES ##########################################################################################
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


### MODIFICATION DES DONNEES ##########################################################################################

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
               names_to = "sex", 
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

df_global_long <- pivot_longer(df_global, cols = c(legal_males, adu_fem, juv_fem, juv_males,total_crabs), names_to = "crab_category", values_to = "count")




### ANALYSE ECHANTILLONNAGE ################################################################################################################################# 
# On regarde la tête des données : Nb de crabes par casier en fonction du temps pour chaque catégorie
ggplot(survey_long_category, aes(x = as.factor(year), y = nb_crabs_per_pot, color = crab_category)) +
  geom_boxplot() +
  theme_bw() 

ggplot(survey_long_category, aes(x = year, y = nb_crabs_per_pot, color = crab_category))+
  geom_point()+
  geom_smooth(method = "lm")
# on remarque que bcp de valeurs extrêmes, compliquées à étudier, on n'utilisera pas le nb de crabe par piège

# On regarde la dynamique du nb de crabes
# Nb de crabes moyen par année de chaque catégorie en fonction du temps (corrigé par l'effort de pêche)
ggplot(df_global_long, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)+
  labs(title = "Nombre de crabes échantillonnés par année (corrigé par l'effort d'échantillonnage)", x = "Année", y = "Nombre de crabes échantillonnés (corrigé par l'effort d'échantillonnage)", color = "Classe d'âge et sexe") +
  theme(axis.title.y = element_text(size = 8)) +
  scale_colour_manual(values =  brewer.pal(5, "Set1"), labels = c("Femelles adultes", "Femelles juvéniles", "Mâles juvéniles", "Mâles légaux", "Total")) +
  theme_bw()

# autre moyen de l'observer, choisir une des 2 !!
ggplot(df_global_long, aes(x = year, y = count, color = crab_category, group = crab_category)) +
  geom_point() +
  geom_line() +
  labs(title = paste("Nombre de crabes échantillonnés par année \n (corrigé par l'effort d'échantillonnage)"), x = "Année", y = "Nombre de crabes échantillonnés (corrigé par l'effort d'échantillonnage)", color = "Classe d'âge et sexe") +
  scale_colour_manual(values =  brewer.pal(5, "Set1"), labels = c("Femelles adultes", "Femelles juvéniles", "Mâles juvéniles", "Mâles légaux", "Total")) +
  theme(axis.title.y = element_text(8)) +
  theme_bw()





### ANALYSE TEMPERATURE ################################################################################################################################# 
ggplot(celsius, aes(x = year, y = temp, color = saison))+
  geom_point()+
  labs(x = "Année", y = "Température de l'eau à 100m de profondeur (en °C)", color = "Saison")+
  geom_smooth(method="lm", se=F)

# Etude de l'effet de l'année et de la saison sur la température de l'eau
# avec interaction
ancova_temp_inter <- lm(temp ~ year*saison, data = celsius)
shapiro.test(ancova_temp_inter$residuals)
bartlett.test(ancova_temp_inter$residuals ~ celsius$saison)
par(mfrow=c(2,2))
plot(ancova_temp_inter)
summary(ancova_temp_inter)
anova(ancova_temp_inter) # *** year et saison mais pas l'interaction

# sans interaction
ancova_temp <- lm(temp ~ year + saison, data = celsius)
shapiro.test(ancova_temp$residuals)
bartlett.test(ancova_temp$residuals ~ celsius$saison)
par(mfrow=c(2,2))
plot(ancova_temp)
summary(ancova_temp)
anova(ancova_temp) # *** year et saison

# comparaison des 2 modèles
AIC(ancova_temp_inter, ancova_temp) # + AIC faible, mieux c'est : sans interaction mieux
BIC(ancova_temp_inter, ancova_temp) # + BIC faible, mieux c'est : sans interaction mieux
anova(ancova_temp_inter, ancova_temp) # + RSS faible, mieux c'est : pas de différence significative donc on prend le modèle le plus simple : sans interaction
# meilleure p-value pour le modèle sans interaction
# On utilise donc le modèle sans interaction





### ANALYSE SALINITE #################################################################################################################################
ggplot(salinity, aes(x = year, y = salinity, color = saison))+
  geom_point()+
  labs(x = "Année", y = "Salinité de l'eau à 100m de profondeur (en parties par milliers)", color = "Saison")+
  geom_smooth(method="lm", se=F) # faire une ancova avec la salinité en fonction de l'année et de la saison puis tester l'effet de la salinté sur les variables de survey

# ANCOVA avec interaction
ancova_salinity_inter <- lm(salinity ~ year*saison, data = salinity)
shapiro.test(ancova_salinity_inter$residuals)
par(mfrow=c(2,2))
plot(ancova_salinity_inter) # résidus non normaux donc on va normaliser la salinité pour chaque saison

# Test autre méthode de normalisation
salinity_normalized <- salinity %>%
  group_by(saison) %>%
  mutate(salinity_norm = boxcox(salinity)$x.t)


# ANCOVA avec interaction 
ancova_salinity_inter_normalized <- lm(salinity_norm ~ year*saison, data = salinity_normalized)
shapiro.test(ancova_salinity_inter_normalized$residuals)
bartlett.test(ancova_salinity_inter_normalized$residuals ~ salinity_normalized$saison)
par(mfrow=c(2,2))
plot(ancova_salinity_inter_normalized)
summary(ancova_salinity_inter_normalized)
anova(ancova_salinity_inter_normalized) #  ** year mais saison pas significatif

# ANCOVA sans interaction normalisée
ancova_salinity_normalized <- lm(salinity_norm ~ year + saison , data = salinity_normalized)
shapiro.test(ancova_salinity_normalized$residuals)
bartlett.test(ancova_salinity_normalized$residuals ~ salinity_normalized$saison)
par(mfrow=c(2,2))
plot(ancova_salinity_normalized)
summary(ancova_salinity_normalized)
anova(ancova_salinity_normalized) # ** year mais saison pas significatif

# Test des 2 modèles
AIC(ancova_salinity_normalized, ancova_salinity_inter_normalized) # + AIC faible, mieux c'est : sans interaction mieux
BIC(ancova_salinity_normalized, ancova_salinity_inter_normalized) # + BIC faible, mieux c'est : sans interaction mieux
anova(ancova_salinity_normalized, ancova_salinity_inter_normalized) # + RSS faible, mieux c'est : pas de différence significative donc on prend le modèle le plus simple : sans interaction
# p-value plus faible pour le modèle sans interaction
# on choisit donc le modèle sans interaction




### ANALYSE OEUFS ############################################################################################################
# Sûrement pas utilisé mais peut être utile
# REGRESSION LINEAIRE
df_global_short <- df_global[1:11,]
mod_temp_sal_eggs <- lm(estim_eggs_per_adu_f ~ sal_moy + temp_moy, data = df_global_short) # * quand que salinité sinon rien quand aussi température
par(mfrow=c(2,2))
plot(mod_temp_sal_eggs)
shapiro.test(mod_temp_sal_eggs$residuals)
summary(mod_temp_sal_eggs) # * sal_moy mais temp_moy pas significative




### ANALYSE TAILLE ##################################################################################################
# Graphique avec les tailles moyennes pour chaque année car trop de données sinon
# Longueur moyenne annuelle en fonction de la température moyenne de l'année pour chaque sexe
ggplot(dstns_simplified_long, aes(x = temp_moy, y = length_moy, color = sex))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

# Longueur moyenne annuelle en fonction de la salinité moyenne de l'année pour chaque sexe
ggplot(dstns_simplified_long, aes(x = sal_moy, y = length_moy, color = sex))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

# Longueur en fonction de la température moyenne de l'année, pour chaque sexe
ggplot(dstns_long, aes(x = as.factor(temp_moy), y = length_mm, fill = sex))+
  geom_violin()

# Longueur en fonction de la salinité moyenne de l'année, pour chaque sexe
ggplot(dstns_long, aes(x = as.factor(sal_moy), y = length_mm, fill = sex))+
  geom_violin()

# ANCOVA - 3 FACTEURS
mod_taille_temp_sal <- lm(length_mm ~ temp_moy + sal_moy + sex, data = dstns_long)
par(mfrow=c(2,2))
plot(mod_taille_temp_sal)
summary(mod_taille_temp_sal) # R² faible donc bcp de variance pas expliquée, on peut justifier cela par le fait que les différentes classes d'âges sont fusionnées


# Call:
#   lm(formula = length_mm ~ temp_moy + sal_moy + sex, data = dstns_long)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -98.103 -13.387   0.585  14.824  81.172 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1016.74501    8.35462   121.7   <2e-16 ***
#   temp_moy      -2.90902    0.02857  -101.8   <2e-16 ***
#   sal_moy      -27.67978    0.26201  -105.6   <2e-16 ***
#   sexM          16.44091    0.05267   312.2   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.95 on 712480 degrees of freedom
# (24636 observations effacées parce que manquantes)
# Multiple R-squared:  0.1467,	Adjusted R-squared:  0.1467 
# F-statistic: 4.082e+04 on 3 and 712480 DF,  p-value: < 2.2e-16


# On va tester si température seule a un effet car graphiquement on ne le voit pas, pas à inclure dans le rapport
# REGRESSION LINEAIRE - Longueur en fonction de la température moyenne de l'année (uniquement mâles)
dstns_long_M <- dstns_long |> 
  filter(sex == "M")
mod_taille_temp_M <- lm(length_mm ~ temp_moy, data = dstns_long_M)
par(mfrow=c(2,2))
plot(mod_taille_temp_M)
summary(mod_taille_temp_M)

