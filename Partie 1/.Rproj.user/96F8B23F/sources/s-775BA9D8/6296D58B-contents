#===========================# 
#  Statistique DESCRIPTIVE  #
#===========================#

rm(list=ls())

library(questionr)  # Pour l’analyse descriptive et les tableaux croisés
library(openxlsx)   # Pour lire/écrire des fichiers Excel
library(readxl)     # Pour lire les fichiers Excel
library(ggplot2)    # Pour la visualisation
library(dplyr)      # Pour la manipulation de données
library(haven)
library(ggplot2)


###### ANALYSE UNIVARIEE

### Charger base

house4 <- read_dta("./Donnees/round04_households.dta")

str(house4)

house4 <- house4 %>%
  mutate(across(where(is.labelled), ~ as_factor(.)))

View(house4)













house4 <- read_dta("./Donnees/round04_households.dta")
income <- read_dta("./Donnees/round04_income.dta")
price <- read_dta("./Donnees/round04__price.dta")
foods <- read_dta("./Donnees/round04__foods.dta")
shocks <- read_dta("./Donnees/round04_shocks.dta")





mem <- left_join(mem4, house4, by="hhid")

revenu <- left_join(income, house4, by="hhid")

prix <- left_join(price, house4,by="hhid")

choc <- left_join(shocks, house4, by="hhid")


library(janitor)

# Nettoyage des noms de colonnes après fusion
mem <- mem %>% clean_names()
revenu <- revenu %>% clean_names()
prix <- prix %>% clean_names()
choc <- choc %>% clean_names()

# Ensuite, réessaie l'export
write_dta(mem, "./Donnees/mem.dta")
write_dta(revenu, "./Donnees/revenu.dta")
write_dta(prix, "./Donnees/prix.dta")
write_dta(choc, "./Donnees/choc.dta")
