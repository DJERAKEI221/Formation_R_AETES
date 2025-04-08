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
library(gtsummary)

###### ANALYSE UNIVARIEE

### Charger base

mem4 <- read_dta("./Donnees/mem.dta")





install.packages("skimr")
library(skimr)

skim(mem4)

library(tidyverse)


glimpse(mem4)

### Mettre format factor

library(haven)
mem4 <- mem4 %>%
  mutate(across(where(is.labelled), ~ as_factor(.)))

View(mem4)

### Stat des

mean(mem4$age)

mean(mem4$age, na.rm = TRUE)

sd(mem4$age, na.rm = TRUE)

min(mem4$age, na.rm = TRUE)

max(mem4$age, na.rm = TRUE)

range(mem4$age, na.rm = TRUE) # min et max

diff(range(mem4$age, na.rm = TRUE)) #Étendu 

quantile(mem4$age, na.rm = TRUE, probs = c(0.5,0.75))

summary(mem4$age)


mean(mem4$age[!mem4$age %in% c(2019, 1990, 0)], na.rm = T)


### graphique


plot(density(mem4$age, na.rm = TRUE),
     main = "Age")

plot(ecdf(mem4$age),main = "")

boxplot(mem4$age, 
        main = "",
        ylab = "Age")




Q1 <- quantile(mem4$age, probs = 0.25, na.rm = TRUE)

Q1

Q3 <- quantile(mem4$age, probs = 0.75, na.rm = TRUE)

Q3

### Variable qualitative ou catégorielle

table(mem4$sexe)
table(mem4$lien)

sort(table(mem4$lien))
sort(table(mem4$lien), decreasing = TRUE)

prop.table(table(mem4$lien))


mem4 %>%
  select(sexe, lien) %>%
  tbl_summary(
    label = list(
      sexe ~ "Sexe",
      lien ~ "Lien avec le chef"
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  modify_header(label ~ "**Échantillon**")  # Remplace "Characteristic" par "Échantillon"




# useNA="no" (valeur par défaut), les valeurs manquantes ne sont jamais incluses dans le tri à plat ;
# useNA="ifany" , une colonne NA est ajoutée si des valeurs manquantes sont présentes dans les données ;
# useNA="always" , une colonne NA est toujours ajoutée, même s’il n’y a pas de valeurs


mem4 %>%
  select(sexe,lien) %>%  # ou toute autre variable catégorielle
  tbl_summary(
    missing = "always",   # Affiche toujours une ligne pour les NA
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(sexe = "Sexe", lien = "Lien avec le chef de ménage"))%>%
  modify_header(label ~ "**Échantillon**")  # Remplace "Characteristic" par "Échantillon"




mem4 %>%
  select(sexe, lien) %>%
  tbl_summary(
    missing = "ifany",  # seulement si NA présents
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(sexe = "Sexe", lien = "Lien avec le chef de ménage")
  )%>%
  modify_header(label ~ "**Échantillon**")  # Remplace "Characteristic" par "Échantillon"



library(questionr)


freq(mem4$lien)

freq(mem4$lien, cum = TRUE, total = TRUE, sort = "inc", digits = 2,
     exclude = NA)

freq(mem4$sexe, levels = "labels") #LEVEL POUR AFFICHER LES LABELS

freq(mem4$sexe, cum = TRUE, total = TRUE, sort = "inc", digits = 2,
     exclude = NA)

barplot(sort(table(mem4$sexe),TRUE),col=1:2)

pie(table(mem4$sexe),col=1:2)



dotchart(as.matrix(sort(table(mem4$lien)))[, 1], main = "Lien avec le Chef de ménage")

#### Analyse bivariée

## Deux variables quanti

cor()

cov()



### Deux quali

library(stats)

attach(mem4)


xtabs(~sexe + s00q01)

ltabs(~sexe + s00q01, mem4)

lprop(table(sexe, s00q01), digits = 2)

cprop(table(sexe, s00q01),digits = 2)
cprop(table(sexe, s00q01),digits = 2, percent = T)

cprop(table(sexe, s00q01),digits = 2, percent = F)



### Quali et Quanti


hdv %>% group_by(sexe) %>% summarise(mean(heures.tv,na.rm=T)) %>% ungroup()

tableau<-hdv %>% group_by(sexe) %>% 
  summarise(n(),round(mean(heures.tv,na.rm=T),2),round(sd(heures.tv,na.rm=T),2)) %>% 
  ungroup()

tableau<-as.data.frame(tableau)

colnames(tableau)<-c("Sexe", "Effectif", "Moyenne", "Ecart_type") 

tableau

tableau<-hdv %>% group_by(sexe) %>% 
  summarise(Effectif=n(),
            Moyenne=round(mean(heures.tv,na.rm=T),2),
            Ecart_type=round(sd(heures.tv,na.rm=T),2)) %>% 
  ungroup()

tableau<-as.data.frame(tableau)

tableau_total<-hdv %>% 
  summarise(Effectif=n(),
            Moyenne=round(mean(heures.tv,na.rm=T),2),
            Ecart_type=round(sd(heures.tv,na.rm=T),2)) 

library(openxlsx)
write.xlsx(tableau,"E:/Cours_ENSEA/Introduction_R/TP/stat_des1.xlsx")

tableau1<-hdv %>% group_by(sexe,sport) %>% 
  summarise(Effectif=n(),
            Moyenne=round(mean(heures.tv,na.rm=T),2),
            Ecart_type=round(sd(heures.tv,na.rm=T),2)) %>% 
  ungroup()

tableau1<-as.data.frame(tableau1)

write.xlsx(tableau1,"C:/Users/pc/Desktop/Formation AS/AS2/SEMESTRE 4/Logiciel R/TP_descriptive/Base_Tp_Module2/stat_des2.xlsx")

barplot(tapply(hdv$heures.tv,hdv$sexe,mean,na.rm=T))

barplot(tapply(hdv$heures.tv,hdv$sexe,mean,na.rm=T),
        main="Heure moyenne devant la télé selon le sexe",
        xlab="Sexe",
        ylab="heures")

boxplot(hdv$heures.tv~hdv$sexe, col = grey(0.8),
        main = paste("Nombre d'heures", "devant la télé", sep="\n"),
        ylab = "Heures",
        xlab = "Sexe")

#===========================# 
#         ggplot2           #
#===========================#

rm(list=ls())

### Charger package

library(dplyr)
library(ggplot2)
library(haven)

#=========== Application 1 ============

### charger Base

films<-read_dta("C:/Users/pc/Desktop/Formation AS/AS2/SEMESTRE 4/Logiciel R/TP_descriptive/Base_Tp_Module2/films.dta")

films_reduit <-films %>%
  filter(country %in% c("United States of America", "New Zealand", "United Kingdom", "Spain"))

### Analyse Univariée

ggplot(data = films_reduit, aes(x = runtime)) + geom_histogram()

ggplot(data = films_reduit,
       aes(x = runtime)) +
  geom_histogram(fill = "dodger blue")

ggplot(data = films_reduit, aes(x = runtime, y = ..density..)) +
  geom_histogram(colour = "white") +
  geom_line(stat="density", col = "red", size = 1.2)

ggplot(data = films_reduit, aes(x = runtime, y = ..density..)) +
  geom_histogram(colour = "white") +
  geom_density(col = "red")

### Analyse bivariée

## Nuage de point

ggplot(data = films,
       aes(x = estimated_budget, y = gross_revenue)) + geom_point()

ggplot(data = films, aes(x = estimated_budget, y = gross_revenue)) +
  geom_point(colour = "dodger blue", alpha = .8)

ggplot(data = films_reduit, aes(x = estimated_budget, y = gross_revenue)) +
  geom_point(alpha = .8, aes(colour = estimated_budget))

ggplot(data = films_reduit, aes(x = estimated_budget, y = gross_revenue)) +
  geom_point(alpha = .8, aes(colour = country))

ggplot(data = films_reduit,
       aes(x = estimated_budget, y = gross_revenue, col = country)) +
  geom_line()

### 

ggplot(data = films_reduit,
       aes(x = country, y = runtime, fill = country)) +
  geom_boxplot()

ggplot(data = films_reduit, aes(x = estimated_budget,
                                y = gross_revenue,
                                colour = country,
                                size = runtime)) +
  geom_point()

ggplot(data = films_reduit,
       aes(estimated_budget/1000000,
           gross_revenue/1000000,
           colour = country,
           size = runtime)) +
  geom_point() +
  facet_wrap( ~ country, scales = "fixed")

ggplot(data = films, aes(x = estimated_budget, y = gross_revenue)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.9, se=F, col="red")

ggplot(data = films, aes(x = estimated_budget, y = gross_revenue)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.9)

#============== Application 2 ===============

rp <- read_xlsx("C:/Users/pc/Desktop/Formation AS/AS2/SEMESTRE 4/Logiciel R/TP_descriptive/Base_Tp_Module2/rp2018.xlsx")

rp <- rp %>% 
  filter(departement %in% c("Oise", "Rhône", "Hauts-de-Seine", "Lozère", "Bouches-du-Rhône"))

ggplot(rp,aes(x = cadres)) +
  geom_histogram()

ggplot(data = rp) +
  geom_histogram(aes(x = cadres)) +
  facet_wrap(vars(departement))

ggplot(data = rp) +
  geom_histogram(aes(x = cadres)) +
  facet_grid(rows = vars(departement))

ggplot(rp) +
  geom_bar(aes(x = departement))

ggplot(rp) +
  geom_bar(aes(x = departement)) +
  coord_flip()

ggplot(rp) +
  geom_bar(
    aes(x = departement),
    fill = "darkblue", width = .5
  )

ggplot(rp) +
  geom_bar(
    aes(x = departement, fill = pop_cl),
    position = "dodge"
  )

ggplot(rp) +
  geom_bar(
    aes(x = departement, fill = pop_cl),
    position = "fill"
  )

ggplot(rp) +
  geom_point(aes(x = dipl_sup, y = cadres))

ggplot(rp,aes(x = dipl_sup, y = cadres)) +
  geom_point(col="blue")

ggplot(rp) +
  geom_point(
    aes(x = dipl_sup, y = cadres, size = pop_tot),
    color = "royalblue"
  )

ggplot(rp) +
  geom_boxplot(
    aes(x = departement, y = maison))

ggplot(rp) +
  geom_boxplot(
    aes(x = departement, y = maison),
    fill = "wheat", color = "tomato4"
  )

ggplot(rp) +
  geom_violin(
    aes(x = departement, y = maison),
    bw = 2
  )

ggplot(data = rp) +
  geom_histogram(aes(x = cadres)) +
  facet_wrap(vars(departement))

ggplot(data = rp) +
  geom_histogram(aes(x = cadres)) +
  facet_grid(rows = vars(departement))

ggplot(rp) +
  geom_bar(aes(x = departement))

ggplot(rp) +
  geom_bar(aes(x = departement)) +
  coord_flip()

ggplot(rp) +
  geom_bar(
    aes(x = departement),
    fill = "darkblue", width = .5
  )

ggplot(rp) +
  geom_bar(
    aes(x = departement, fill = pop_cl),
    position = "dodge"
  )

ggplot(rp) +
  geom_bar(
    aes(x = departement, fill = pop_cl),
    position = "fill"
  )

#==================== Application 3

rm(list=ls())

job_indicator <- read_xlsx("C:/Users/pc/Desktop/Formation AS/AS2/SEMESTRE 4/Logiciel R/TP_descriptive/Base_Tp_Module2/job_indicator.xlsx")

job_indicator$date=as.Date(job_indicator$date)

ggplot(job_indicator) +
  geom_line(aes(x = date, y = unemploy))


library(lubridate)

job_indicator <- job_indicator %>%
  mutate(Mois = factor(substr(as.character(date), 6, 7)))


table(job_indicator$Mois)

names(job_indicator)


ggplot(job_indicator) +
  geom_line(aes(x = date, y = unemploy)) +
  facet_wrap(vars(Mois))

ggplot(job_indicator) +
  geom_line(aes(x = date, y = unemploy)) +
  facet_grid(vars(Mois))






