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

ggplot(mem4) +
  geom_bar(aes(x = s05q04))

ggplot(mem4) +
  geom_bar(aes(x = s05q04)) +
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






