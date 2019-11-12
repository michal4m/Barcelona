
options(scipen = 999)

library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

# Barcelona metropolitan area
# https://en.wikipedia.org/wiki/Barcelona_metropolitan_area

# Districts and barrios of Barcelona
# https://en.wikipedia.org/wiki/Districts_of_Barcelona

#### LIST OF HABITATGE DATASETS ####
# name
# 1                 est-mercat-immobiliari-lloguer-mitja-mensual
# 2                                            habitatges-2na-ma
# 3                                       n-edif-hab-n-immo-edif
# 4             est-mercat-immobiliari-lloguer-nombre-contractes
# 5            est-mercat-immobiliari-lloguer-superficie-mitjana
# 6                            est-cadastre-habitatges-propietat
# 7                                      est-cadastre-locals-tip
# 8                            est-cadastre-habitatges-any-const
# 9                                      est-cadastre-locals-sup
# 10                                                   h2mallo-a
# 11                                             h2mave-totalt4b
# 12                          est-mercat-immobiliari-compravenda
# 13                                    est-cadastre-locals-prop
# 14                                 habit-ppal-segons-regim-tin
# 15                          est-cadastre-habitatges-superficie
# 16                                            h2mallo-evolucio
# 17                                             h2mave-anualt1b
# 18               est-mercat-immobiliari-compravenda-preu-total
# 19                                         h2mave-semestralt2b
# 20                                 habit-ppal-segons-any-const
# 21                      est-cadastre-locals-valor-cadastral-us
# 22                                             h2mave-totalt3b
# 23                 est-cadastre-locals-mitjana-valors-unitaris
# 24                    habit-fam-sit-edif-dest-habit-segons-tip
# 25                         habit-ppal-segons-nombre-pers-viuen
# 26                             habit-ppal-segons-instal2-aigua
# 27                                             h2mave-anualt2b
# 28                                       n-edif-hab-tipus-edif
# 29                                 habit-ppal-segons-nom-habit
# 30                        immo-edif-hab-segons-any-construccio
# 31                                  habit-ppal-segons-sup-util
# 32                            n-edif-hab-segons-nombre-plantes
# 33                              n-edif-hab-segons-any-construc
# 34                                    n-edif-hab-segons-n-immo
# 35                        habit-ppal-segons-estat-conserv-edif
# 36                                             h2mave-totalt2b
# 37                                             h2mave-anualt3b
# 38                        habit-ppal-segons-instal3-telf-i-int
# 39                       habit-fam-dest-habit-segons-any-const
# 40                             habit-ppal-segons-instal1-calef
# 41                n-edif-hab-segons-nombre-plantes-sota-rasant
# 42                                         h2mave-semestralt3b
# 43                immo-edif-hab-segons-num-plantes-sota-rasant
# 44                                             h2mave-totalt1b
# 45                        n-edif-hab-segons-estat-cons-edifici
# 46               n-edif-hab-segons-nombre-plantes-sobre-rasant
# 47                    n-edif-hab-segons-n-plantes-sobre-rasant
# 48                                         h2mave-semestralt1b
# 49                      est-cadastre-edificacions-edat-mitjana
# 50                      immo-edif-hab-segons-estat-conservacio
# 51                                             h2mave-totalt5b
# 52                           immo-edif-hab-segons-instalacions
# 53                      est-mercat-immobiliari-compravenda-sup
# 54                     n-edif-hab-segons-n-plantes-sota-rasant
# 55                              n-edif-hab-segons-n-places-gar
# 56                               n-edif-hab-segons-nombre-immo
# 57                          est-cadastre-locals-sup-tipus-prop
# 58                                    n-edif-hab-segons-instal
# 59               immo-edif-hab-segons-num-plantes-sobre-rasant
# 60                        est-cadastre-edificacions-superficie
# 61                             est-cadastre-finques-superficie
# 62                     est-cadastre-edificacions-nombre-locals
# 63                        est-cadastre-habitatges-edat-mitjana
# 64                      est-cadastre-carrecs-valors-cadastrals
# 65                  est-cadastre-habitatges-superficie-mitjana
# 66 est-cadastre-locals-valor-cadastral-tipus-nacionalitat-prop
# 67                         est-cadastre-edificacions-any-const
# 68                             est-cadastre-carrecs-superficie
# 69     est-cadastre-carrecs-valors-cadastrals-tipus-propietari
# 70                        est-cadastre-finques-regim-propietat
# 71                     est-cadastre-habitatges-valor-cadastral
# 72         est-cadastre-habitatges-superficie-tipus-propietari
# 73                                     est-cadastre-carrecs-us
# 74                   est-cadastre-carrecs-valors-cadastrals-us
#### LIST OF HABITATGE DATASETS ####

#### FIRST DATA SET CONCERNING HABITATGE ####

# Rent prices
data <- fread('2018_lloguer_preu_trim.csv')
cols <- colnames(data)

# Average rent per squared meter
data %>% 
  filter(Lloguer_mitja == 'Lloguer mitjà per superfície (Euros/m2 mes)') %>% 
  filter(Trimestre == 4) %>% 
  group_by(Nom_Districte,Nom_Barri) %>% 
  summarise(rent_monthly_per_meter = median(Preu)) %>% 
  arrange(desc(rent_monthly_per_meter)) -> data_rent_monthly_per_meter

# Average rent
data %>% 
  filter(Lloguer_mitja != 'Lloguer mitjà per superfície (Euros/m2 mes)') %>% 
  filter(Trimestre == 4) %>% 
  group_by(Nom_Districte,Nom_Barri) %>% 
  summarise(rent_monthly = median(Preu)) %>% 
  arrange(desc(rent_monthly)) -> data_rent_monthly

data_rent <- left_join(data_rent_monthly_per_meter,data_rent_monthly)

data_rent %>% 
  mutate(meters_average = round(rent_monthly/rent_monthly_per_meter,2)) -> data_rent

# data %>%
#   filter(Lloguer_mitja == 'Lloguer mitjà per superfície (Euros/m2 mes)') %>%
#   filter(Nom_Districte == 'Eixample') %>% view

#### FIRST DATA SET CONCERNING HABITATGE ####

#### SECOND DATA SET CONCERNING HABITATGE ####

# Number of rental agreements made
#est-mercat-immobiliari-lloguer-nombre-contractes

data2 <- fread('2018_lloguer_cont_trim.csv')
cols2 <- colnames(data2)

data2 %>% 
  filter(Trimestre == 4) %>% 
  group_by(Nom_Districte,Nom_Barri) %>% 
  summarise(n = sum(Nombre)) %>% 
  arrange(desc(n)) -> data2_transactions

#### SECOND DATA SET CONCERNING HABITATGE ####

data_rent2 <- left_join(data_rent,data2_transactions)

# How much would a 50 meter flat cost me?
flat_size = 50
eur_pln = 4.3

data_rent2 %>% 
  mutate(estimated_50 = flat_size*rent_monthly_per_meter) %>% 
  mutate(estimated_50_pln = eur_pln * estimated_50 ) %>% 
  mutate(class_eur = cut(estimated_50,breaks = seq(350,900,50))) %>%
  mutate(class_pln = cut(estimated_50_pln,breaks = seq(1500,4000,100),dig.lab = 4)) -> data_rent2

data_rent2 %>% 
  group_by(class_pln) %>% 
  summarise(n_of_districts = n()) %>% 
  arrange(desc(class_pln)) %>% view
  
# TO DO: 
# 1) import map of Barcelona
# 2) plot rent costs 
         
