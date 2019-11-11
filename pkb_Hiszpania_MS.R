
#### USTAWIANIE SCIEZKI I POBIERANIE PAKIETOW ####
setwd('C:/Users/user/Desktop/Ekonometria przestrzenna')

library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)

library(spdep)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(classInt)

library(geospacom)

library(cmprsk)
library(eurostat)
library(ggplot2)
#### USTAWIANIE SCIEZKI I POBIERANIE PAKIETOW ####

#### WCZYTANIE GOTOWYCH ZBIOROW ####
dane = read.csv2("C:/Users/user/Desktop/Ekonometria przestrzenna/ALL3.csv")
load(file="C:/Users/user/Desktop/Ekonometria przestrzenna/mapa2.RData")
#### WCZYTANIE GOTOWYCH ZBIOROW ####

#### DODANIE ETYKIET DLA REGIONOW ####
dat1 <- get_eurostat(id="nama_10r_3gdp",time_format = "num",
                     stringsAsFactors = FALSE)
dat1 %>% filter(unit == "EUR_HAB" &
                  time == '2014' &
                  substr(geo,1,2) == "ES" & 
                  nchar(geo) == 4) %>% 
  label_eurostat() %>% 
  dplyr::select(geo) ->dat1
colnames(dat1) = "geo_name"
dane = cbind(dat1,dane)

#### DODANIE ETYKIET DLA REGIONOW ####

#### NORMALIZACJA POD KATEM WAG PRZESTRZENNYCH ####
dane$population_norm = as.numeric(dane$population)/max(as.numeric(dane$population))
dane$area_norm = as.numeric(dane$area)/max(as.numeric(dane$area))
dane$equator_dist_norm = as.numeric(dane$latitude)/max(as.numeric(dane$latitude))
#### NORMALIZACJA POD KATEM WAG PRZESTRZENNYCH ####

#### DODATKOWA ZMIENNA ####
# ES30 - Madryt
dane$stolica = ifelse(dane$geo == 'ES30',1,0)
#### DODATKOWA ZMIENNA ####

#### PRZYGOTOWANIE MAPY ####
mapa2 <- spTransform(mapa2, "+proj=longlat")
mapa2@data$country <- substr(as.character(mapa2@data$NUTS_ID), 1, 2) 
mapa2 <- mapa2[mapa2@data$country == "ES", ]
#plot(mapa2)

spatial_data <- merge(x = mapa2, y = dane, 
                      by.x = "NUTS_ID", by.y = "geo")

#par(mfrow=c(1,1))
# WYKRES 1
colors <- brewer.pal(9, "BuGn")
#spatial_data$X2014 <- as.numeric(spatial_data$gdp_eur_hab)
brks <- classIntervals(spatial_data$gdp_eur_hab, n = 9, style = "quantile")
brks <- brks$brks
windows(10, 7)
plot(spatial_data, col = colors[findInterval(spatial_data$gdp_eur_hab, brks,all.inside = TRUE)], axes = F)
title(paste ("PKB w euro na mieszkañca (NUTS 3)"))
legend(x = "right", legend = leglabs(round(brks, 1), under = "poni¿ej", over = "powy¿ej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

# WYKRES 2
spatial_data <- spatial_data[nchar(as.character(spatial_data@data$NUTS_ID)) == 4,]
brks <- classIntervals(spatial_data$gdp_eur_hab, n = 9, style = "quantile")
brks <- brks$brks
windows(10, 7)
plot(spatial_data, col = colors[findInterval(spatial_data$gdp_eur_hab, brks,all.inside = TRUE)], axes = F)
title(paste ("PKB w euro na mieszkañca (NUTS 2)"))
legend(x = "right", legend = leglabs(round(brks, 1), under = "poni¿ej", over = "powy¿ej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)


sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
N <- nrow(spatial_data)
centroids <- coordinates(spatial_data)
plot(spatial_data)
points(centroids, pch = 16, col = sgh_green)

#### PRZYGOTOWANIE MAPY ####

#### TWORZENIE MACIERZY S¥SIEDZTWA ####

# 1) NORMALIZACJA NAJWIEKSZA WARTOSCIA WLASNA
cont1 <- poly2nb(spatial_data, queen = T,row.names = spatial_data@data$NUTS_ID)
#cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1 ,zero.policy = TRUE)
#attr(W1_list,"region.id")
W1 <- listw2mat(W1_list)
m_eigen <- max(as.numeric(unlist(eigen(W1))))
W1 <- W1/m_eigen

plot.nb(cont1, centroids, col = sgh_green, pch = 16)

# 2) OPARTA NA ODWROCONYCH KWADRATACH ODLEGLOSCI (do 200 km)
distance <- DistanceMatrix(spatial_data, "NUTS_ID", unit = 1000)
gamma <- 2
W3 <- ifelse(distance < 200, 1 / (distance ^ gamma), 0)
#W3 <- ifelse(W3<=0.0001 | is.na(W3) ,0.0001,W3)
diag(W3) <- 0
#W3 <- W3 / as.matrix(rowSums(W3)) %*% View(matrix(1, nrow = 1, ncol = N))
#W3_list <- mat2listw(W3, style="W")
W3_list <- mat2listw(W3, style="M")

# 3) OPARTA NA EUKLIDESOWEJ ODLEGLOSCI POMIEDZY TRZEMA WYBRANYMI ZMIENNYMI
# W TYM WYPADKU SA TO LICZBA MIESZKANCOW, POWIERZCHNIA I ODLEGLOSC OD ROWNIKA

W6 <- matrix(0, nrow = N, ncol = N)
for(ii in 1:N) {
  for(jj in 1:N) {
    W6[ii, jj] <- sqrt(sum((spatial_data@data$population_norm[ii] - spatial_data@data$population_norm[jj])^2,
                           (spatial_data@data$area_norm[ii] - spatial_data@data$area_norm[jj])^2,
                           (spatial_data@data$equator_dist_norm[ii] - spatial_data@data$equator_dist_norm[jj])^2))
  }
}
W6 <- W6 / as.matrix(rowSums(W6)) %*% matrix(1, nrow = 1, ncol = N)
rm(ii, jj)
W6_list <- mat2listw(W6, style="W")

#### TWORZENIE MACIERZY S¥SIEDZTWA ####

#### OCENA EFEKTOW PRZESTRZENNYCH ####

# MODEL LINIOWY SZACOWANY KMNK

#model.liniowy <- lm(gdp_eur_hab ~ disposable_income ,data=spatial_data)
model.liniowy <- lm(disposable_income ~ stolica ,data=spatial_data)
#model.liniowy <- lm(gdp_eur_hab ~ longitude + latitude ,data=spatial_data)
#model.liniowy <- lm(disposable_income ~ gdp_eur_hab ,data=spatial_data)
#model.liniowy <- lm(property_income ~ gdp_eur_hab + disposable_income ,data=spatial_data)
#model.liniowy <- lm(trade_mark_apps ~ gdp_eur_hab ,data=spatial_data)
summary(model.liniowy)
res = model.liniowy$residuals

# GRAFICZNA OCENA ZALEZNOSCI

#res <- model.liniowy$residuals
colors <- brewer.pal(9, "BuGn") #set breaks for the 9 colors 
brks <- classIntervals(res, n=9, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res, brks,all.inside = TRUE)], axes = F)
title(paste ("Reszty z modelu liniowego"))
legend(x = "right", y = NULL, legend = leglabs(round(brks), under = "poniÅ¼ej", over = "powyÅ¼ej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)


# GLOBALNY TEST MORANA
moran(spatial_data$gdp_eur_hab, W1_list, length(W1_list$neighbours), length(W1_list$neighbours), zero.policy = T) #Tylko wartoÅ›Ä‡ I oraz kurtoza reszt
moran.test(spatial_data$gdp_eur_hab, W6_list,zero.policy = T,alternative = "less") #Statystyka testowa wraz z p-value
moran.plot(res , W6_list, 
           ylab="Opoznienie przestrzenne zmiennej gdp_eur_hab", 
           xlab="Zmienna eur_hab",pch = 20, main = "Wykres Morana (W6)" , col = sgh_green)



# LOKALNY TEST MORANA
localmoran(spatial_data$gdp_eur_hab, W1_list, p.adjust.method = "bonferroni",zero.policy = T)

# TEST GEARYEGO
geary.test(spatial_data$gdp_eur_hab, W6_list, zero.policy = T,alternative = "less")

# TEST LICZBY POLACZEN
joincount.test(as.factor(res > -5), listw = W1_list, zero.policy = T)

#### OCENA EFEKTOW PRZESTRZENNYCH ####

#### MODELE PRZESTRZENNE ####

#### MODEL CZYSTEJ AUTOREGRESJI PRZESTRZENNEJ ####
# OCENA WYNIKOW I POROWNANIE Z WYNIKAMI POWYZSZYCH TESTOW
model.SAR.points <- spautolm(spatial_data@data$gdp_eur_hab ~ 1, listw = W1_list,zero.policy = T)
summary(model.SAR.points) # male p-value; proces autoregresji przestrzennej zachodzi
res.points <- model.SAR.points$fit$residuals

colors <- brewer.pal(9, "BuGn") #set breaks for the 9 colors 
brks <- classIntervals(res.points, n=9, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res.points, brks,all.inside = TRUE)], axes = F)
title(paste ("Reszty z modelu czystej autoregesji"))
legend(x = "right", y = NULL, legend = leglabs(round(brks), under = "poni¿ej", over = "powy¿ej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)
moran.test(res.points, W1_list,zero.policy = T)
#### MODEL CZYSTEJ AUTOREGRESJI PRZESTRZENNEJ ####

#### WSTRZ¥S W KATALONII i KRAJU BASKÓW ####
# Katalonia - ES51
# Kraj Basków - ES21

#Wyznaczamy mnoÅ¼niki
W1_new <- listw2mat(W1_list)
# usuniecie regionow bez sasiadow
W1_new_f = W1_new[-which(apply(W1_new,1,sum)==0),-which(apply(W1_new,1,sum)==0)]
N <- dim(W1_new_f)[1]
sp.multiplier <- solve(diag(N) - model.SAR.points$lambda * W1_new_f)
epsilon <- matrix(0, nrow = N, ncol = 1)
#WstrzÄ…s o 1 odchylenie standardowe
epsilon[which(rownames(W1_new_f) %in% c("ES21","ES51")), 1] <- 1
dY <- sp.multiplier %*% epsilon
rownames(dY) = rownames(W1_new_f)

#DoÅ‚Ä…czamy do przestrzennej bazy danych
dY.regions <- data.frame(rownames(dY),dY, stringsAsFactors = FALSE)
colnames(dY.regions) <- c("NUTS_ID","dY")
spatial_data_new <- merge(x = spatial_data, y = dY.regions[,c("NUTS_ID","dY")], 
                          by.x = "NUTS_ID", 
                          by.y = "NUTS_ID")

#Ilustrujemy
colors <- brewer.pal(9, "Greens")
brks <- classIntervals(spatial_data_new$dY, n = 9, style = "jenks")
brks <- brks$brks
windows(12,7)
plot(spatial_data_new, col = colors[findInterval(spatial_data_new$dY, brks,all.inside = TRUE)], 
     axes = F, pch=16, cex = 0.5)
title(paste ("Wp³yw jednostkowego wstrz¹su w obu regionach"))
legend(x = "right", legend = leglabs(round(brks, 3), under = "<", over = ">"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

# 
# colors <- c(brewer.pal(3, "BuGn"),"red")
# #spatial_data$X2014 <- as.numeric(spatial_data$gdp_eur_hab)
# #brks <- classIntervals(round(spatial_data_new$dY[-which.max(spatial_data_new$dY)],2),n = 3, style = "kmeans")
# #brks <- brks$brks
# brks = c(0.07,0.08,0.09,0.5)
# windows(10, 7)
# plot(spatial_data_new, 
#      col = colors[findInterval(round(spatial_data_new$dY,2), 
#                                brks,all.inside = F)], axes = F)
# 
# #plot(spatial_data_new, col = colors[findInterval(round(spatial_data_new$dY,2), brks,all.inside = TRUE)], axes = F)
# title(paste ("PKB w euro na mieszkañca (NUTS 3)"))
# legend(x = "right", legend = leglabs(round(brks, 2), under = "poni¿ej", over = "powy¿ej"), 
#        fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)
# 

#### WSTRZ¥S W KATALONII i KRAJU BASKÓW ####

#### MODEL SAR ####
model2b <- stsls(gdp_eur_hab ~ disposable_income, 
                 #spatial_data@data$property_income +
                 #spatial_data@data$stolica, 
                 listw = W1_list,zero.policy = T,data = spatial_data@data)
summary(model2b)
res2b <- model2b$residuals
brks <- classIntervals(res2b, n=9, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res2b, brks,all.inside = TRUE)], axes = F)
title(paste ("Reszty z modelu przestrzennej autoregesji"))
legend(x = "right", y = NULL, 
       legend = leglabs(round(brks), under = "poni¿ej", over = "powy¿ej"), 
       fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

moran.test(res2b, listw = W1_list,zero.policy = T)
lm.LMtests(res2b, listw = W1_list, test = "LMerr",zero.policy = T)
#### MODEL SAR ####

#### MODEL SEM ####
model3a <- errorsarlm(
  gdp_eur_hab ~ disposable_income,
  listw = W1_list,zero.policy = T,data = spatial_data@data)
summary(model3a)
res3a <- model3a$residuals
moran.test(res3a, listw = W1_list, zero.policy = T)
#### MODEL SEM ####

#### MODEL SLX ####
model4b <- lmSLX(gdp_eur_hab ~ disposable_income,
                 #spatial_data@data$property_income,
                 listw = W1_list,zero.policy = T,data=spatial_data@data)
summary(model4b)
res4b <- model4b$residuals
lm.morantest(model4b, listw = W1_list, zero.policy = T)
lm.LMtests(model4b, listw = W1_list, test = "all", zero.policy = T)
#### MODEL SLX ####

#### ILUSTRACJA RESZT NA MAPIE I BADANIE PRZESTRZENNEJ AUTOKORELACJI ####
spatial_data@data$res2b <- model2b$residuals
plot(spatial_data@data$res2b)

spatial_data@data$res3a <- model3a$residuals
plot(spatial_data@data$res3a)

spatial_data@data$res4b <- model4b$residuals
plot(spatial_data@data$res4b)
#### ILUSTRACJA RESZT NA MAPIE I BADANIE PRZESTRZENNEJ AUTOKORELACJI ####

#### MODEL SARAR ####
model5a <- sacsarlm(spatial_data@data$gdp_eur_hab ~ 
                      spatial_data@data$disposable_income + 
                      spatial_data@data$property_income, 
                    listw = W1_list,zero.policy = T)
summary(model5a)
res5a <- model5a$residuals
# dane.przestrzenne@data$res5a <- model5a$residuals
# plot(dane.przestrzenne@data$res5a)
moran.test(res5a, listw = W1_list,zero.policy = T)

#### MODEL SARAR ####

#### MODEL SDM ####
model6a <- lagsarlm(spatial_data@data$gdp_eur_hab ~ 
                      spatial_data@data$disposable_income, 
                    #spatial_data@data$property_income, 
                    listw = W1_list,zero.policy = T)
summary(model6a)
res6a <- model6a$residuals
moran.test(res6a, listw = W1_list, zero.policy = T)

#### MODEL SDM ####

#### MODEL SDEM ####
model7b <- errorsarlm(spatial_data@data$gdp_eur_hab ~ 
                        spatial_data@data$disposable_income, 
                      #spatial_data@data$property_income, 
                      listw = W1_list,zero.policy = T)
summary(model7b)
res7b <- model7b$residuals
moran.test(res7b, listw = W1_list, zero.policy = T)

#### MODEL SDEM ####

#### TESTY 2M/LR ####
#LR test SDM vs SEM (mo¿emy tak przetestowaæ ka¿d¹ parê modeli)
# Musza to byc modele zagniezdzone 
lL0 <- logLik(model2a)
lL1 <- logLik(model6a)
# Wartosc statystyki testowej
LRa <- 2 * (lL1 - lL0)
# Statystyk Chi - kwadrat ma taka liczbe stopni swobody jak liczba parametrow 
# szacowanych w modelu
# Testujemy mozliwosc eliminacji 2 zmiennych wiec mamy 2 restrykcje
# Nie mamy podstaw do odrzucenia H0 ze theta jest rowna 0
# Model szczegolny nie jest gorszy od ogolnego a wiec bedzie preferowany
# bo ma mniej parametrow
LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE)
# Inny, prostszy sposób :
LRb <- LR.sarlm(lL1, lL0)
#### TESTY 2M/LR ####

#### EFEKTY ####
#Efekty z SDEM
SDEM <- errorsarlm(gdp_eur_hab ~ disposable_income, 
                   etype = "emixed", 
                   listw = W1_list,
                   zero.policy = T,
                   data=spatial_data@data
)
impacts.SDEM <- impacts(SDEM, listw = W_list, zstats = TRUE, R = 200)
summary(impacts.SDEM)

# #Efekty z SAR
# SAR <- lagsarlm(gdp_eur_hab ~ disposable_income, 
#                 listw = W1_list,
#                 #control = list(fdHess = TRUE),
#                 zero.policy = T,
#                 data=spatial_data@data)
# impacts.SAR <- impacts(SAR, listw = W1_list, zstats = TRUE, R = 200)

#Efekty z SARAR
SARAR <- sacsarlm(gdp_eur_hab ~ disposable_income, 
                  zero.policy = T,
                  listw = W1_list,
                  data = spatial_data@data)
summary(SARAR)
impacts.SARAR <- impacts(SARAR, listw = W1_list, zstats = TRUE, R = 200)
HPDinterval.lagImpact(impacts.SARAR, prob = 0.95, choice = "direct")
HPDinterval.lagImpact(impacts.SARAR, prob = 0.95, choice = "indirect")
HPDinterval.lagImpact(impacts.SARAR, prob = 0.95, choice = "total")

#Efekty z SDM
SDM <- lagsarlm(gdp_eur_hab ~ disposable_income, 
                listw = W1_list,
                zero.policy = T,
                type = "Durbin", 
                data = spatial_data@data,
                control = list(fdHess = TRUE))
impacts.SDM <- impacts(SDM, listw = W1_list, zstats = TRUE, R = 200)
HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "direct")
HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "indirect")
HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "total")
#### EFEKTY ####

#### MODELE PRZESTRZENNE ####
