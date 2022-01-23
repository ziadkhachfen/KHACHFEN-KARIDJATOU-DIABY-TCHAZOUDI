library(tidyverse)


## Importation des donnees 
url="https://data.enedis.fr/explore/dataset/prod-region/download/?format=csv&timezone=Africa/Lagos&lang=fr&use_labels_for_header=true&csv_separator=%3B"

download.file(url,destfile="data1.csv")

url1="https://data.enedis.fr/explore/dataset/conso-sup36-region/download/?format=csv&timezone=Africa/Lagos&lang=fr&use_labels_for_header=true&csv_separator=%3B"

download.file(url1,destfile="data_sup.csv")

url2="https://data.enedis.fr/explore/dataset/conso-inf36-region/download/?format=csv&timezone=Africa/Lagos&lang=fr&use_labels_for_header=true&csv_separator=%3B"

download.file(url2,destfile="data_inf.csv")


data1 <-  read.csv("data1.csv", sep = ";", encoding = "UTF-8")
data_inf <- read.csv("data_inf.csv", sep = ";", encoding = "UTF-8")
data_sup <- read.csv("data_sup.csv", sep = ";", encoding = "UTF-8")


data1 <-rename(data1 , "Region" = "Région", "Filiere.de.production" = "Filière.de.production" ,"Courbe.Moyenne.n1..Wh." = "Courbe.Moyenne.n.1..Wh." ,   "Total.Energie.injecte..Wh."= "Total.énergie.injectée..Wh." ) 
data_sup <-rename(data_sup,c("Total.Energie.soutire..Wh."= "Total.énergie.soutirée..Wh."  ,"Courbe.Moyenne.n1..Wh." = "Courbe.Moyenne.n.1..Wh." ,  "Region" = "Région",  "Secteur.activite" = "Secteur.activité" ))
data_inf <-rename(data_inf,c( "Total.Energie.soutire..Wh." = "Total.énergie.soutirée..Wh.", "Courbe.Moyenne.n1..Wh." = "Courbe.Moyenne.n.1..Wh.", "Region"= "Région"))


### Trairement de donnnees:
library(lubridate)
data1$Horodate <- as.POSIXct(data1$Horodate, tz="UTC", "%Y-%m-%dT%H:%M:%OS")
data_inf$Horodate <-as.POSIXct(data_inf$Horodate  ,tz="UTC", "%Y-%m-%dT%H:%M:%OS")
data_sup$Horodate <- as.POSIXct(data_sup$Horodate , tz="UTC", "%Y-%m-%dT%H:%M:%OS")

data1 <- na.omit(data1)
data_inf <- na.omit(data_inf)
data_sup <-na.omit(data_sup)


########### 

data1$Total.Energie.injecte..Wh. <- data1$Total.Energie.injecte..Wh./1000000
data_sup$Total.Energie.soutire..Wh. <- data_sup$Total.Energie.soutire..Wh./1000000
data_inf$Total.Energie.soutire..Wh. <- data_inf$Total.Energie.soutire..Wh./1000000




library(reshape2)

#tableau pour inferieur à 36
CT_inf <-c("Nb.points.soutirage","Total.Energie.soutire..Wh.","Courbe.Moyenne.n1..Wh.")
data_inf_verticale <- melt(data_inf, id.vars = c("Horodate", "Region", "Profil", "Plage.de.puissance.souscrite"), measure.vars = CT_inf)


categorie = unique(data_inf_verticale$variable )
filliere_prod = data1$Filiere.de.production

#tableau pour production 

CT_prod <-c("Total.Energie.injecte..Wh.","Nb.points.injection","Courbe.Moyenne.n1..Wh.")
data_11_verticale <- melt(data1, id.vars = c("Horodate", "Region", "Plage.de.puissance.injection", "Filiere.de.production"), measure.vars = CT_prod)


CT_sup <-c("Nb.points.soutirage","Total.Energie.soutire..Wh.","Courbe.Moyenne.n1..Wh.")
data_sup_verticale <- melt(data_sup, id.vars = c("Horodate", "Region", "Profil", "Plage.de.puissance.souscrite", "Secteur.activite"), measure.vars = CT_sup)



