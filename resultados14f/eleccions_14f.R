### Análisis 14F: Municipios ###

library(tidyverse)
library(tidyr)
library(readxl)

#Datos


#1- Dades electorals municipals (dades obertes GENCAT)
data <- read_excel("Desktop/eleccions14_recod.xlsx")
names(data)
names(data)[4]="Codi Municipi"

data1 <- read_excel("Desktop/resultats_14fmun.xlsx", 
                     sheet = "Hoja2")
names(data1)

names(data1)[3]= "Nom.Municipi"

#Unimos las dos hojas

x <- merge(data, data1, by= "Nom.Municipi")
x$mun <- str_to_lower(x$Nom.Municipi)

#2- Datos Población

pob <- read_excel("Desktop/pob_cat_mun20.xlsx")

pob$mun <- str_to_lower(pob$Nom.Municipi)
pob$codi <- str_sub(pob$Codi.mun, 0,-1)

#3- Codi INE
codis <- read.csv("Desktop/mpiscatalunya-4.csv", 
                  fileEncoding = "utf8",
                  sep = ";",
                  skip = 3)
names(codis)[1]="CodiINE"
codis$mun <- str_to_lower(codis$Nom.Municipi)                 #### hem posat en minúscules tots els noms dels municipis, per tal d'unir-los

dem <- merge(codis, pob, by="mun")

#Unim totes les dades

a <- merge(x, dem, by="mun")


#Fem neteja


delete <- c("Codi de comarca", "Codi municipi", "Nom.Municipi.y", "X", "Codi de Catalunya", "Codi de circumscripció", "Valor fix", "Codi Municipi", "Nombre total de meses")
a <- a[,!(names(a)%in%delete)]

delete_1 <- c("Codi.comarca", "Nom.comarca", "x", "Codi.mun")
a <- a[,!(names(a)%in%delete_1)]
names(a)

a <- a[,-34]

## Arreglamos nombres

names(a)

names(a)[34]="Poblacio"



##A Añadimos datos de renta bruta mitja per persona (INE) (2017)

renta <- read_excel("Desktop/rmb_p17.xlsx")
names(renta)[1]="codi"
names(renta)[2]="Municipi"


### Unimos datos de renta

total14 <- merge(renta, a, by="codi")

write_csv(total14, "Eleccions14_tot.csv")

### DATOS 21D

ant <- read_excel("Desktop/DADES/Eleccions/Autonomiques/oficials_gencat_17/resum_2017.xlsx")
names(ant)[4]="codi"


### Unimos eleccions 2017 - 2021

total_cat <- merge(total14, ant, by="codi")

write_csv(total_cat, "total.eleccions.csv")
______________________________________________


### Gráficos

ggplot(total14, aes(x=total14$RBM_pers17, y=total14$perc_participacio))+
  geom_jitter(aes(color=total14$`1rpartit`))+
  scale_y_continuous(breaks=10,80, 90, 100)+
  scale_x_continuous(breaks = 5.000, 10.000, 15.000, 20.000, 25.000, 35.000,45.000, 55.000, 100.000)







