## Ejercicio Marc Cabañas - Programación UNIR ##

setwd("Documents/")

# 1- Cargamos las librerias
library(tidyverse)
library(ggplot2)
library(ggthemes)
options(scipen=999)  # De esta forma, le decimos que no queremos que nos aparezca notación científica en los datos

#Cargamos el csv
path <- "https://gist.githubusercontent.com/mcabanashidalgo/55755b83d434f37fc898a0add2c43edd/raw/239a64b8852043aafb1f8d8697abc431cde18af5/dades_inversions_SOC"
library(readr)
data<- read_csv(path,
                col_types = cols(F_IMPORT = col_number()), 
                locale = locale(decimal_mark = ",", grouping_mark = "."),  
                na = "0")
names(data)

#Hacemos las transformaciones necesarias


data_f <- spread(data, key = ANY_PROGRAMACIÓ, value =F_IMPORT ) #hacemos spread para obtener el numero de años para luego modificarlo

data_x <- select(data_f, D_COMARCA, `2015`, `2016`, `2017`, `2018`, `2019`, C_GARANTIA_JUVENIL, C_SERVEI_OCUPACIONAL, C_COMARCA, F_PERSONES, D_MUNICIPI) #seleccionamos las columnas que nos interesan

names(data_x)

names(data_x) <- c("Comarca", "'15","'16","'17","'18","'19", "Garantía_juv", "Servei_Ocu", "Cod_com", "Personas", "Municipi") # cambiamos nombres

data_a <- gather(data_x, key = "Any", value = "Import", -Garantía_juv, -Servei_Ocu, -Comarca , -Cod_com, -Personas, -Municipi) #volvemos a juntar los años

delete.na <- function(data_a, n=0) {
  data_a[rowSums(is.na(data_a)) <= n,]
}
data_a <- delete.na(data_a) ##eliminamos los NA (no porogramas de inversion o sin datos)

data_a<- filter(data_a, Comarca=="Baix Llobregat"|Comarca== "Barcelonès"|Comarca=="Vallès Occidental"|Comarca== "Vallès Oriental")  #seleccionamos las comarcas que nos interesan

#Graficamos por primera vez para ver la cantudad de programas ocupacionales hechos en estas 4 comarcas



ggplot(data_a, aes(x=Any, y=Import, color= Personas))+
  geom_point(aes(size=Personas))+
  facet_grid(~Comarca)+
  labs(
    title = "Gasto anual del SOC por Comarcas",
    x= "Any programació",
    y= "Import",
    caption = "Datos: Generalitat de Catalunya.
    Elaboración propia: @mcabanashidalgo",
    subtitle = "Baix Llobregat, Barcelonès, Vallès Occidental y Vallès Oriental") +
  theme(legend.position="bottom")+
  theme_solarized_2()+
  scale_color_continuous_tableau()+
  scale_y_continuous(breaks=c(0, 1000000, 3000000, 5000000, 7000000,  9000000, 11000000, 13000000, 15000000), 
                     labels=c("0", "1.000.000", "3.000.000", "5.000.000", "7.000.000", "9.000.000","11.000.000" ,"13.000.000", "15.000.000")) #graficamos estas comarcas. 

# Ahora miramos el número de programas de Garantía Juvenil por anual
ggplot(data_a, aes(x=Any, y=Import, color= Garantía_juv))+
  geom_point()+
  facet_grid(~Comarca)+
  labs(
    title = "Programas de Garantía Juvenil ",
    x= "Año de programación",
    y= "Importe",
    caption = "Datos: Generalitat de Catalunya.
    Elaboración propia: @mcabanashidalgo",
    subtitle = "Baix Llobregat, Barcelonès, Vallès Occidental y Vallès Oriental") +
  theme_minimal()+
  theme(legend.position="right")+
  scale_y_continuous(breaks=c(0, 5000000, 10000000, 15000000), 
                     labels=c("0", "5.000.000", "10.000.000", "15.000.000"))+
  scale_color_tableau() 


## Guardamos el csv resultante

write_csv(data_a,"Practica_programacion_Marc.csv")
