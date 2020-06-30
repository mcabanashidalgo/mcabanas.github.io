library(tidyverse)
library(readxl)
options(scipen=999)
library(ggthemes)
data <- read_excel("Desktop/prog.ocupacionals.xlsx") # Cargamos los datos

names(data)

ggplot(data, aes(x=a, y=F_IMPORT))+
  geom_point(color="dark blue")+
  facet_wrap(~D_COMARCA)+
  labs(
    title = "Inversió anual del SOC per Comarques",
    x= "Any programació",
    y= "Import",
    caption = "Dades: Generalitat de Catalunya.
    Elaboració pròpia: @mcabanashidalgo"
  )+
  scale_y_continuous(breaks=c(0, 5000000, 10000000, 15000000), 
                     labels=c("0", "5.000.000", "10.000.000", "15.000.000"))     #Gráficamos en bruto

data_f <- spread(data, key = a, value =F_IMPORT ) #hacemos gather para obtener el numero de años para luego modificarlo
data_x <- select(data_f, D_COMARCA, `2015`, `2016`, `2017`, `2018`, `2019`, C_GARANTIA_JUVENIL, C_SERVEI_OCUPACIONAL, C_COMARCA, D_MUNICIPI) #seleccionamos las columnas que nos interesan
names(data_x)
names(data_x) <- c("Comarca", "'15","'16","'17","'18","'19", "Garantía_juv", "Servei_Ocu", "Cod_com", "municipi") # cambiamos nombres

data_a <- gather(data_x, key = "Any", value = "Import", -Garantía_juv, -Servei_Ocu, -Comarca , -Cod_com, -municipi) #volvemos a juntar los años

data_a[is.na(data_a)] <- 0 #na = 0€ de inversions

data_a<- filter(data_a, Comarca=="Baix Llobregat"|Comarca== "Barcelonès"|Comarca=="Vallès Occidental"|Comarca== "Vallès Oriental")  #seleccionamos las comarcas que nos interesan

ggplot(data_a, aes(x=Any, y=Import, color= Comarca))+
  geom_point()+
  facet_grid(~Comarca)+
  labs(
    title = "Inversió anual del SOC per Comarques",
    x= "Any programació",
    y= "Import",
    caption = "Dades: Generalitat de Catalunya.
    Elaboració pròpia: @mcabanashidalgo",
    subtitle = "Baix Llobregat, Barcelonès, Vallès Occidental i Vallès Oriental") +
  theme_fivethirtyeight()+
  theme(legend.position="none")+
  scale_y_continuous(breaks=c(0, 5000000, 10000000, 15000000), 
                     labels=c("0", "5.000.000", "10.000.000", "15.000.000")) #graficamos estas comarcas. 

ggplot(data_a, aes(x=Any, y=Import, color= Garantía_juv))+
  geom_point()+
  facet_grid(~Comarca)+
  labs(
    title = "Programes de Garantia Juvenil ",
    x= "Any programació",
    y= "Import",
    caption = "Dades: Generalitat de Catalunya.
    Elaboració pròpia: @mcabanashidalgo",
    subtitle = "Baix Llobregat, Barcelonès, Vallès Occidental i Vallès Oriental") +
  theme_fivethirtyeight()+
  theme(legend.position="right")+
  scale_y_continuous(breaks=c(0, 5000000, 10000000, 15000000), 
                     labels=c("0", "5.000.000", "10.000.000", "15.000.000"))+
  scale_color_tableau() #graficamos estas comarcas en función de si tienen garantia juvenil o no

?colorRampPalette()

ggplot(data_a, aes(x=Any, y=Import, color= Servei_Ocu))+
  geom_point()+
  facet_grid(~Comarca)+
  labs(
    title = "Programes de Garantia Juvenil ",
    x= "Any programació",
    y= "Import",
    caption = "Dades: Generalitat de Catalunya.
    Elaboració pròpia: @mcabanashidalgo",
    subtitle = "Baix Llobregat, Barcelonès, Vallès Occidental i Vallès Oriental",
    colour="Servei d'Ocupació") +
  theme_fivethirtyeight()+
  theme(legend.position="bottom", legend.box ="horizontal", legend.title = element_text(color = "blue", size = 1))+
  scale_y_continuous(breaks=c(0, 5000000, 10000000, 15000000), 
                     labels=c("0", "5.000.000", "10.000.000", "15.000.000")) #graficamos estas comarcas en función de si tienen garantia juvenil o no


#Vemos los programas por municipios del baix llobregat y el tipo de servei d'ocupació
baix <- filter(data_a, Comarca=="Baix Llobregat")
baix <- filter(baix, municipi =="Sant Joan Despí"|municipi =="Cornellà de Llobregat"|municipi =="Sant Boi de Llobregat"|municipi=="Esplugues de Llobregat"|municipi=="Gavà"|municipi=="Viladecans"|municipi=="Castelldefels")

ggplot(baix, aes(x=Any, y=Import, color=municipi))+
  geom_point()+
  facet_wrap(~Servei_Ocu)+
  labs(
    title = "Inversió del SOC al Baix Llobregat",
    x= "Any",
    y= "Import",
    caption = "Dades: Generalitat de Catalunya.
    Elaboració pròpia: @mcabanashidalgo",
    subtitle = "Quantitat i quantia dels programes ocupacionals a Gavà, Sant Joan Despí, Cornellà, Viladecans, Sant Boi, Castelldefels i Esplugues",
    colour="Municipi")+
  theme_fivethirtyeight()+
  theme(legend.position = "bottom", legend.box = "horizontal")+
  scale_y_continuous(breaks=c(0, 1000000, 2000000, 3000000), 
                     labels=c("0", "1.000.000€", "2.000.000€", "3.000.000€"))+
  scale_colour_tableau()
  
  
  
  
  
  
  
