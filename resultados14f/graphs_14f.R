## Gràfics 14F
install.packages("hrbrthemes")
library(tidyverse)
library(ggthemes)
library(hrbrthemes)
install.packages("plotly")
install.packages("gapminder")
library(plotly)
library(gapminder)

## cargamos base de datos

datos <- read_csv("Desktop/resultados14f/total.eleccions.csv", 
                            col_types = cols(RBM_pers17 = col_number()), 
                            locale = locale(date_names = "es"))

class(datos$RBM_pers17)


##Gráficos

#1- Abstención en función de la renta bruta media personal (2017)

#previamente creamos una nueva variable que agrupe en función de los deciles de renta

ggplot(datos, aes(x=datos$RBM_pers17, y=datos$perc_abst))+
  geom_jitter(aes(color=datos$`1rpartit`))+
  labs(
    title = "Abstenció i renda el 14F",
    subtitle = "Municipis (per partit guanyador), segons la renda mitjana bruta per persona i el percentage d'abstenció el 14F", 
    x= "Renda bruta mitjana per persona (2017)", 
    y= "Abstenció"
  )+
  geom_smooth()+
  facet_wrap(~datos$`Nom Província`)+
  scale_fill_manual(values=c("yellow", "orange","green", "dark blue", "blue", "red", "dark green"))


#2- VOX on treu més del 10%

vox <- filter(datos, datos$VOX.perc>10)


ggplot(vox, aes(x=vox$RBM_pers17, y=vox$VOX.perc))+
  geom_point(aes(color=vox$`Nom Província`, size=Poblacio))+
  geom_smooth(se= FALSE)+
  labs(
    title = "Suport a VOX en funció de la renda", 
    subtitle = "10% o més de suport a VOX per municipi en funció de la renda bruta mitjana (2017)",
    x= "Nivell de renda",
    y="Vot a Vox")+
  scale_color_discrete("Província")+
  geom_text(aes(label = vox$Municipi), size=3, vjust=-1)


#suport a vox en funció de l'abstenció

ggplot(vox, aes(x=vox$perc_abst, y=vox$VOX.perc))+
  geom_point(aes(color=`Nom Província`, size=Poblacio))+
  geom_smooth(se=FALSE)+
  labs(
    title = "Vot a Vox i abstencionisme",
    subtitle = "Suport a VOX en funció del percentatge d'abstenció del municipi. Només s'observen aquells municipis que tenen +10% vot a VOX", 
    x="Abstenció", 
    y="Vot a Vox"
  )

