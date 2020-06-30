### Más de ggplot2###
## Gráficos de barras y de lineas
library(plyr)
library(tidyverse)
dades <- read.csv("dades_censals_municipis.csv",
                  sep = "\t",
                  dec = ",",
                  fileEncoding = "latin1")
head(dades)

#Histogramas
ggplot(dades, aes(Immig.2000))+
  geom_histogram()+
  labs(x="Percentatge de població inmigrada, any 2000",
       y="Freqüència de municipis")
#Lo complicamos un poco más
ggplot(dades, aes(Immig.2000))+
  geom_histogram(colour="black", fill="white")+
  labs(x="Percentatge de població inmigrada, any 2000",
       y="Freqüència de municipis")+
  theme_minimal()
#modificamos el tamaño de los rectángulos con binwidth
ggplot(dades, aes(Immig.2000))+
  geom_histogram(colour="black", fill="white", binwidth = 2)+
  labs(x="Percentatge de població inmigrada, any 2000",
       y="Freqüència de municipis")+
  theme_minimal()
#Y ahora le decimos que nos muestre el valor medio del porcentaje de población inmigrada
ggplot(dades, aes(Immig.2000))+
  geom_histogram(colour="black", fill="white", binwidth = 2)+
  geom_vline(aes(xintercept=mean(Immig.2000, na.rm = TRUE)), colour="red",
linetype="dashed")+
  labs(x="Percentatge de població inmigrada, any 2000",
       y="Freqüència de municipis")+
  theme_minimal()


#Boxplots
ggplot(dades, aes(x=as.factor(provincia), y=Immig.2000,
                  fill=as.factor(provincia)))+
  geom_boxplot()+
  labs(fill="Província")+
  scale_x_discrete(labels=c("Bacelona", "Girona","Lleida", "Tarragona"))+
  scale_fill_discrete(labels=c("Bacelona", "Girona","Lleida", "Tarragona"))+
  labs(x="",
       y="Distribució percentatge de població immigrada")+
  theme_minimal()
#Quitamos la leyenda con guide=FALSE en scales_fill
ggplot(dades, aes(x=as.factor(provincia), y=Immig.2000,
                  fill=as.factor(provincia)))+
  geom_boxplot()+
  labs(fill="Província")+
  scale_x_discrete(labels=c("Bacelona", "Girona","Lleida", "Tarragona"))+
  scale_fill_discrete(guide=FALSE)+
  labs(x="",
       y="Distribució percentatge de població immigrada")+
  theme_minimal()

#Gráficos de barras
ggplot(dades, aes(x=as.factor(provincia)))+
  geom_bar(stat="count")+
  labs(x="",
       y="Nombre de municipis")+
  scale_x_discrete(labels=c("Barcelna", "Girona", "Lleida","Tarragona"))+
  theme_minimal()
##En este caso nos cuenta el nombre total de municipios por províncias, si quisieramos que nos diera la población mediana de los municipios de cada província hariamos lo siguiente:
mitjanes <- dades%>% group_by(provincia)%>%
  dplyr::summarise(mean_pob=mean(pob.2001, na.rm = TRUE))

mitjanes$nom_prov <- recode(mitjanes$provincia, "8"="Barcelona", 
                            "17"="Girona",
                            "25"="Lleida",
                            "43"="Tarragona")       ### Les damos nombres a las províncias, en vez de numeros, pero sin substituirlo, solo añadiendo columna
mitjanes
ggplot(mitjanes, aes(nom_prov, mean_pob))+
  geom_bar(stat = "identity")+     ##identity representa idénticamente el valor de la población
  coord_flip()+
  theme_minimal()

#Ahora queremos representar el nivel de población media de cada província en dos años diferentes. 
dades_noves <- dades%>% select(municipi, provincia, pob.2001, 
                               pob.2009, Comarca)%>% as.tibble()
dades_noves
dad_g <- dades_noves%>%gather(key="any",value = "població",
                               -municipi, -provincia,-Comarca)%>%
  separate(any, into = c("variable", "any"))
dad_g

mitjanes <- dad_g%>%group_by(provincia, any)%>%
  dplyr::summarise(mean_pob=mean(població,na.rm = TRUE))

mitjanes$nom_prov <- recode(mitjanes$provincia, "8"="Barcelona", 
                            "17"="Girona",
                            "25"="Lleida",
                            "43"="Tarragona") 
ggplot(mitjanes, aes(nom_prov, mean_pob, fill=any))+
  geom_bar(stat = "identity")+
  theme_minimal()

#Y si queremos las barras independientes y no superpuestas:
ggplot(mitjanes, aes(nom_prov, mean_pob, fill=any))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme_minimal()

#Para mostrar la variación cin barras de error
mitjanes <- dad_g%>%group_by(provincia,any)%>%
  dplyr::summarise(mean_pob=mean(població, na.rm = TRUE),
                   se=sd(població, na.rm = TRUE)/sqrt(length(població)))
mitjanes$nom_prov <- recode(mitjanes$provincia, "8"="Barcelona", 
                            "17"="Girona",
                            "25"="Lleida",
                            "43"="Tarragona") 
ggplot(mitjanes, aes(nom_prov, mean_pob, fill=any))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=mean_pob-se, ymax=mean_pob+se),       #Con este comando indicamos las barras de error. se(standar error)
                width=.2,
                position = position_dodge(1))+
  theme_minimal()

#Gráficos de linea
ggplot(mitjanes, aes(any, mean_pob, group=nom_prov, colour=nom_prov))+
  geom_line()+
  geom_point()+
  labs(x="",
       y="Població mitjana",
       colour="Provincia")+
  theme_minimal()
#Tambien las podemos presentar por separado:
ggplot(mitjanes, aes(any, mean_pob, group=1))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=mean_pob-se, ymax=mean_pob+se),
                width=.05,
                position = position_dodge(1))+
  facet_wrap(~nom_prov)+
  labs(x="",
       y="Població mitjana") +
  theme_minimal()








