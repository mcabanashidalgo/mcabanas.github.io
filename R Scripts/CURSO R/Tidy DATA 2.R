## Tidy data II: cambios en la estructura de datos ##
#gather() - para cambiar datos de amplios a largos

library(tidyverse)
rfd <- read_csv("rfd_bcn.csv")
rfd

#Tenemos 4 varibles para 5 columnas... hay que cambiarlo con gather

rfd.g <- rfd%>%gather(key=ANY, value = RFD, -DTE, -BARRIS) ## Hemos cambiado el formato (recomiendo ver los dos datasets para observar la diferencia)

#Observamos la evolución de la renda familiar disponible de los barrios de l'Eixample entre 2008 y 2010
Eixample <- rfd.g%>%filter(DTE==2) #2 es el valor de l'Eixample
Eixample

#Lo graficamos
ggplot(Eixample, aes(ANY, RFD, group=1))+
  geom_line()+
  facet_wrap(~BARRIS)+
  labs(x="",
       y="Renda Familiar Disponible")


## SPREAD -- ponemos las observaciones en una sola fila

barris <- read_csv("rendafam2010_ample.csv")
barris

#Convertimos en amplias
barris.spread <- barris%>%spread(key = MESURA, value = VALOR)
barris.spread

#Ahora que ya lo tenemos listo, graficamos
ggplot(barris.spread, aes(x=POBLACIO,RFD, label=BARRIS))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x="Població del barri",
       y="Renda Familiar Disponible")+
  theme_minimal()

## SEPARATE -- Separar cosas que estan unidas
rfd.s <- rfd.g%>% separate(BARRIS, into = c("codi_barri", "nom_barri"),
                           extra = "merge") #Hemos separado el numero del barrio con el nombre de este. "merge" nos dice que todo lo que esté despues del separador, sale fuera

#Tambien podriamos haber añadido sep="." para indicar que se separará despues del punto.

#Ahora graficamos
Eixample <- rfd.s%>%filter(DTE==2)
ggplot(Eixample, aes(ANY,RFD, group=1))+
  geom_line(size=1)+
  facet_wrap(~nom_barri)+
  labs(x="",
       y="Renda Familiar Disponible")

## UNITE para unir cosas separadasx
rfd.t <- rfd.s%>% unite(codi_unic, DTE, codi_barri, sep = "_",
                        remove = FALSE)   ##Hemos dicho que nos una en un único código unido por _ el codigo del barrio y del distrito y que no los elimine de la base de datos (FALSE)
rfd.t












