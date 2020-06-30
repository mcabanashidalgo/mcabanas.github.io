## tutorials R:: ggplot2##

#Creación de un gráfico
library(ggplot2)
library(reshape2)

#En este ejercicio vincularemos los resultados electorales de BCN en 2015 con el % de parados de los barrios con los resultados del PSC.

result <- read.csv("resultats_municipals_2015_barris.csv",
                   fileEncoding = "latin1")
p <- ggplot(data = result,
            mapping = aes(x=atur, y=PSC.CP))
p       ##Como que no hemos dicho que forma (barra, linea, etc) va a dar forma a nuestro gráfico, nos aparece en blanco

p+geom_point()  #Elegimos que nos los de en número. Ahora modificamos un poco el gráfico

p+geom_point(color="red",
             size=2,
             shape=21)      #shape para elegir la forma, size el tamaño

#Añadimos una tercera variable

#Ahora queremos representar al partido más votado con otro color dentro del gráfico de puntos y creamos un gráfico diferente para cada partido más votado

ggplot(result, aes(x=atur, y=participacio_muni_2015, colour=partit.mes.votat))+
  geom_point()

#Para crear un gráfico separado para cada partido más votado, utilizamos facet_wrap()
ggplot(result, aes(x=atur, y= participacio_muni_2015))+
  geom_point()+
  facet_wrap(~partit.mes.votat)

#Tercera variable contínua
result$part_rec <- ifelse(result$participacio_muni_2015<mean(result$participacio_muni_2015),
                          "Participació baixa",
                          "Participació alta")  #Hemos creado una nueva variable que nos diga si los barrios participan más o menos de la media de participación
result$part_rec <- as.factor(result$part_rec)
ggplot(result, aes(x=atur, y=PSC.CP, colour=part_rec))+
  geom_point()  ##Lo que hemos dicho es: Colorea los barrios según su nivel de participación y muestrame la distribución en funcion del paro(x) y el voto al PSC(y)

#Ahora decidimos hacerlo por gradación de color en vez de colores diferente y le pedimos que nos lo separe por distitos
ggplot(result, aes(x=atur, y=PSC.CP))+
  geom_point(aes(color=participacio_muni_2015))+
  facet_wrap(~districte)

#Cambiar el tamaño en función de la participación en las municipales
ggplot(result, aes(x=atur, y=PSC.CP))+
  geom_point(aes(size=participacio_muni_2015))

#Añadimos el nombre del barrio al lado de cada punto 
ggplot(result, aes(x=atur, y=PSC.CP))+
  geom_point()+
  geom_text(aes(label=result$barri))

#Modificamos tamaño y letra
ggplot(result, aes(x=atur, y=PSC.CP))+
  geom_point()+
  geom_text(aes(label=barri),
            check_overlap = TRUE,
            size=3,
            nudge_y = 0.2,
            vjust=0)

## Coordenadas y escalas
ggplot(result, aes(x=renda_100, y=PSC.CP))+
  geom_point()+
  scale_y_continuous(breaks = c(seq(0,20,10)))   ##con scale_x/y_ cambiamos la medida de los ejes

ggplot(result, aes(x=partit.mes.votat, y=renda_100))+
  geom_jitter()+
  scale_x_discrete(labels=c("Barcelona en Comú", "Convergència i Unió"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  ##Con panel grid quitamos las lineas de division

##Coordenadas
ggplot(result, aes(x=partit.mes.votat, y=renda_100))+
  geom_jitter()+
  scale_x_discrete(labels=c("Barcelona en Comú", "Convergència i Unió"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 100, linetype="dashed") ## de esta manera marcamos una linea en el 100. "dashed" indica el tipo de linea

## Títulos y etiquetas
#etiquetas de ejes
ggplot(result, aes(x=renda_100, y=PSC.CP))+
  geom_point()+
  labs(x= "Renda disponible",
       y= "Percentatge de vot al PSC")
#Ahora incluímos títulos y subtítulos
ggplot(result, aes(x=renda_100, y=PSC.CP))+
  geom_point()+
  labs(x= "Renda disponible",
       y= "Percentatge de vot al PSC", 
       title = "Relació entre renda i vot al PSC",
       subtitle = "Cada punt representa un barri de Barcelona",
       caption = "Font: Ajuntament de Barcelona")

# TEMAS
ggplot(result, aes(x=renda_100, y=PSC.CP))+
  geom_point()+
  labs(x= "Renda disponible",
       y= "Percentatge de vot al PSC", 
       title = "Relació entre renda i vot al PSC",
       subtitle = "Cada punt representa un barri de Barcelona",
       caption = "Font: Ajuntament de Barcelona")+
  theme_minimal()

# o tambien podemos usar los ggthemes
library(ggthemes)
ggplot(result, aes(x=renda_100, y=PSC.CP))+
  geom_point()+
  labs(x= "Renda disponible",
       y= "Percentatge de vot al PSC", 
       title = "Relació entre renda i vot al PSC",
       subtitle = "Cada punt representa un barri de Barcelona",
       caption = "Font: Ajuntament de Barcelona")+
  theme_fivethirtyeight()

