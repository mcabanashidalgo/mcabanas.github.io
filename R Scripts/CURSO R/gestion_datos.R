
##Importació de dades

barris <- read.csv("lloguer_barris.csv", 
                   sep = ",", 
                   na.strings = "n.d.",            ##Cargamos los datos
                   fileEncoding = "latin1")

## Aquestes són les 6 primeres observacions
head(barris)

## Aquestes són les 6 darreres observacions
tail(barris)

##Quantes files i columnes té la base de dades
dim(barris)


##Treballem amb un altre format de dades

#SPSS
library(foreign)
datacis <- read.spss("es2799.sav")
class(datacis)

##Esto nos genera una lista, pero no nos lee el dataset, así que aplicamos lo siguiente

datacis <- read.spss("es2799.sav", 
                     to.data.frame = TRUE)    ##Le decimos que es un dataframe
dim(datacis)

#Lo observamos
datacis[1:10, 1:12]
names(datacis)


#Niveles de medida de R: Hay *4* tipos que los ejecutamos con class()
class(barris$T1) #Numeric
class(datacis$CCAA) #factor

#Qué categorias tiene la variable CCAA?
levels(datacis$CCAA)
class(datacis$CUES) #numeric
datacis$CUES.c <- as.character(datacis$CUES)     #creamos una nueva variable pero de otra clase
class(datacis$CUES.c) #Resultado = character
#Nos fijamos en los 2 tipos de variables CUES y CUES.c
datacis$CUES [1:10]
datacis$CUES.c[1:10]  #Al ser caracter aparece entre " "

## Recodificación de variables
table(barris$T1)
barris$T1_rec <- cut(barris$T1, breaks=4) #Número de cortes que definirán los intervalos
table(barris$T1_rec)
#Ahora cambiamos el rango por nombres/categorías
barris$T1_rec <- cut(barris$T1, breaks = 4, 
                     labels = c("Baix", "Baix-Mig", "Mig-Alt", "Alt"))  #Ponemos nombres
barris$T1_rec <- as.factor(barris$T1_rec)
class(barris$T1_rec) #Hemos creado una bariable categórica
table(barris$T1_rec) #Creamos una tabla de frecuencias
head(barris) #La variable aparece añadida como una columna en la base de datos


#Tambien podríamos establecer por nosotros mismos los cortes de los intervalos
barris$T1_rec <- cut(barris$T1, breaks = c(300,550,800,950,1200,1450),
                     labels=c("Baix", "Baix-Mig","Mig","Mig-Alt","Alt"))
table(barris$T1_rec)

#De contínuas a dicotómicas
barris$T1_dic <- ifelse(barris$T1>mean(barris$T1, na.rm = TRUE),     ##ifelse() divide valores de una variable en dos partes en función de si cumple o no los criterios establecidos
                        1,0)
barris$T1_dic <- as.factor(barris$T1_dic) #La convertimos en un factor
table(barris$T1_dic)
#Cambiamos 0,1 por Alt i Baix
barris$T1_dic <- ifelse(barris$T1>mean(barris$T1, na.rm = TRUE), 
                        "Alt", "Baix")
barris$T1_dic <- as.factor(barris$T1_dic)
table(barris$T1_dic)

#De categóricas a categóricas
barris$t1_rec_3[barris$T1_rec=="Baix"] <- "Baix"
barris$t1_rec_3[barris$T1_rec=="Baix-Mig"|
                  barris$T1_rec=="Mig"|
                  barris$T1_rec=="Mig-Alt"] <- "Mig"
barris$t1_rec_3[barris$T1_rec=="Alt"] <- "Alt"           ##reformulamos los valores para tener los 5 anteriores en solo 3
table(barris$t1_rec_3)

#Ordenamos estas en función de como queramos gracias a levels

barris$t1_rec_3 <- factor(barris$t1_rec_3,
                          levels = c("Alt","Mig","Baix"))
table(barris$t1_rec_3)

#Si lo hacemos ahora en inglés

barris$t1_rec_3_en <- factor(barris$t1_rec_3,
                             labels = c("High","Medium","Low"))
table(barris$t1_rec_3_en)

#De categóricas a dicotómicas

barris$t1_dic_2 <- ifelse(barris$T1_rec=="Alt"|barris$T1_rec=="Mig-Alt",
                          "lloguer alt","lloguer mig o baix")
barris$t1_dic_2 <- as.factor(barris$t1_dic_2)
table(barris$t1_dic_2)

#Noves variables
barris$T1_quadrat <- (barris$T1)^2  # Elevamos al cuadrado
barris$T1_quadrat <- sqrt(barris$T1)  #raíz cuadrada
barris$T1_log <- log(barris$T1)  #logaritmo natural
head(barris)

#Representación gráfica
#barplot
t <- table(barris$T1_rec)
barplot(t)  #Así hacemos un gráfico simple
#Damos un poco de forma al gráfico
barplot(t,
        xlab = "Nivell de lloguer dels barris",
        ylab = "Freqüència",
        main = "Distribució de lloguers a Barcelona")


#Gráficos de caja (boxplot)
boxplot(barris$T1,
        horizontal = TRUE,
        xlab="Lloguer mitjà")
#Si lo ejecutamos como false, se cambia el sentido. Además añadimos los barrios y distritos
boxplot(barris$T1~barris$districte,
        horizontal = FALSE,
        xlab= "Districte",
        ylab="Lloguer Mitjà")

#Histogrames
hist(barris$T1) 
#Modificamos un poco el histograma
hist(barris$T1,
     main = "Distribució del lloguer dels pisos dels barris de Barcelona",
     xlab = "Lloguer mensual mitjà",
     ylab = "Freqüència",
     col="blue",
     breaks = 15,
     xlim = c(200,1400),
     ylim = c(0,14),
     las=1)
#Gráfico de puntos
dotchart(barris$T1) #Como que tenemos muchos puntos y no hay información sobre ellos, lo primero que hacemos es decir el tamaño de la letra de los nombres de los barrios (0.3)

dotchart(barris$T1, labels = barris$barri, cex = 0.3, 
         xlab = "Lloguer mensual mitjà")
#ordenamos los barrios mediante order() en función del nivel del alquiler (de más a menos)
barris.ord <- barris[order(barris$T1,
                           decreasing = TRUE),] 
head(barris.ord)

dotchart(barris.ord$T1,
         labels = barris.ord$barri,
         cex = 0.3,
         xlab = "Lloguer mensual mitjà",
         pch = 20)   #pch son los símbolos de los puntos (si ejecutamos ?pch tenemos más info)
#variación entre los barrios de cada distrito
barris.ord$districte <- factor(barris.ord$districte)
dotchart(barris.ord$T1,
         labels = barris.ord$barri,
         cex = 0.3,
         xlab = "Lloguer mensual mitjà",
         pch = 20,
         groups = barris.ord$districte)
#Exportación de gráficos
pdf("histograma_blau.pdf", width =8, height = 6)
hist(barris$T1,
     main = "Distribució del lloguer dels pisos dels barris de Barcelona",
     xlab = "Lloguer mensual mitjà",
     ylab = "Freqüència",
     col="blue",
     breaks = 15,
     xlim = c(200,1400),
     ylim = c(0,14),
     las=1)
dev.off() #de esta manera lo hemos exportado a la carpeta seleccionada
