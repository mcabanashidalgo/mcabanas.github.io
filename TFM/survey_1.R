library(tidyverse)
library(ggthemes)

data <- Wages_in_the_Parlamentary_Institutions_European_Parliament_1_103_

## Gráfico distribución Hombre-Mujer
ggplot(data, aes(x=ID, y=Media, color=Gender))+
  geom_jitter(size=2.5)+
  scale_y_continuous(breaks=c( 2500, 3500, 4500, 5500, 6500, 7500), 
                     labels=c("Less than 2.500€", "2.500-3.500€", "3.500-4.500€", "4.500-5.500€", "5.500-6.500€", "6.500-7.500€"))+
  labs(
    title = "Salary Wages in the European Parliament",
    subtitle = "Pay rate by gender", 
    x= "Number of observations",
    y="Salary/month",
    caption = "Data: Wages in the Parlamentary Institutions
    Graph: Marc Cabañas Hidalgo"
  )+
  geom_hline(yintercept = 3871,3, linetype="dashed")+
  scale_colour_wsj()+
  theme_clean()+
  annotate("text", x=5, y=3700, label="Average salary")+
  theme (plot.title = element_text(
                                                                                         size=rel(1.5), #Tamaño relativo de la letra del título
                                                                                         vjust=1, #Justificación vertical, para separarlo del gráfico
                                                                                         face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                                                                         lineheight=1))

## Gráfico distribución país
ggplot(data, aes(x=ID, y=Media, color=Gender))+
  facet_wrap(~Country)+
  geom_jitter(size=2.5)+
  scale_y_continuous(breaks=c( 2500, 3500, 4500, 5500, 6500, 7500), 
                     labels=c("Less than 2.500€", "2.500-3.500€", "3.500-4.500€", "4.500-5.500€", "5.500-6.500€", "6.500-7.500€"))+
  labs(
    title = "Salary Wages in the European Parliament",
    subtitle = "Pay rate by gender and Country", 
    x= "Number of observations",
    y="Salary/month",
    caption = "Data: Wages in the Parlamentary Institutions
    Graph: Marc Cabañas Hidalgo"
  )+
  geom_hline(yintercept = 3871,3, linetype="dashed")+
  scale_colour_wsj()+
  annotate("text", x=15, y=3700, label="Average salary", size=2)+
  theme (plot.title = element_text(
    size=rel(1.5), #Tamaño relativo de la letra del título
    vjust=1, #Justificación vertical, para separarlo del gráfico
    face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
    lineheight=1))


##Graficamos por partido
ggplot(data, aes(x=ID, y=Media, color=Gender))+
  facet_wrap(~Group)+
  geom_jitter(size=2.5)+
  scale_y_continuous(breaks=c( 2500, 3500, 4500, 5500, 6500, 7500), 
                     labels=c("Less than 2.500€", "2.500-3.500€", "3.500-4.500€", "4.500-5.500€", "5.500-6.500€", "6.500-7.500€"))+
  labs(
    title = "Salary Wages in the European Parliament",
    subtitle = "Pay rate by gender and Group", 
    x= "Number of observations",
    y="Salary/month",
    caption = "Data: Wages in the Parlamentary Institutions
    Graph: Marc Cabañas Hidalgo"
  )+
  geom_hline(yintercept = 3871,3, linetype="dashed")+
  scale_colour_wsj()+
  annotate("text", x=15, y=3700, label="Average salary", size=2)+
  theme (plot.title = element_text(
    size=rel(1.5), #Tamaño relativo de la letra del título
    vjust=1, #Justificación vertical, para separarlo del gráfico
    face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
    lineheight=1))


##Unión total

ggplot(data, aes(x=ID, y=Media ))+
  geom_jitter(aes(color=Group, shape=Gender))+
  facet_wrap(~Country)+
  scale_y_continuous(breaks=c( 2500, 3500, 4500, 5500, 6500, 7500), 
                     labels=c("Less than 2.500€", "2.500-3.500€", "3.500-4.500€", "4.500-5.500€", "5.500-6.500€", "6.500-7.500€"))+
  labs(
    title = "Salary Wages in the European Parliament",
    subtitle = "Pay rate by gender, politcal group and Country", 
    x= "Number of observations",
    y="Salary/month",
    caption = "Data: Wages in the Parlamentary Institutions
    Graph: Marc Cabañas Hidalgo"
  )+
  scale_color_manual(values = c("#155D95", #ECR
                                "#822386", #GUE
                                "#1DBAE8", #ID
                                "grey", #Others
                                "#1C7D91", #PPE
                                "#F5A21E", #Renew
                                "#E8231D",#S&D
                                "#52BE80" #Greens
  ))+
  theme (plot.title = element_text(
    size=rel(1.5), #Tamaño relativo de la letra del título
    vjust=1, #Justificación vertical, para separarlo del gráfico
    face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
    lineheight=1))

  
  

#### Correlaciones

#seleccionamos las variables a correlacionar

corr <- subset(data, select = c(Gender, Group, Country, Media))

x=cor(corr)








