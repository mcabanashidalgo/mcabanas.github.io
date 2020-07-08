## PAY GAP EP

library(readxl)
library(tidyverse)

scales <- read_excel("Desktop/TFM/scales.xlsx")
scales <- scales %>% unite(Party_Cou, Party:Country, sep = "_", remove = FALSE)
scales$suma <- scales$Ideology+scales$Geo

ggplot(scales, aes(y=Geo,x= Ideology, color=Party))+
  geom_point()+
  geom_label_repel(aes(label = Party_Cou),
                   box.padding   = 0.35, 
                   point.padding = 0.5)+
  theme_minimal()+
  scale_color_manual(values = c("#155D95", #ECR
                                "#52BE80", #Greens
                                "#822386", #GUE
                                "#1DBAE8", #ID
                                "grey", #Others
                                "#1C7D91", #PPE
                                "#F5A21E", #Renew
                                "#E8231D"  #S&D
                                ))+
  labs(
    x= "Geographical position",
    y="Ideology", 
    caption = "@mcabanashidalgo",
    subtitle = "Position of political parties based on the hypothesis raised"
  )+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_vline(xintercept = 0, linetype="dashed")+
  annotate("text", x=-2.51, y=-0.1, label="-- Pay Gap", color="#838E90")+
  annotate("text", x=0.31, y=-0.1, label="+ Pay Gap", color="#838E90")+
  annotate("text", x=0.31, y=2.5, label="++ Pay Gap", color="#838E90")+
  annotate("text", x=-2.51, y=2.5, label="- Pay Gap", color="#838E90")+
  ggtitle ("Pay Gap in the European Parliament") + # Título del gráfico
  theme (plot.title = element_text(family="Avenir Next",
                                   size=rel(1.5), #Tamaño relativo de la letra del título
                                   vjust=2, #Justificación vertical, para separarlo del gráfico
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   lineheight=1))


library(ggrepel)

library(ggthemes)

#2C3E50 #rojo
#59A1B0 #azul grisáceo
#822386 #purple
#40C5E1 #azul PP