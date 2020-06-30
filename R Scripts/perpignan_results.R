library(tidyverse)
library (ggthemes)
library(ggplot2)
library(readxl)
data <- read_excel("Desktop/perpignan_results.xlsx")
names(data)=c("Coalition", "Party", "Candidate", "first_round", "second_round")
x <- filter(data, Party=="LR"|Party=="RN")

library(png)
escudo1 <- readPNG("Desktop/l_aliot.png")
escudo2 <- readPNG("Desktop/j_pujol.png")
library(grid)

ggplot(x, aes(x=Candidate, y=second_round, fill=Party))+
  geom_col()+
  scale_fill_manual(values = c("DARK BLUE",
                               "#2C3E50"
                               ))+
  labs(
    title = "Perpignan Elections (28/06/2020)",
    subtitle = "Results second round", 
    caption = "Data: rfi.fr
    Chart: @mcabanashidalgo",
    x= "Candidate",
    y= "% vote") +
  theme_solarized_2()+
  theme (plot.title = element_text(
    face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
    color="black", #Color del texto
    lineheight=1.5))
