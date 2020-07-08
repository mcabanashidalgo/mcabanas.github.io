library(readxl)
library(tidyverse)
library(ggthemes)
data <- read_excel("Desktop/docs_x.xlsx")

data$totalas <- data$`Ass. Male`+data$`Ass. Female`

ggplot(data, aes(x=Group, y=MEPs, color=Country))+
  geom_point(size=data$`MEPs FEM`)+
  scale_color_stata()+
  theme_fivethirtyeight()+
  labs(
    title  ="MEPs women for country and group",
    subtitle = "The size of the circle determines the number of women",
    caption = "Data: European Parliament
    Chart: @mcabanashidalgo"
  )

ggplot(data, aes(x= `MEPs FEM`, y=`Ass. Female`, color=Group))+
  geom_point()+
  facet_grid(~Country)+
  theme_fivethirtyeight()+
  scale_colour_stata()+
  theme(legend.position = "bottom")+
  labs(
    title = "MEPs assistants female",
    subtitle = "Number of female assistants depending on country, party and number of female MEPs", 
    caption = "Data: European Parliament
    Chart: @mcabanashidalgo",
    x= "Number of MEPs female"
  )

ggplot(aes(x=data$`MEPs MALE`, y=data$`MEPs FEM`))+
  geom_line()
