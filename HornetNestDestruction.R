# setwd("")
library(tidyverse)
library(readxl)
data <- read.csv("Destruction.csv")

Destru21 %>% filter(Encontrado == "SI") %>% group_by(location, Destruction) %>% 
  summarise(number = length(Destruction)) %>% group_by(location, Destruction) -> counts

counts %>% group_by(location) %>% summarise(Destruction, number, porcen = number/sum(number)*100) -> porcens

porcens %>% ggplot(aes(x = location, y = number, fill = Destruction))+
  geom_col(color = "grey")+
  scale_fill_brewer(palette="YlGn")+
  geom_text(aes(label = paste0(round(porcen), "%")), position = position_stack(vjust = 0.6))+
  labs(x = "Location", y = "No. of Asian-hornet nests")+
  theme_minimal()