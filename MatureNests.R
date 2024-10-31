# setwd("")
library(tidyverse)
library(readxl)
library(lubridate)
library(chron)
library(patchwork)
library(lme4)
library(nlme)
library(ggeffects)
library(performance)
library(DHARMa)
library(sjPlot)
library(car)
library(patchwork)
library(MuMIn)
data <- read.csv("MatureNest.csv")

datos[-c(15,57),] %>% dplyr::filter(AVE == "Abejero") %>%
  ggplot()+
  geom_point(aes(x = DISTANCIA, y = densHA, color = Territorio))+
  geom_smooth(aes(x = DISTANCIA, y = densHA))+
  ggtitle("Abejero")

datos %>% dplyr::filter(AVE == "Azor") %>%
  ggplot()+
  geom_point(aes(x = DISTANCIA, y = densHA, color = Territorio))+
  geom_smooth(aes(x = DISTANCIA, y = densHA))+
  ggtitle("Azor")

m0 <- lmer(data = databe, log(HubDist) ~ 1 + (1|Territorio), REML = F)
m1 <- lmer(data = databe, log(HubDist) ~ DISTANCIA + (1|Territorio), REML = F)
m2 <- lmer(data = databe, log(HubDist) ~ AVE + (1|Territorio), REML = F)
m3 <- lmer(data = databe, log(HubDist) ~ DISTANCIA + AVE + (1|Territorio), REML = F)
m4 <- lmer(data = databe, log(HubDist) ~ DISTANCIA : AVE + (1|Territorio), REML = F)
m5 <- lmer(data = databe, log(HubDist) ~ DISTANCIA * AVE + (1|Territorio), REML = F)
m5 <- lmer(data = databe, log(HubDist) ~ DISTANCIA : AVE + DISTANCIA + AVE + (1|Territorio), REML = F)
m5.1 <- lme(data = databe, log(HubDist) ~ DISTANCIA * AVE, random = list(Territorio=~1))

AICc(m0, m1, m2, m3, m4, m5)
# check_model(m5)
DHARMa::testResiduals(m5)
summary(m5.1)
car::Anova(m5.1, type = 3)

colnames(databe)[2] <- "Raptor species"
databe$DISTANCIA <- as.numeric(databe$DISTANCIA)
mutate(databe, DISTANCIA = ifelse(DISTANCIA == 1, 0,
                                  ifelse(DISTANCIA == 2, 500, 1000))) -> databe

databe %>% filter(densHA < 5) %>%
  dplyr::mutate(`Raptor species` = ifelse(`Raptor species` == "Abejero", "Honey-buzzard", "Goshawk")) %>% 
  ggplot()+
  geom_jitter(aes(x = DISTANCIA, y = log(HubDist), color = `Raptor species`), width = 5)+
  geom_smooth(aes(x = DISTANCIA, y = log(HubDist), color = `Raptor species`), show.legend = T, alpha = 0.2, method = "lm")+
  labs(x = "Distance from raptor nest (m)", y = "log(Distance to full-grwon hornet nests)")+
  scale_x_continuous(n.breaks = 3)+
  theme_minimal()