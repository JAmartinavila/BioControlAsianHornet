# setwd("")

library(lme4)
library(nlme)
library(readxl)
library(tidyverse)
library(ggeffects)
library(performance)
library(sjPlot)
library(car)
library(patchwork)
library(MuMIn)
library(lmtest)

datos <- read.csv("WorkersAbundance.csv")
datos$Periodo <- as.factor(datos$Periodo)
## Honey Buzzard (Abejero): ----
abejeros <- datos %>% filter(Ave == "Abejero")
abejeros <- as.data.frame(abejeros) 

lmer(data = abejeros, 
     formula = TCVL ~ 1 + (1|Nombre_Nido), REML = F) -> m0

lmer(data = abejeros, 
     formula = TCVL ~ Distancia_Trampa + (1|Nombre_Nido), REML = F) -> m1

lmer(data = abejeros, 
     formula = TCVL ~ Distancia_Trampa + Periodo + (1|Nombre_Nido), REML = F) -> m2

lmer(data = abejeros, 
     formula = TCVL ~ Distancia_Trampa : Periodo + (1|Nombre_Nido), REML = F) -> m3

lmer(data = abejeros, 
     formula = TCVL ~ Distancia_Trampa * Periodo + (1|Nombre_Nido), REML = F) -> m4

# check_model(m4) 
AICc(m0, m1, m2, m3, m4) #Selecciona el m2 o el m3

# lmer(data = abejeros, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1|Nombre_Nido), REML = T) -> m2 #Esta
# lmer(data = abejeros, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1+Distancia_Trampa|Nombre_Nido), REML = T) -> m21
# lmer(data = abejeros, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (0+Distancia_Trampa|Nombre_Nido), REML = T) -> m22
# AICc(m2, m21, m22)

Anova(m3, type = 3)
# Interacction sign.
# La presencia de la interacción es significativa
anova(update(m2, REML = F), update(m3, REML = F))
lrtest(update(m2, REML=F), update(m3, REML=F)) # m3 is better

lmer(data = abejeros, 
     formula = TCVL ~ Distancia_Trampa : Periodo + (1|Nombre_Nido), REML = T) -> m3
# check_model(m3) # perfecto

lme(data = abejeros, TCVL ~ Distancia_Trampa : Periodo,
    random = list(Nombre_Nido=~1), method = "ML") -> me3
summary(me3)

as.data.frame(ggpredict(me3, terms = c("Distancia_Trampa[0:1000]","Periodo"))) -> prediabe
prediabe %>% dplyr::rename(Distancia = `x`, TCVL = predicted, Periodo = group) -> prediabe

set_theme(theme_minimal())
ggplot()+
  geom_line(data = prediabe, aes(x=Distancia, y = TCVL, color = Periodo), linewidth = 1.2, show.legend = F)+
  geom_jitter(data = abejeros, aes(x=Distancia_Trampa, y = TCVL, color = Periodo), 
              width = 25, alpha = 0.3, show.legend = F)+
  scale_color_brewer(palette = "Paired")+
  ylim(0,1)+
  scale_x_continuous(breaks = c(0, 500, 1000))+
  labs(title = "Honey-buzzard", y = "Workers captured / hour", x = "Distance from nest (m)")+
  theme_minimal()

## Goshawk (Azor): ----
azores <- datos %>% filter(Ave == "Azor")
azores <- as.data.frame(azores) 

lmer(data = azores, 
     formula = TCVL ~ 1 + (1|Nombre_Nido), REML = F) -> m0

lmer(data = azores, 
     formula = TCVL ~ Distancia_Trampa + (1|Nombre_Nido), REML = F) -> m1

lmer(data = azores, 
     formula = TCVL ~ Distancia_Trampa + Periodo + (1|Nombre_Nido), REML = F) -> m2

lmer(data = azores, 
     formula = TCVL ~ Distancia_Trampa : Periodo + (1|Nombre_Nido), REML = F) -> m3

lmer(data = azores, 
     formula = TCVL ~ Distancia_Trampa * Periodo + (1|Nombre_Nido), REML = F) -> m4

# check_model(m4) #
AICc(m0, m1, m2, m3, m4) #El 3 o el 2

# lmer(data = azores, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1|Nombre_Nido), REML = T) -> m4 #Esta
# lmer(data = azores, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1+Distancia_Trampa|Nombre_Nido), REML = T) -> m41
# lmer(data = azores, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (0+Distancia_Trampa|Nombre_Nido), REML = T) -> m42
# AICc(m4, m41, m42)

Anova(m3, type = 3) #Sig

anova(update(m2, REML = F), update(m3, REML = F))
lrtest(update(m2, REML=F), update(m3, REML=F)) # m3 is better

lmer(data = azores, 
     formula = TCVL ~ Distancia_Trampa : Periodo + (1|Nombre_Nido), REML = T) -> m3
# check_model(m3) 

lme(data = na.omit(azores), TCVL ~ Distancia_Trampa : Periodo,
    random = list(Nombre_Nido=~1)) -> me3
summary(me3) 

as.data.frame(ggpredict(me3, terms = c("Distancia_Trampa[0:1000]","Periodo"))) -> prediazo
prediazo %>% dplyr::rename(Distancia = `x`, TCVL = predicted, Periodo = group) -> prediazo

set_theme(theme_minimal())
ggplot()+
  geom_line(data = prediazo, aes(x=Distancia, y = TCVL, color = Periodo), linewidth = 1.2, show.legend = F)+
  geom_jitter(data = azores, aes(x=Distancia_Trampa, y = TCVL, color = Periodo), 
              width = 25, alpha = 0.3, show.legend = F)+
  scale_color_brewer(palette = "Paired")+
  ylim(0,1)+
  scale_x_continuous(breaks = c(0, 500, 1000))+
  labs(title = "Goshawk", y = "", x = "Distance from nest (m)")+
  theme_minimal()

# Both raptor species, distance to nest = 0m
datos %>% filter(Distancia_Trampa == 0) -> percua2
percua2 <- as.data.frame(percua2)

lmer(data = percua2, 
     formula = TCVL ~ 1 + (1|Nombre_Nido))  -> m0

lme(data = percua2, TCVL ~ 1,
    random = list(Nombre_Nido=~1), method = "ML") -> m0.1


lmer(data = percua2, 
     formula = TCVL ~ Ave + (1|Nombre_Nido))  -> m1

lme(data = percua2, TCVL ~ Ave,
    random = list(Nombre_Nido=~1), method = "ML") -> m1.1

summary(m1.1)

AICc(m0, m1)


as.data.frame(ggpredict(m1, terms = c("Ave"))) -> pref
pref %>% dplyr::rename(Ave = x, TCVL = predicted) -> pref
levels(pref$Ave) <- c("Honey-buzzard", "Goshawk")


ggplot()+
  geom_errorbar(data = pref, aes(x = Ave, ymin = TCVL - std.error, ymax = TCVL + std.error), 
                width = .05, color = "chartreuse4")+
  geom_point(data = pref, aes(x=Ave, y = TCVL), 
             color = "chartreuse4", size = 4)+
  ylim(.2,.6)+
  labs(title = "Mean effect between raptors", 
       subtitle =  "0m from nest",
       y ="", x = "Raptor species")
