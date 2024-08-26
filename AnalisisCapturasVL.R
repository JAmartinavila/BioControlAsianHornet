setwd("C:/Users/ja12m/OneDrive - Universidad de Alcala/TesisNube/Proyecto/Paper 4 Efecto/Analisis")

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

# Reanalizamos las capturas de avispas centrándonos en Velutina:

datos <- read.csv("datoscapturas20-22.csv")
datos$Periodo <- as.factor(datos$Periodo)

# sd(datos[datos$Ave=="Azor"&datos$Periodo==4,]$TCVL) #media de capturas de azor4

# AVES POR SEPARADO ----

## Abejero: ----
abejeros <- datos %>% filter(Ave == "Abejero")
abejeros <- as.data.frame(abejeros) # Corro esto porque necesito tener data.frame para predict

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

# check_model(m4) # Los modelos son buenos
AICc(m0, m1, m2, m3, m4) #Selecciona el m2 o el m3
#Qué estructura de random sería mejor?
# lmer(data = abejeros, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1|Nombre_Nido), REML = T) -> m2 #Esta
# lmer(data = abejeros, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1+Distancia_Trampa|Nombre_Nido), REML = T) -> m21
# lmer(data = abejeros, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (0+Distancia_Trampa|Nombre_Nido), REML = T) -> m22
# AICc(m2, m21, m22)
#Nos olvidamos
Anova(m3, type = 3)
# Interacción sign.
# La presencia de la interacción es significativa
anova(update(m2, REML = F), update(m3, REML = F))
lrtest(update(m2, REML=F), update(m3, REML=F)) # El 3 es signif. mejor
 
lmer(data = abejeros, 
     formula = TCVL ~ Distancia_Trampa : Periodo + (1|Nombre_Nido), REML = T) -> m3
# check_model(m3) # perfecto
# Está modelizando la variación en el intercepto por la pendiente unicamente

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
  theme_minimal() -> A

## Azor: ----
azores <- datos %>% filter(Ave == "Azor")
azores <- as.data.frame(azores) # Corro esto porque necesito tener data.frame para predict

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

# check_model(m4) # Los modelos son buenos
AICc(m0, m1, m2, m3, m4) #El 3 o el 2
#Qué estructura de random sería mejor?
# lmer(data = azores, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1|Nombre_Nido), REML = T) -> m4 #Esta
# lmer(data = azores, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (1+Distancia_Trampa|Nombre_Nido), REML = T) -> m41
# lmer(data = azores, 
#      formula = TCVL ~ Distancia_Trampa + Periodo + (0+Distancia_Trampa|Nombre_Nido), REML = T) -> m42
# AICc(m4, m41, m42)
#Nos olvidamos
Anova(m3, type = 3) #Sig
# La presencia de la interacción es significativa
anova(update(m2, REML = F), update(m3, REML = F))
lrtest(update(m2, REML=F), update(m3, REML=F)) # El 3 es mejor
 
lmer(data = azores, 
     formula = TCVL ~ Distancia_Trampa : Periodo + (1|Nombre_Nido), REML = T) -> m3
# check_model(m3) # perfecto
# Está modelizando la variación en el intercepto

lme(data = na.omit(azores), TCVL ~ Distancia_Trampa : Periodo,
    random = list(Nombre_Nido=~1)) -> me3
summary(me3) #Nada es significativo como esperaba

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
  theme_minimal() -> B

A + B
ggsave(device = "png", dpi = "retina", filename = "abazo.png", units = "cm", width = 22, height = 12)

# Periodo 4 ambas aves ----
# En este periodo quizá solo el intercepto es significativo.
datos %>% filter(Periodo == 4) -> percua
percua <- as.data.frame(percua)

lmer(data = percua, 
     formula = TCVL ~ 1 + (1|Nombre_Nido), REML = F) -> m0

lmer(data = percua,
     formula = TCVL ~ Distancia_Trampa + (1|Nombre_Nido), REML = F) -> m1

lmer(data = percua, 
     formula = TCVL ~ Ave + (1|Nombre_Nido), REML = F) -> m1.1

lmer(data = percua, 
     formula = TCVL ~ Distancia_Trampa + Ave + (1|Nombre_Nido), REML = F) -> m2

lmer(data = percua, 
     formula = TCVL ~ Distancia_Trampa : Ave + (1|Nombre_Nido), REML = F) -> m3

lmer(data = percua, 
     formula = TCVL ~ Distancia_Trampa * Ave + (1|Nombre_Nido), REML = F) -> m4

# check_model(m4) # Los modelos son buenos
AICc(m0, m1, m1.1, m2, m3, m4) #Selecciona el NULO
#Qué estructura de random sería mejor?
# lmer(data = percua, 
#      formula = TCVL ~ Distancia_Trampa * Periodo + (1|Nombre_Nido), REML = T) -> m4 #Esta
# lmer(data = percua, 
#      formula = TCVL ~ Distancia_Trampa * Periodo + (1+Distancia_Trampa|Nombre_Nido), REML = T) -> m41
# lmer(data = percua, 
#      formula = TCVL ~ Distancia_Trampa * Periodo + (0+Distancia_Trampa|Nombre_Nido), REML = T) -> m42
# AICc(m4, m41, m42)
#Nos olvidamos

datos %>% filter(Periodo == 4, Distancia_Trampa == 0) -> percua
percua <- as.data.frame(percua)

lm(data = percua, 
     formula = TCVL ~ 1)  -> m0

lm(data = percua,
     formula = TCVL ~ Ave) -> m1


summary(m0)
summary(m1)

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
       subtitle =  "4th period, 0m from nest",
       y ="", x = "Raptor species")

ggsave(device = "png", dpi = "retina", filename = "mean.png", units = "cm", width = 8, height = 15)





