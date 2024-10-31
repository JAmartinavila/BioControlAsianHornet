# setwd("")
library(tidyverse)
library(nlme)
library(lme4)
library(performance)
library(sjPlot)
library(car)
library(ggeffects)
library(MuMIn)
read.csv("VelutinaAttacked.csv") -> attq

## add distance:

attq %>% mutate(atck_distance = sqrt((UTM_X-UTM.X.1)^2+(UTM_Y-UTM.Y.1)^2)) -> attq

attq %>% 
  ggplot()+
  geom_density(aes(x = atck_distance, color = bird_year, fill = bird_year), alpha = 0.2)

attq %>% filter(atck_distance < 10000) %>% 
  group_by(bird_year) %>% summarise(p90 = quantile(atck_distance, 0.9)) -> kk
mean(kk$p90)

## calculate julian date:


attq <- rbind(
  attq %>% 
    filter(Year == 2021) %>% 
    mutate(juliano = round(julian.Date(as.POSIXct(UTC_date), origin = as.POSIXct("2021-01-01"))/(24*3600),0)), 
  attq %>% 
    filter(Year == 2022) %>% 
    mutate(juliano = round(julian.Date(as.POSIXct(UTC_date), origin = as.POSIXct("2022-01-01"))/(24*3600),0))) -> attq

distan <- seq(500, 10000, 500)
densidades2 <- data.frame()
for (i in distan) {
  
  attq %>% 
    filter(atck_distance > i-500, atck_distance <= i) %>% 
    group_by(bird_year) %>% 
    summarise(atacount = length(atck_distance)) %>% 
    mutate(radius = i, atadens = atacount/((pi*i^2)-(pi*(i-500)^2))*10000) -> kk
  
  densidades2 <- rbind(densidades2, kk) 
  
}

densidades2 %>% 
  ggplot()+
  geom_point(aes(x=radius, y=atadens, color = bird_year))+
  geom_line(aes(x=radius, y=atadens, color = bird_year))

densidades2 %>%
  ggplot()+
  geom_point(aes(x=radius, y=log(atadens), color = bird_year))+
  geom_smooth(aes(x=radius, y=log(atadens), color = bird_year),
              alpha = 0,
              method = "lm",
              linewidth = 0.5)

densidades2 %>%
  ggplot()+
  geom_point(aes(x=log(radius), y=log(atadens), color = bird_year))+
  geom_smooth(aes(x=log(radius), y=log(atadens), color = bird_year),
              alpha = 0,
              method = "lm",
              linewidth = 0.5)

densidades2 %>% mutate(logdens = log(atadens),
                       logradius = log(radius)) -> densidades2

densidades2 <- as.data.frame(densidades2)

lmer(logdens ~ radius  + (1|bird_year), data = densidades2, REML = F) -> mlog
lmer(logdens ~ logradius  + (1|bird_year), data = densidades2, REML = F) -> mloglog
AICc(mlog, mloglog)
#Una log - normal
lmer(logdens ~ radius  + (1|bird_year), data = densidades2) -> mlog
lme(logdens ~ radius, data = densidades2, random = list(bird_year=~1)) -> emlog
summary(emlog)
# check_model(mlog)

plot_model(emlog, type = "eff", show.data = T)

as.data.frame(ggpredict(emlog, terms = c("radius[500:10000]"))) -> predi
predi %>% dplyr::rename(Distancianido = x, logDensidadAtcHA  = predicted) -> predi
predi %>% mutate(DensidadAtcHA = exp(logDensidadAtcHA), 
                 tconf.high = exp(conf.high),
                 tconf.low = exp(conf.low)) -> predi

predi %>% 
  ggplot()+
  geom_line(aes(x=Distancianido, y = DensidadAtcHA), linewidth = 1)+
  geom_ribbon(aes(x=Distancianido, ymax = tconf.high, ymin = tconf.low), 
              alpha = 0.2)+
  theme_minimal()+
  geom_jitter(data = densidades2, aes(x=radius, y=atadens))

# y = 0.01777886 e^-0.000641x

#Ahora una log-log
lmer(logdens ~ logradius  + (1|bird_year), data = densidades2, REML = F) -> mlog
lmer(logdens ~ 1 + (1|bird_year), data = densidades2, REML = F) -> mlognull
AICc(mlog, mlognull)
lme(logdens ~ logradius, data = densidades2, random = list(bird_year=~1)) -> emlog
summary(emlog)
# check_model(mlog)

plot_model(emlog, type = "eff", show.data = T)

as.data.frame(ggpredict(emlog), terms = c("logradius[6:9]")) -> predi
predi %>% dplyr::rename(logDistancianido = `logradius.x`, logDensidadAtcHA  = `logradius.predicted`) -> predi
predi %>% mutate(Radius = exp(logDistancianido),
                 DensidadAtcHA = exp(logDensidadAtcHA), 
                 tconf.high = exp(logradius.conf.high),
                 tconf.low = exp(logradius.conf.low)) -> predi

predi %>% 
  ggplot()+
  geom_line(aes(x=Radius, y = DensidadAtcHA), linewidth = 1)+
  geom_ribbon(aes(x=Radius, ymax = tconf.high, ymin = tconf.low), 
              alpha = 0.2)+
  theme_minimal()+
  xlim(150,10000)+
  geom_jitter(data = densidades2, aes(x=radius, y=atadens))

# No conseguimos que nos la pinte suave, podemos hacerlo a mano:
# y = ax^b
# y = 456.3659x^-1.552105 #Media
# = 974.5634^-1.65201 #+sd
# = 213.7058^-1.452201 #-sd

media <- function(x){
  print(456.3659*x^(-1.552105))
} 

upsd <- function(x){
  print(974.5634*x^(-1.65201))
} 

dowsd <- function(x){
  print(213.7058*x^(-1.452201))
} 

prediccion <- data.frame()
for (j in 400:10000) {
  prediccion <- rbind(prediccion,
                      c(j,  media(j), upsd(j), dowsd(j)))
}
colnames(prediccion) <- c("DistNido", "Densidad", "SDUP", "SDDOWN")

prediccion %>% 
  ggplot()+
  geom_jitter(data = densidades2, aes(x=radius, y=atadens), alpha = 0.4)+
  geom_line(aes(x=DistNido, y = Densidad), linewidth = 1)+
  # geom_ribbon(aes(x=DistNido, ymax = SDUP, ymin = SDDOWN),
  # alpha = 0.2)+
  labs(x = "Distance from honey-buzzard nest (m)", y = "No. of nests attacked / ha")+
  theme_minimal()

ggsave(device = "png", dpi = "retina", filename = "Density.png", units = "cm", width = 12, height = 12)


# Si ahora miramos las distribuciones de las distancias de ataque?

attq %>% 
  ggplot()+
  geom_density(aes(x = atck_distance), linewidth = 1)+
  geom_vline(xintercept = density(attq$atck_distance)$x[which.max(density(attq$atck_distance)$y)])+
  geom_label(label = "mode = 842.89m", x = 2000, y = 0.0003)+
  theme_minimal()

density(attq$atck_distance)$x[which.max(density(attq$atck_distance)$y)]

# Particionando la varianza por bird_year
attq %>% 
  ggplot()+
  geom_density(aes(x = atck_distance, color = bird_year), linewidth = 1)+
  theme_minimal()

# Podríamos representar la moda promedio de todos para tener una moda más real?


modas <- data.frame()
for (i in unique(attq$bird_year)) {
  
  modas <- rbind(modas, 
                 c(i, density(attq[attq$bird_year==i,]$atck_distance)$x[which.max(density(attq[attq$bird_year==i,]$atck_distance)$y)]))
  
}
colnames(modas) <- c("bird_year", "moda")
modas$moda <- as.numeric(modas$moda)
mean(modas$moda)

attq %>% 
  ggplot()+
  geom_density(aes(x = atck_distance, fill = bird_year), linewidth = .5, alpha = 0, color = "grey", show.legend = F)+
  scale_fill_discrete(guide="none")+
  geom_density(aes(x = atck_distance), linewidth = 1.5)+
  geom_vline(xintercept = mean(modas$moda), linewidth = 1, linetype = "dashed")+
  geom_vline(xintercept = density(attq$atck_distance)$x[which.max(density(attq$atck_distance)$y)], linewidth = 1)+
  labs(x="Distance from honey-buzzard nest (m)", fill = "Bird", y = "Kernel density")+
  xlim(0,10000)+
  theme_minimal()
