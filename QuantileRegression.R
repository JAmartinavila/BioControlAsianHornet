# setwd("")
library(tidyverse)
library(readxl)
library(nlme)
library(lme4)
library(performance)
library(sjPlot)
library(car)
library(ggeffects)
library(quantreg)
library(lqmm)
library(patchwork)
read.csv("VelutinaAttacked.csv") -> datvl

datvl %>% 
  ggplot()+
  geom_point(aes(x = dias_desde, y = atck_distance))+
  geom_smooth(aes(x = dias_desde, y = atck_distance, color = bird_year), method = "lm", se = F)

plot(data = datvl,
     atck_distance ~ dias_desde)

pred <- data.frame()
for (i in c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  
  modelo <- rq(data = datvl,
               atck_distance ~ poly(dias_desde, 1), tau = i)
  
  as.data.frame(ggpredict(modelo, terms = "dias_desde[all]")) -> kk
  colnames(kk)[5] <- "quantil"
  kk[,5] <- i
  pred <- rbind(pred, kk)
  
}
ggplot()+
  geom_point(data = datvl, aes(x = dias_desde, y = atck_distance))+
  geom_line(data = pred, aes(x = `x`, y = predicted, color = as.factor(quantil)), show.legend = F)+
  geom_ribbon(data = pred, aes(x = `x`, ymax = conf.high, ymin = conf.low, fill = as.factor(quantil)), alpha = 0.2)+
  geom_smooth(data = datvl, aes(x = dias_desde, y = atck_distance), method = "lm", color = "white", fill = "black", alpha = 0.5)+
  scale_fill_discrete(name = "Quantile")+
  labs(y = "Attack distance (m)", x = "Day of attack since hatching", title = "Attacks ")+
  theme_minimal()

# summary(lm(data = datvl, atck_distance ~ dias_desde))

# summary(modelo, se = "nid")

# Y CON EFECTOS ALEATORIOS?? ----

# Copio aquí el ejemplo
# set.seed(123)
# 
# M <- 50
# n <- 10
# test <- data.frame(pred = runif(n*M,0,1), group = rep(1:M,each=n))
# test$resp <- 10*test$pred + rep(rnorm(M, 0, 2), each = n) + rchisq(n*M, 3)
# test %>% filter(group < 4) %>%  # Voy a graficarme un poquito para entenderlo
#   ggplot()+
#   geom_point(aes(x = `pred`, y = `resp`, color = as.factor(group)))+
#   geom_quantile(aes(x = `pred`, y = `resp`, color = as.factor(group)))
# 
# 
# fit.lqmm <- lqmm(fixed = resp ~ pred, random = ~ 1, group = group,	
#                  data = test, tau = c(0.05, 0.5, 0.95), nK = 11, type = "normal")
# fit.lqmm
# summary(fit.lqmm)
# plot(resp~pred,data=test)
# curve( coef(fit.lqmm)[1,3]  +coef(fit.lqmm)[2,3]*(x), add = TRUE)


####
lqmmControl(method = "gs", LP_tol_ll = 1e-5, LP_tol_theta = 1e-5,
            check_theta = FALSE, LP_step = NULL, beta = 0.5, gamma = 1,
            reset_step = FALSE, LP_max_iter = 1000, UP_tol = 1e-4,
            UP_max_iter = 100, startQR = FALSE, verbose = FALSE)

fit.lqmm <- lqmm(fixed = atck_distance ~ dias_desde, random = ~ 1, group = bird_year,
                 data = datvl, tau = c(.05, .25, .5, .75, .95), type = "normal")
# summary(fit.lqmm)
fit.lqmm
quantreg <- data.frame(Intercept = coef(fit.lqmm)[1,],
                       Slope = coef(fit.lqmm)[2,])
quantreg$Quant <- rownames(quantreg)

# plot(atck_distance~dias_desde,data=datvl)
# curve( coef(fit.lqmm)[1]  +coef(fit.lqmm)[2]*(x), add = TRUE)

mixed <- lmer(atck_distance ~ dias_desde + (1|bird_year),
              data = datvl)
prediction <- ggpredict(mixed, terms = "dias_desde[0:75]")

set_theme(theme_minimal())
ggplot()+
  scale_color_brewer(palette = "Paired")+
  geom_ribbon(data = prediction, aes(x = `x`, ymin = conf.low, ymax = conf.high), color = "grey", alpha = 0.3)+
  geom_point(data = datvl, aes(x = dias_desde, y = atck_distance), alpha = 0.4, size = 2)+
  geom_abline(slope = quantreg$Slope[1], intercept = quantreg$Intercept[1], color = "darkslategray3", linewidth = 1)+
  geom_abline(slope = quantreg$Slope[2], intercept = quantreg$Intercept[2], color = "dodgerblue3", linewidth = 1)+
  geom_abline(slope = quantreg$Slope[3], intercept = quantreg$Intercept[3], color = "#000000", linewidth = 1)+
  geom_abline(slope = quantreg$Slope[4], intercept = quantreg$Intercept[4], color = "chartreuse4", linewidth = 1)+
  geom_abline(slope = quantreg$Slope[5], intercept = quantreg$Intercept[5], color = "olivedrab2", linewidth = 1)+
  geom_line(data = prediction, aes(x = `x`, y = predicted), color = "white", linewidth = 1)+
  xlim(c(10,70))+
  labs(x = "Days since hatching", y ="Distance from nest (m)", title = "Asian-hornet colonies attacked")

ggsave(device = "png", dpi = "retina", filename = "quantile2.png", units = "cm", width = 12, height = 10)

ggplot()+
  scale_color_brewer(palette = "Paired")+
  geom_ribbon(data = prediction, aes(x = `x`, ymin = conf.low, ymax = conf.high), color = "grey", alpha = 0.3)+
  geom_point(data = datvl, aes(x = dias_desde, y = atck_distance), alpha = 0.4, size = 2)+
  # geom_abline(slope = quantreg$Slope[1], intercept = quantreg$Intercept[1], color = "black", linewidth = 1)+
  # geom_abline(slope = quantreg$Slope[2], intercept = quantreg$Intercept[2], color = "black", linewidth = 1)+
  # geom_abline(slope = quantreg$Slope[3], intercept = quantreg$Intercept[3], color = "black", linewidth = 1)+
  # geom_abline(slope = quantreg$Slope[4], intercept = quantreg$Intercept[4], color = "black", linewidth = 1)+
  geom_abline(slope = quantreg$Slope[5], intercept = quantreg$Intercept[5], color = "olivedrab2", linewidth = 1)+
  geom_line(data = prediction, aes(x = `x`, y = predicted), color = "white", linewidth = 1)+
  xlim(c(10,70))+
  labs(x = "Days since hatching", y ="Distance from honey-buzzard nest (m)", title = "Asian-hornet colonies attacked")
