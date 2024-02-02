rm(list=ls())

library(tidyverse)
library(ggplot2)
library(WeightIt)
library(cobalt)
library(survey)
library(gbm)
library(interactions)
library(jtools)
library(ggstance)
library(broom)
library(stargazer)
library(readxl)
library(modelsummary)
library(httpuv)
library(flextable)
library(huxtable)

# Figure 1
library(geobr)
library(tidyverse)
library(ggspatial)
library(viridis)
library(extrafont) 
font_import()
y
loadfonts(device = "win")
library(SciViews)
library(RColorBrewer)
library(tidyr)
library(sf)
library(patchwork)

Brazil <- read_country(year = 2010) 

Brazil.Munic <-read_municipality(code_muni = "all", year=2010)

ggplot() +
  geom_sf(data=Brazil.Munic)+
  theme_void()

setwd("C:/Users/brusc/Desktop/Artigo_Bruna Sineta Scarpioni")
getwd()

base <- read_excel("Brazil_Database.xlsx", na = "-")

data_brazil <- Brazil.Munic %>% 
  left_join(base, by = c("code_muni"="city")) 

ggplot() +
  geom_sf(data=Brazil)+ # National borders
  geom_point(data=data_brazil, aes(x=long, y=lat)) + 
  # dots: 2010 Census municipalities (used for 2000 and 1872) 
  theme_void()

breaks <- c(10, 20, 30, 40, 57) 
labels <- c("less than 10%", "10% to 20%", 
            "20% to 30%", "30% to 40%", "40% to 57%") 

data_brazil %>%
  drop_na(sld) %>% 
  subset(sld!= "0") %>% # Exclude control group
  arrange(sld) %>% # Highest values on top (more visibility) 
  ggplot() +
  geom_sf(data=Brazil)+
  geom_point( aes(x=long, y=lat, 
                  size=sld, color=sld)) +
  scale_size_continuous(breaks = breaks, labels = labels) +
  scale_color_viridis(option="turbo",
                      breaks = breaks, labels = labels)+
  theme_void()+
  guides( colour = guide_legend()) +
  theme(
    legend.text = element_text(size=12, family = "Times New Roman"),
    legend.title = element_blank())

# Estimation

class (base)
base <- as.data.frame(base)
class (base)

summary(base)

base = base [c("homr","gini", "theil", "sld", "ypc", "pd", "unmp", 
               "be", "ym", "cps", "long", "lat", "state")]

base[["state"]] <- as.factor(base[["state"]])

str(base)

# Model 1

W1 <- weightit(sld ~ ypc + pd + unmp + be + ym + cps +
                 long + lat + state, data=base, method = "gbm", 
               criterion ="p.mean")

balance1 <- bal.tab(W1, stats = c("c"), thresholds = c(cor = .1))
balance1

d.w1 <- svydesign(~1, weights = W1$weights, data = base)

fit1 <- svyglm(homr ~ sld + ypc + pd + unmp + be + ym + cps +
                 long + lat + state, design = d.w1) 

summ(fit1, confint = TRUE ,digits = 3)

# Model 2

W2 <- weightit(sld ~ ypc + pd + unmp + be + ym + cps + 
                 long + lat + gini + state, 
               data=base, method = "gbm", criterion ="p.mean")

balance2 <- bal.tab(W2, stats = c("c"), thresholds = c(cor = .1))
balance2

d.w2 <- svydesign(~1, weights = W2$weights, data = base)

fit2 <- svyglm(homr ~ sld + ypc + pd + unmp + be + ym + cps + 
                 long + lat + gini + state, design = d.w2) 

summ(fit2, confint = TRUE , digits = 3)

# Model 3

W3 <- weightit(sld ~ ypc + pd + unmp + be + ym + cps +
                 long + lat + theil + state, data=base, 
               method = "gbm", criterion ="p.mean")

balance3 <- bal.tab(W3, stats = c("c"), thresholds = c(cor = .1))
balance3

d.w3 <- svydesign(~1, weights = W3$weights, data = base)

fit3 <- svyglm(homr ~ sld + ypc + pd + unmp + be + ym + cps + 
                 long + lat + theil + state, design = d.w3) 

summ(fit3, confint = TRUE ,digits = 3)

# Model 4

W4 <- weightit(sld ~ ypc + pd + unmp + be + ym + cps + 
                 long + lat + gini + ypc*gini + state, 
               data=base, method = "gbm", criterion ="p.mean")

balance4 <- bal.tab(W4, stats = c("c"), thresholds = c(cor = .1))
balance4

d.w4 <- svydesign(~1, weights = W4$weights, data = base)

fit4 <- svyglm(homr ~ sld + ypc + pd + unmp + be + ym + cps + 
                 long + lat + gini + ypc*gini + state,
               design = d.w4) 

summ(fit4, confint = TRUE ,digits = 3)

# Model 4 - Johnson-Neumann

probe_interaction(fit4, pred = gini, modx = ypc, pvals = FALSE,
                  confint = TRUE, interval = TRUE,  jnplot = TRUE)

# Model 5 

W5 <- weightit(sld ~ ypc + pd + unmp + be + ym + cps +
                 long + lat + theil + ypc*theil + state, 
               data=base, method = "gbm", criterion ="p.mean")

balance5 <- bal.tab(W5, stats = c("c"), thresholds = c(cor = .1))
balance5

d.w5 <- svydesign(~1, weights = W5$weights, data = base)

fit5 <- svyglm(homr ~ sld + ypc + pd + unmp + be + ym + cps + 
                 long + lat + theil + ypc*theil + state, 
               design = d.w5) 

summ(fit5, confint = TRUE ,digits = 3)

# Model 5 - Johnson-Neumann

probe_interaction(fit5, pred = theil, modx = ypc, pvals = FALSE,
                  confint = TRUE, interval = TRUE,  jnplot = TRUE)

# Keeping Johnson-Neumann output
sink ("Brazil_JN.txt")
sim_slopes(fit4, pred = gini, modx = ypc, pvals = FALSE,
           confint = TRUE, interval = TRUE)
sim_slopes(fit5, pred = theil, modx = ypc, pvals = FALSE,
           confint = TRUE, interval = TRUE)
sink()

# Keeping balance tables

sink("Brazil_Balance.csv")
balance1
balance2
balance3
balance4
balance5
sink()

# Comparing Model Results 

brazil <- list(fit1, fit2, fit3, fit4, fit5)

modelsummary(brazil, stars = TRUE)
modelsummary(brazil, output = "latex")

# Omitting state dummies for a better visual

stargazer(brazil,
          type = "html",
          out = "Brazil_3Star.html",
          dep.var.labels=c("(1)","(2)", "(3)", "(4)", "(5)"),
          omit = "state", keep.stat= "n", 
          ci=TRUE, ci.level=0.95,
          align = TRUE)

stargazer(brazil,
          type = "text",
          out = "Brazil_4Star.txt",
          dep.var.labels=c("(1)","(2)", "(3)", "(4)", "(5)"),
          align = TRUE)

stargazer(brazil,
          type = "latex",
          out = "Brazil_Star_Latex.html",
          dep.var.labels=c("(1)","(2)", "(3)", "(4)", "(5)"),
          omit = "state", keep.stat= "n", 
          ci=TRUE, ci.level=0.95,
          align = TRUE)

