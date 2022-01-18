### The purpose of this script is to model HI/FI against fixed and random effects and perform model selection techniques to see which model is best fit

# Input: 2019_2020 HI for each catchment that is will be from Output_from_analysis->07_Models
# Step 1) import HI.dat file which is HI for individual storms in 2018-2021 across DoD sites
# Step 2) standardize and center the data
# Step 3) add fixed (storm total, intensity, week precip, month precip, 3month precip and doy) and random effects (catchments)
# Output: linear model plot 


# import libraries # 
library(here)
library(tidyverse)
library(boot)
library(broom)
library(purrr)
library(viridis)
library(readr)
library(lubridate)
library(data.table)
library(rio)
library(ggplot2)
library(scales)
library(lme4)
library(MARSS)
library(nlme)
library(datasets)
library(tidyverse)
library(dplyr)
library(MASS)
library(stats)
library(visreg)
library(coefplot)
library(datasets)
library(ggthemes)
library(sjPlot)
library(GGally)
library(car)
install.packages("AED")
vif

# read in data #
HI.2019.2020 <- read.csv("Output_from_analysis/07_Models/HI.2019.2020.csv") # has the 50 values for each storm
HI.all <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.all.csv") # has the mean for HI for each storm

#scaling # 
HI.all[c(7:10, 12)] <- lapply(HI.all[c(7:10, 12)], function(x) c(scale(x)))
pairs(HI.all[c(7:10, 12)])
f1 <- formula(HI ~ precip + precip.week + precip.month + ThreeMonth + Intensity + doy)
M0 <- gls(f1, data = HI.all)
summary(M0)
plot(M0)
E <- resid(M0, type = "normalized")
hist(E)

plot(x=HI.all$HI, y=M0$fitted, 
     type='p', pch=21, bg=rgb(1,0,0, alpha=0.25),
     xlab="Observed", ylab="Predicted",
     main="LM")
abline(a=0, b=1, col='blue', lwd=2, lty=2)
AIC(M0) # 551.468

mod.1 <- lm(HI ~ precip + precip.week + precip.month + ThreeMonth + Intensity + doy, data = HI.all)
step(mod.1)

mod.2 <- lme(HI ~ precip + precip.week + precip.month + ThreeMonth + Intensity + doy, data = HI.all, random=~1|site.ID)
summary(mod.2)
AIC(mod.2)# 552.0757
plot(mod.2)
plot(ACF(mod.2))

mod.3 <- update(mod.2, correlation = corAR1())
anova(mod.2, mod.3)
mod.4 <- update(mod.2, correlation = corARMA(q = 2))
mod.4
anova(mod.3, mod.4)
mod.5 <- update(mod.2, corr = corARMA(p = 1, q = 1))
anova(mod.3,  mod.5)
plot(ACF(mod.5))




