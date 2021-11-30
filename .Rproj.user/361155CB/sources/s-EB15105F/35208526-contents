### The purpose of this script is to model HI/FI against fixed and random effects and perform model selection techniques to see which model is best fit

# Input: HI.MOOS.csv as of 11/29/21 because all i have is moos rn 
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




# read in data #
HI.MOOS <- read.csv("Output_from_analysis/07_Models/HI.MOOS.csv")
HI.moos.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2019.csv") # just mean values of HI 
HI.moos.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2020.csv")# just mean values of HI 
HI.moos <- rbind(HI.moos.2019, HI.moos.2020)

# clean unnecesary columns 
HI.MOOS <- HI.MOOS[,-c(2,3,12,17,18,20,21,23,24)]
HI.moos <- HI.moos[,-c(11)]
HI.moos$doy[4] <- 213
HI.moos$doy[5] <- 217
HI.moos$doy[6] <- 225
HI.moos$doy[7] <- 226
HI.moos$doy[8] <- 227


HI.MOOS.2 <- left_join(HI.moos, HI.MOOS, by = "storm.num")
# scaling storm total, intensity, week precip, month precip and 3 month precip #
HI.MOOS[c(6:10)] <- lapply(HI.MOOS[c(6:10)], function(x) c(scale(x)))
HI.moos[c(7:11)] <- lapply(HI.moos[c(7:11)], function(x) c(scale(x)))

pairs(HI.MOOS[c(6:10)])
pairs(HI.moos[c(7:11)])

mod.1 <- gls(HI ~ precip, HI.moos)
mod.2 <- gls(HI ~ precip + precip.week, HI.moos)
mod.3 <- gls(HI ~ precip + precip.week + precip.month, HI.moos)
mod.4 <- gls(HI ~ precip + precip.week + precip.month + ThreeMonth, HI.moos)
mod.5 <- gls(HI ~ precip + precip.week + precip.month + ThreeMonth + Intensity, HI.moos)
mod.6 <- gls(HI ~ precip + precip.week + precip.month + ThreeMonth + Intensity + doy, HI.moos)

summary(mod.1)
summary(mod.2)
summary(mod.3)
summary(mod.4)
summary(mod.5)
summary(mod.6)








