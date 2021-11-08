### The purpose of this script is to regress mean HI at each site per year against burn extent (percentage burned)
  # and against permafrost extent (% coverage OR soil temperature profile)
# Input: HI.dat 
# Step 1) import HI.dat file which is HI for individual storms in 2018-2021 across DoD sites
# Step 2) Calculate mean HI at each site for each year
# Step 3) run a linear model of mean HI against burn extent 
# Step 4) run a linear model of mean HI against permafrost extent
# Output: linear model plot 

# % of most recent burn # 
# Poker Creek 33%
# Vault Creek: NA
# French Creek: 7.3% 
# Moose Creek: 65.7%
# Stuart Creek: 67.4%

library(here)
library(tidyverse)
library(boot)
library(broom)
library(purrr)
library(viridis)
library(readr)
library(tidyverse)
library(lubridate)
library(data.table)
library(rio)
library(ggplot2)
library(scales)
library(psych)
library(here)
library(googledrive)
library(readxl)
library(cowplot)
library(zoo)
library(readr)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(ggpmisc)

# Import data #
HI.dat <- read.csv("Output_from_analysis/HI.dat.csv")

# subset data by site and year # 
FRCH.HI.2018.NO3 <- subset(HI.dat, site.ID == "FRCH" & year == "2018" & response == "NO3")
MOOS.HI.2018.NO3 <- subset(HI.dat, site.ID == "MOOS" & year == "2018" & response == "NO3")
FRCH.HI.2018.fDOM <- subset(HI.dat, site.ID == "FRCH" & year == "2018" & response == "fDOM")
MOOS.HI.2018.fDOM <- subset(HI.dat, site.ID == "MOOS" & year == "2018" & response == "fDOM")

FRCH.HI.2019.NO3 <- subset(HI.dat, site.ID == "FRCH" & year == "2019" & response == "NO3")
FRCH.HI.2019.fDOM <- subset(HI.dat, site.ID == "FRCH" & year == "2019" & response == "fDOM")

STRT.HI.2019 <- subset(HI.dat, site.ID == "STRT" & year == "2019")
POKE.HI.2019 <- subset(HI.dat, site.ID == "POKE" & year == "2019")
VAUL.HI.2019 <- subset(HI.dat, site.ID == "VAUL" & year == "2019")
MOOS.HI.2019 <- subset(HI.dat, site.ID == "MOOS" & year == "2019")

FRCH.HI.2020.NO3 <- subset(HI.dat, site.ID == "FRCH" & year == "2020" & response == "NO3")
FRCH.HI.2020.fDOM <- subset(HI.dat, site.ID == "FRCH" & year == "2020" & response == "fDOM")

STRT.HI.2020 <- subset(HI.dat, site.ID == "STRT" & year == "2020")
POKE.HI.2020 <- subset(HI.dat, site.ID == "POKE" & year == "2020")
VAUL.HI.2020 <- subset(HI.dat, site.ID == "VAUL" & year == "2020")
MOOS.HI.2020 <- subset(HI.dat, site.ID == "MOOS" & year == "2020")

FRCH.HI.2021.NO3 <- subset(HI.dat, site.ID == "FRCH" & year == "2021" & response == "NO3")
FRCH.HI.2021.fDOM <- subset(HI.dat, site.ID == "FRCH" & year == "2021" & response == "fDOM")

STRT.HI.2021 <- subset(HI.dat, site.ID == "STRT" & year == "2021")
POKE.HI.2021 <- subset(HI.dat, site.ID == "POKE" & year == "2021")
VAUL.HI.2021 <- subset(HI.dat, site.ID == "VAUL" & year == "2021")
MOOS.HI.2021 <- subset(HI.dat, site.ID == "MOOS" & year == "2021")

# Calculate the mean of HI across each site # 
# 2018 # 
FRCH.2018.no3.mean <- mean(FRCH.HI.2018.NO3$HI) %>% as.data.frame()
names(FRCH.2018.no3.mean) <- "HI"
FRCH.2018.no3.mean$Site <- "FRCH"
FRCH.2018.no3.mean$Response <- "NO3"
# fDOM #
FRCH.2018.fDOM.mean <- mean(FRCH.HI.2018.fDOM$HI) %>% as.data.frame()
FRCH.2018.fDOM.mean$Fire <- "0.073"
FRCH.2018.fDOM.mean$Fire <- as.numeric(FRCH.2018.fDOM.mean$Fire)
names(FRCH.2018.fDOM.mean) <- "HI"
FRCH.2018.fDOM.mean$Site <- "FRCH"
FRCH.2018.fDOM.mean$Response <- "fDOM"
FRCH.2018.fDOM.mean$year <- "2018"

# merged # 
FRCH.2018$Fire <- "0.073"
FRCH.2018$Fire <- as.numeric(FRCH.2018$Fire)

MOOS.2018.no3.mean <- mean(MOOS.HI.2018.NO3$HI) %>% as.data.frame()
names(MOOS.2018.no3.mean) <- "HI"
MOOS.2018.no3.mean$Site <- "MOOS"
MOOS.2018.no3.mean$Response <- "NO3"

MOOS.2018.fDOM.mean <- mean(MOOS.HI.2018.fDOM$HI) %>% as.data.frame()
names(MOOS.2018.fDOM.mean) <- "HI"
MOOS.2018.fDOM.mean$Site <- "MOOS"
MOOS.2018.fDOM.mean$Response <- "fDOM"

FRCH.2018 <- rbind(FRCH.2018.fDOM.mean, FRCH.2018.no3.mean)
MOOS.2018 <- rbind(MOOS.2018.fDOM.mean, MOOS.2018.no3.mean)
HI.2018 <- rbind(FRCH.2018, MOOS.2018)

# 2019 # 
# fDOM #
FRCH.2019.fDOM.mean <- mean(FRCH.HI.2019.fDOM$HI) %>% as.data.frame()
names(FRCH.2019.fDOM.mean) <- "HI"
FRCH.2019.fDOM.mean$Fire <- "0.073"
FRCH.2019.fDOM.mean$Fire <- as.numeric(FRCH.2019.fDOM.mean$Fire)
FRCH.2019.fDOM.mean$Site <- "FRCH"
FRCH.2019.fDOM.mean$Response <- "fDOM"
FRCH.2019.fDOM.mean$year <- "2019"


French.lm <- lm(FRCH.2018$HI ~ FRCH.2018$Fire) 
frch.formula <- y ~ x

frc.1 <- ggplot(aes(x = Fire, y = HI), data = FRCH.2018) +
  geom_point(aes(color = HI), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_classic() +
  ggtitle("French1 all measured Q") 

frc.1
