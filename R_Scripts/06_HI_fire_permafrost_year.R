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

# Permafrost extent # 
# Poker Creek Low
# Vault Creek: High- continuous (100%)
# French Creek: Medium
# Moose Creek: Medium
# Stuart Creek: High

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
library(SLOPE)

library(ggpubr)

# Import data #
HI.dat <- read.csv("~/Documents/Storms/Output_from_analysis/HI.dat.csv")


HI.mean<- HI.dat %>% group_by(site.ID, response, year) %>%  
  summarise_at(vars(HI), list(HI = mean)) # take mean by site response and year 

# merged # 
# By site and response
FRCH.fDOM <- subset(HI.mean, site.ID == "FRCH" & response == "fDOM")
POKE.fDOM <- subset(HI.mean, site.ID == "POKE" & response == "fDOM")
MOOS.fDOM <- subset(HI.mean, site.ID == "MOOS" & response == "fDOM")
STRT.fDOM <- subset(HI.mean, site.ID == "STRT" & response == "fDOM")
VAUL.fDOM <- subset(HI.mean, site.ID == "VAUL" & response == "fDOM")

FRCH.NO3 <- subset(HI.mean, site.ID == "FRCH" & response == "NO3")
POKE.NO3 <- subset(HI.mean, site.ID == "POKE" & response == "NO3")
MOOS.NO3 <- subset(HI.mean, site.ID == "MOOS" & response == "NO3")
STRT.NO3 <- subset(HI.mean, site.ID == "STRT" & response == "NO3")
VAUL.NO3 <- subset(HI.mean, site.ID == "VAUL" & response == "NO3")

FRCH.fDOM$burn <- "unburned"

POKE.fDOM$burn <- "burned"

MOOS.fDOM$burn <- "burned"

STRT.fDOM$burn <- "burned"

VAUL.fDOM$burn <- "unburned"

FRCH.NO3$burn <- "unburned"

POKE.NO3$burn <- "burned"

MOOS.NO3$burn <- "burned"

STRT.NO3$burn <- "burned"

VAUL.NO3$burn <- "unburned"

fdom.hi <- rbind(FRCH.fDOM, POKE.fDOM, MOOS.fDOM, STRT.fDOM, VAUL.fDOM)
no3.hi <- rbind(FRCH.NO3, POKE.NO3, MOOS.NO3, STRT.NO3, VAUL.NO3)

fdom.hi$year <- as.character(fdom.hi$year)
no3.hi$year <- as.character(no3.hi$year)


fdom.lm <- lm(fdom.hi$HI ~ fdom.hi$burn)
no3.lm <- lm(no3.hi$HI ~ no3.hi$burn)

fdom.hi %>%
  ggplot(aes(x=burn, 
             y=HI, 
             color=year))+
  geom_point() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("DOC") +
  xlab("Catchment burned (%)") +
  ylab("HI-Solute Storage")

no3.hi %>%
  ggplot(aes(x=burn, 
             y=HI, 
             color=year))+
  geom_point() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("NO3") +
  xlab("Catchment burned (%)") +
  ylab("HI-Solute Storage")

# Permafrost #
FRCH.fDOM$pf <- "medium"

POKE.fDOM$pf <- "medium"

MOOS.fDOM$pf <- "medium"

STRT.fDOM$pf <- "high"

VAUL.fDOM$pf <- "continuous"

FRCH.NO3$pf <- "medium"

POKE.NO3$pf <- "medium"

MOOS.NO3$pf <- "medium"

STRT.NO3$pf <- "high"

VAUL.NO3$pf <- "continuous"


pf.fdom.hi <- rbind(FRCH.fDOM, POKE.fDOM, MOOS.fDOM, STRT.fDOM, VAUL.fDOM)
pf.no3.hi <- rbind(FRCH.NO3, POKE.NO3, MOOS.NO3, STRT.NO3, VAUL.NO3)

pf.fdom.hi$year <- as.character(pf.fdom.hi$year)
pf.no3.hi$year <- as.character(pf.no3.hi$year)


pf.fdom.lm <- lm(pf.fdom.hi$HI ~ pf.fdom.hi$burn)
pf.no3.lm <- lm(pf.no3.hi$HI ~ pf.no3.hi$burn)

pf.fdom.hi %>%
  ggplot(aes(x=pf, 
             y=HI, 
             color=year))+
  geom_point() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("DOC") +
  xlab("Permafrost Extent (%)") +
  ylab("HI-Solute Storage")

pf.no3.hi %>%
  ggplot(aes(x=pf, 
             y=HI, 
             color=year))+
  geom_point() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("NO3") +
  xlab("Permafrost Extent (%)") +
  ylab("HI-Solute Storage")


### H 1.1: HI against precip ###
HI.mean.precip <- HI.dat %>% group_by(site.ID, year, storm.num) %>%  
  summarise_at(vars(HI), list(HI = mean)) # take mean by site response and year 

HI.mean.precip.response <- HI.dat %>% group_by(site.ID, year, storm.num, response) %>%  
  summarise_at(vars(HI), list(HI = mean)) # take mean by site response and year 

######################################## 2018 #####################################################################
FRCHstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2018/All_final/", 
                              recursive=F, 
                              pattern="FRCH", 
                              full.names=TRUE)

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                  read.csv, 
                                  check.names = FALSE,
                                  stringsAsFactors=FALSE, 
                                  header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 142),
                        rep("storm10a", 145),
                        rep("storm10b", 558),
                        rep("storm11a", 91),
                        rep("storm11b", 264),
                        rep("storm2a", 230),
                        rep("storm2b", 190),
                        rep("storm3", 234),
                        rep("storm4a", 72),
                        rep("storm4b", 383),
                        rep("storm5", 331),
                        rep("storm6", 303),
                        rep("storm7", 133),
                        rep("storm8a", 79),
                        rep("storm8b", 95),
                        rep("storm9", 116))

POKE_RainGauge_2018 <- read_csv("~/Documents/DoD_2018/RainGauge/POKE.RainGauge.2018.csv")
POKE_RainGauge_2018$DateTime <- as.POSIXct(POKE_RainGauge_2018$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH.2018.storms.1<- left_join(FRCH_storms, POKE_RainGauge_2018, by = "DateTime")

names(FRCH.2018.storms.1)[names(FRCH.2018.storms.1) == ''] <- 'x'

FRCH.2018.per.storm.1 <- FRCH.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

FRCH.2018 <- read.csv("Q_Chem/FRCH/FRCH_chem_2018.csv")
FRCH.2018$DateTime <- as.POSIXct(FRCH.2018$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M", origin = "2018-01-01")
FRCH.2018 <- left_join(FRCH.2018, POKE_RainGauge_2018, by = "DateTime")
FRCH.2018$week <- rollapplyr(FRCH.2018$inst_rainfall_mm, 336, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2018$month <- rollapplyr(FRCH.2018$inst_rainfall_mm, 1344, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2018$ThreeMonth <- rollapplyr(FRCH.2018$inst_rainfall_mm, 4032, sum, na.rm = TRUE, fill = NA, partial = TRUE)

FRCH.2018.1 <- left_join(FRCH.2018.storms.1, FRCH.2018, by = "DateTime") # week month and 3 month precip totals 

FRCH.2018.per.storm.2 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2018.per.storm.3 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2018.per.storm.4 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "turb")

HI.frch.no3.2018 <- left_join(HI.mean.precip.frch.NO3, FRCH.2018.per.storm.1, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.2, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.3, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.4, by = "storm.num")

HI.moos.frch.2018$precip.week <- as.numeric(HI.moos.no3.2018$precip.week)
HI.moos.frch.2018$precip.month <- as.numeric(HI.moos.no3.2018$precip.month)
HI.moos.frch.2018$ThreeMonth <- as.numeric(HI.moos.no3.2018$ThreeMonth)

moos.lm.no3 <- lm(HI.moos.no3$HI ~ HI.moos.no3$precip) # model one with just total precip
moos.lm.no3.1 <- lm(HI.moos.no3$HI ~ HI.moos.no3$precip.week) # model one with week before storm event
moos.lm.no3.2 <- lm(HI.moos.no3$HI ~ HI.moos.no3$precip.month) # model one with week before storm event
moos.lm.no3.3 <- lm(HI.moos.no3$HI ~ HI.moos.no3$ThreeMonth) # model one with week before storm event


HI.mean.precip.frch <- subset(HI.mean.precip, year == "2018" & site.ID == "FRCH")


HI.mean.precip.frch.2 <- cbind(HI.mean.precip.frch, FRCH.2018.per.storm.1)

precip.lm.1 <- lm(HI.mean.precip.frch.2$HI ~ HI.mean.precip.frch.2$precip) # model one with just total precip

frch.formula <- y ~ x

HI.mean.precip.frch.2 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("FRCH") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- FRCH.2018.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                             min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.mean.precip.frch.3 <- cbind(HI.mean.precip.frch.2, sum.time) # merging total time per storm event and the HI per storm 
HI.mean.precip.frch.3$TOTAL.TIME <- as.numeric(HI.mean.precip.frch.3$TOTAL.TIME)
HI.mean.precip.frch.3$Intensity <- HI.mean.precip.frch.3$precip/HI.mean.precip.frch.3$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

precip.lm.2 <- lm(HI.mean.precip.frch.3$HI ~ HI.mean.precip.frch.3$precip + HI.mean.precip.frch.3$Intensity) # model one with total precip and intensity 

HI.mean.precip.frch.3 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

FRCH.2018.storms.1$day <- julian(FRCH.2018.storms.1$DateTime, origin = as.POSIXct('2018-01-01', tz = 'America/Anchorage')) # making a fractional day column 
FRCH.2018.storms.1$day <- as.numeric(FRCH.2018.storms.1$day)

#time.since <- FRCH.2018.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(day),
                                                         min(day),units="hour")) %>% 
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=(TOTAL.TIME)) # creating a total time column


#time.since <- FRCH.2018.storms.1 %>% 
  group_by(storm.num) %>% 
  summarize(TimeSince = difftime(max(day), min(day), units = "days"))

#setDT(FRCH.2018.storms.1)[, days_since_last_event := as.numeric(max(day-shift(day,type="lag"))), 
         # by = storm.num]




HI.mean.precip.response <- HI.dat %>% group_by(site.ID, year, storm.num, response) %>%  
  summarise_at(vars(HI), list(HI = mean)) # take mean by site response and year 


HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "turb")


HI.mean.precip.frch.test <- left_join(HI.mean.precip.frch.fDOM, FRCH.2018.per.storm.1, by = "storm.num")

precip.lm.test <- lm(HI.mean.precip.frch.test$HI ~ HI.mean.precip.frch.test$precip) # model one with just total precip

frch.formula <- y ~ x

HI.mean.precip.frch.test %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 


# NO3 # 
HI.mean.precip.frch.test.1 <- left_join(HI.mean.precip.frch.NO3, FRCH.2018.per.storm.1, by = "storm.num")

precip.lm.test.1 <- lm(HI.mean.precip.frch.test.1$HI ~ HI.mean.precip.frch.test.1$precip) # model one with just total precip

frch.formula <- y ~ x

HI.mean.precip.frch.test.1 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.mean.precip.frch.SPC <- left_join(HI.mean.precip.frch.SPC, FRCH.2018.per.storm.1, by = "storm.num")

precip.lm.test <- lm(HI.mean.precip.frch.SPC$HI ~ HI.mean.precip.frch.SPC$precip) # model one with just total precip

frch.formula <- y ~ x

HI.mean.precip.frch.SPC %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.mean.precip.frch.turb <- left_join(HI.mean.precip.frch.turb, FRCH.2018.per.storm.1, by = "storm.num")

precip.lm.test <- lm(HI.mean.precip.frch.turb$HI ~ HI.mean.precip.frch.turb$precip) # model one with just total precip

frch.formula <- y ~ x

HI.mean.precip.frch.turb %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 


HI.mean.precip.frch.test.2 <- left_join(HI.mean.precip.frch.test, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.mean.precip.frch.test.2$TOTAL.TIME <- as.numeric(HI.mean.precip.frch.test.2$TOTAL.TIME)
HI.mean.precip.frch.test.2$Intensity <- HI.mean.precip.frch.test.2$precip/HI.mean.precip.frch.test.2$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

precip.lm.test.2 <- lm(HI.mean.precip.frch.test.2$HI ~ HI.mean.precip.frch.test.2$precip + HI.mean.precip.frch.test.2$Intensity) # model one with total precip and intensity 

HI.mean.precip.frch.test.2 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.mean.precip.frch.test.3 <- left_join(HI.mean.precip.frch.test.1, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm of NO3
HI.mean.precip.frch.test.3$TOTAL.TIME <- as.numeric(HI.mean.precip.frch.test.3$TOTAL.TIME)
HI.mean.precip.frch.test.3$Intensity <- HI.mean.precip.frch.test.3$precip/HI.mean.precip.frch.test.3$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

precip.lm.test.3 <- lm(HI.mean.precip.frch.test.3$HI ~ HI.mean.precip.frch.test.3$precip + HI.mean.precip.frch.test.3$Intensity) # model one with total precip and intensity 

HI.mean.precip.frch.test.3 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2 <- left_join(HI.mean.precip.frch.SPC, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm of NO3
HI.frch.SPC.2 $TOTAL.TIME <- as.numeric(HI.frch.SPC.2$TOTAL.TIME)
HI.frch.SPC.2$Intensity <- HI.frch.SPC.2$precip/HI.frch.SPC.2$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

precip.lm.test.3 <- lm(HI.frch.SPC.2$HI ~ HI.frch.SPC.2$precip + HI.frch.SPC.2$Intensity) # model one with total precip and intensity 

HI.frch.SPC.2 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model

HI.frch.turb.2 <- left_join(HI.mean.precip.frch.turb, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm of NO3
HI.frch.turb.2 $TOTAL.TIME <- as.numeric(HI.frch.turb.2$TOTAL.TIME)
HI.frch.turb.2$Intensity <- HI.frch.turb.2$precip/HI.frch.turb.2$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

precip.lm.test.3 <- lm(HI.frch.turb.2$HI ~ HI.frch.turb.2$precip + HI.frch.turb.2$Intensity) # model one with total precip and intensity 

HI.frch.turb.2 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model


# MOOS # 
MOOSstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2018/All_final/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 58),
                          rep("storm10", 432),
                          rep("storm11", 96),
                          rep("storm12", 324),
                          rep("storm2a", 70),
                          rep("storm2b", 166),
                          rep("storm2c", 192),
                          rep("storm3", 240),
                          rep("storm4", 244),
                          rep("storm5", 276),
                          rep("storm6", 332),
                          rep("storm7", 90),
                          rep("storm8", 176),
                          rep("storm9", 109))

MOOS_storms$DateTime <- as.POSIXct(MOOS_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS.2018.storms.1<- left_join(MOOS_storms, POKE_RainGauge_2018, by = "DateTime")

names(MOOS.2018.storms.1)[names(MOOS.2018.storms.1) == ''] <- 'x'

MOOS.2018.per.storm.1 <- MOOS.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

MOOS.2018 <- read.csv("Q_Chem/MOOS/MOOS_chem_2018.csv")
MOOS.2018$DateTime <- as.POSIXct(MOOS.2018$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
MOOS.2018 <- left_join(MOOS.2018, POKE_RainGauge_2018, by = "DateTime")
MOOS.2018$week <- rollapplyr(MOOS.2018$inst_rainfall_mm, 336, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2018$month <- rollapplyr(MOOS.2018$inst_rainfall_mm, 1344, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2018$ThreeMonth <- rollapplyr(MOOS.2018$inst_rainfall_mm, 4032, sum, na.rm = TRUE, fill = NA, partial = TRUE)

MOOS.2018.1 <- left_join(MOOS.2018.storms.1, MOOS.2018, by = "DateTime") # week month and 3 month precip totals 

MOOS.2018.per.storm.2 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2018.per.storm.3 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2018.per.storm.4 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 


HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "turb")

HI.moos.no3.2018 <- left_join(HI.mean.precip.moos.NO3, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.no3.2018 <- left_join(HI.moos.no3.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.no3.2018 <- left_join(HI.moos.no3.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.no3.2018 <- left_join(HI.moos.no3.2018, MOOS.2018.per.storm.4, by = "storm.num")

HI.moos.no3.2018$precip.week <- as.numeric(HI.moos.no3.2018$precip.week)
HI.moos.no3.2018$precip.month <- as.numeric(HI.moos.no3.2018$precip.month)
HI.moos.no3.2018$ThreeMonth <- as.numeric(HI.moos.no3.2018$ThreeMonth)

moos.lm.no3 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$precip) # model one with just total precip
moos.lm.no3.1 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$precip.week) # model one with week before storm event
moos.lm.no3.2 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$precip.month) # model one with week before storm event
moos.lm.no3.3 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$ThreeMonth) # model one with week before storm event

moos.formula <- y ~ x

a <- HI.moos.no3.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Storm Precip") +
  ylab("HI-Solute Storage") # plot model 

b <- HI.moos.no3.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model

c <- HI.moos.no3.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

d <- HI.moos.no3.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.moos.fDOM.2018 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.fDOM.2018 <- left_join(HI.moos.fDOM.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.fDOM.2018 <- left_join(HI.moos.fDOM.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.fDOM.2018 <- left_join(HI.moos.fDOM.2018, MOOS.2018.per.storm.4, by = "storm.num")


moos.lm.fDOM <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$precip) # model one with just total precip
moos.lm.fDOM.1 <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$precip.week) # model one with week before storm event
moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$precip.month) # model one with week before storm event
moos.lm.fDOM.3 <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$ThreeMonth) # model one with week before storm event

moos.formula <- y ~ x

e <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

f <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-week precip") +
  ylab("HI-Solute Storage") # plot model

g <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-month precip") +
  ylab("HI-Solute Storage") # plot model 

h <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Three-monh precip") +
  ylab("HI-Solute Storage") # plot model



HI.moos.SPC.2018 <- left_join(HI.mean.precip.moos.SPC, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.SPC.2018 <- left_join(HI.moos.SPC.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.SPC.2018 <- left_join(HI.moos.SPC.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.SPC.2018 <- left_join(HI.moos.SPC.2018, MOOS.2018.per.storm.4, by = "storm.num")


moos.lm.SPC <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$precip) # model one with just total precip
moos.lm.SPC.2 <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$precip.week) # model one with just total precip
moos.lm.SPC.3 <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$precip.month) # model one with just total precip
moos.lm.SPC.4 <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

i <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

j <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

k <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

l <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model



HI.moos.turb.2018 <- left_join(HI.mean.precip.moos.turb, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.turb.2018 <- left_join(HI.moos.turb.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.turb.2018 <- left_join(HI.moos.turb.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.turb.2018 <- left_join(HI.moos.turb.2018, MOOS.2018.per.storm.4, by = "storm.num")


moos.lm.turb <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$precip) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$precip.week) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$precip.month) # model one with just total precip
moos.lm.turb.4 <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

m <- HI.moos.turb.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model

n <- HI.moos.turb.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model

o <- HI.moos.turb.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

p <- HI.moos.turb.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model



sum.time <- MOOS.2018.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.moos.no3.2.2018 <- left_join(HI.moos.no3.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.no3.2.2018$TOTAL.TIME <- as.numeric(HI.moos.no3.2.2018$TOTAL.TIME)
HI.moos.no3.2.2018$Intensity <- HI.moos.no3.2.2018$precip/HI.moos.no3.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.no3.2 <- lm(HI.moos.no3.2.2018$HI ~ HI.moos.no3.2.2018$precip + HI.moos.no3.2.2018$Intensity) # model one with total precip and intensity 

q <- HI.moos.no3.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2018 <- left_join(HI.moos.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.moos.fDOM.2.2018$TOTAL.TIME)
HI.moos.fDOM.2.2018$Intensity <- HI.moos.fDOM.2.2018$precip/HI.moos.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2.2018$HI ~ HI.moos.fDOM.2.2018$precip + HI.moos.fDOM.2.2018$Intensity) # model one with total precip and intensity 

r <- HI.moos.fDOM.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2018 <- left_join(HI.moos.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.moos.SPC.2.2018$TOTAL.TIME)
HI.moos.SPC.2.2018$Intensity <- HI.moos.SPC.2.2018$precip/HI.moos.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.SPC.2 <- lm(HI.moos.SPC.2.2018$HI ~ HI.moos.SPC.2.2018$precip + HI.moos.SPC.2.2018$Intensity) # model one with total precip and intensity 

s <- HI.moos.SPC.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2018 <- left_join(HI.moos.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.turb.2.2018$TOTAL.TIME <- as.numeric(HI.moos.turb.2.2018$TOTAL.TIME)
HI.moos.turb.2.2018$Intensity <- HI.moos.turb.2.2018$precip/HI.moos.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.turb.2 <- lm(HI.moos.turb.2.2018$HI ~ HI.moos.turb.2.2018$precip + HI.moos.turb.2.2018$Intensity) # model one with total precip and intensity 

t <- HI.moos.turb.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
MOOS.2018.1 <- MOOS.2018.1[,-25]
MOOS.2018.1$day = yday(MOOS.2018.1$DateTime)
MOOS.2018.per.storm.5 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) 
HI.moos.no3.2.2018 <- left_join(HI.moos.no3.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2018$HI ~ HI.moos.no3.2.2018$doy)

u <- HI.moos.no3.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2018 <- left_join(HI.moos.fDOM.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2018$HI ~ HI.moos.fDOM.2.2018$doy)

u <- HI.moos.fDOM.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2018 <- left_join(HI.moos.SPC.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2018$HI ~ HI.moos.SPC.2.2018$doy)

v <- HI.moos.SPC.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2018 <- left_join(HI.moos.turb.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2018$HI ~ HI.moos.turb.2.2018$doy)

w <- HI.moos.turb.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.2018 <- rbind(HI.moos.no3.2.2018, HI.moos.fDOM.2.2018, HI.moos.SPC.2.2018, HI.moos.turb.2.2018) # merging all responses together 
HI.moos.2018$burn <- "burned" # adding a burn column
HI.moos.2018$pf <- "medium" # adding a pf column

write.csv(HI.moos.2018, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2018.csv")

############################################# 2019 ################################################################
# import rain gauge data #
FRCH_RainGauge_2019 <- read.csv("~/Documents/DoD_2019/RainGauge/FRCH.RainGauge.2019.csv")
POKE_RainGauge_2019 <- read.csv("~/Documents/DoD_2019/RainGauge/POKE.RainGauge.2019.csv")
VAUL_RainGauge_2019 <- read.csv("~/Documents/DoD_2019/RainGauge/VAUL.RainGauge.2019.csv")

# convert to AK time 
FRCH_RainGauge_2019$DateTime <- as.POSIXct(FRCH_RainGauge_2019$Datetime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
POKE_RainGauge_2019$DateTime <- as.POSIXct(POKE_RainGauge_2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
VAUL_RainGauge_2019$DateTime <- as.POSIXct(VAUL_RainGauge_2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")

# round to nearest 15 min 
FRCH_RainGauge_2019$DateTime <- lubridate::floor_date(FRCH_RainGauge_2019$DateTime, "15 minutes")
POKE_RainGauge_2019$DateTime <- lubridate::floor_date(POKE_RainGauge_2019$DateTime, "15 minutes")
VAUL_RainGauge_2019$DateTime <- lubridate::floor_date(VAUL_RainGauge_2019$DateTime, "15 minutes")

# MOOS # 
MOOSstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2019/AllSites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 702),
                          rep("storm3", 250),
                          rep("storm4", 256),
                          rep("storm5", 266),
                          rep("storm6a", 114),
                          rep("storm6b", 95),
                          rep("storm6c", 223),
                          rep("storm6d", 479),
                          rep("storm7a", 166),
                          rep("storm7b", 84),
                          rep("storm7c", 430),
                          rep("storm8", 174),
                          rep("storm9", 530))

MOOS_storms$DateTime <- as.POSIXct(MOOS_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS.2019.storms.1<- left_join(MOOS_storms, FRCH_RainGauge_2019, by = "DateTime")

names(MOOS.2019.storms.1)[names(MOOS.2019.storms.1) == ''] <- 'x'

MOOS.2019.per.storm.1 <- MOOS.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

MOOS.2019 <- read.csv("~/Documents/Storms/Q_Chem/MOOS/MOOS_chem_2019.csv")
MOOS.2019$DateTime <- as.POSIXct(MOOS.2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
MOOS.2019 <- left_join(MOOS.2019, FRCH_RainGauge_2019, by = "DateTime")
MOOS.2019$week <- rollapplyr(MOOS.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2019$month <- rollapplyr(MOOS.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2019$ThreeMonth <- rollapplyr(MOOS.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

MOOS.2019.1 <- left_join(MOOS.2019.storms.1, MOOS.2019, by = "DateTime") # week month and 3 month precip totals 

MOOS.2019.per.storm.2 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2019.per.storm.3 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2019.per.storm.4 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "turb")

HI.moos.no3.2019 <- left_join(HI.mean.precip.moos.NO3, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.no3.2019 <- left_join(HI.moos.no3.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.no3.2019 <- left_join(HI.moos.no3.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.no3.2019 <- left_join(HI.moos.no3.2019, MOOS.2019.per.storm.4, by = "storm.num")

moos.lm.no3 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$precip) # model one with just total precip
moos.lm.no3.2 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$precip.week) # model one with just total precip
moos.lm.no3.3 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$precip.month) # model one with just total precip
moos.lm.no3.4 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

aa <- HI.moos.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ab <- HI.moos.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ac <- HI.moos.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ad <- HI.moos.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.fDOM.2019 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.fDOM.2019 <- left_join(HI.moos.fDOM.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.fDOM.2019 <- left_join(HI.moos.fDOM.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.fDOM.2019 <- left_join(HI.moos.fDOM.2019, MOOS.2019.per.storm.4, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$precip) # model one with just total precip
moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$precip.week) # model one with just total precip
moos.lm.fDOM.3 <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$precip.month) # model one with just total precip
moos.lm.fDOM.4 <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

ae <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

af <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ag <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ah <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.SPC.2019 <- left_join(HI.mean.precip.moos.SPC, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.SPC.2019 <- left_join(HI.moos.SPC.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.SPC.2019 <- left_join(HI.moos.SPC.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.SPC.2019 <- left_join(HI.moos.SPC.2019, MOOS.2019.per.storm.4, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$precip) # model one with just total precip
moos.lm.SPC.2 <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$precip.week) # model one with just total precip
moos.lm.SPC.3 <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$precip.month) # model one with just total precip
moos.lm.SPC.4 <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

ai <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

aj <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ak <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

al <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.turb.2019 <- left_join(HI.mean.precip.moos.turb, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.turb.2019 <- left_join(HI.moos.turb.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.turb.2019 <- left_join(HI.moos.turb.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.turb.2019 <- left_join(HI.moos.turb.2019, MOOS.2019.per.storm.4, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$precip) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$precip.week) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$precip.month) # model one with just total precip
moos.lm.turb.4 <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

am <- HI.moos.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

an <- HI.moos.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ao <- HI.moos.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ap <- HI.moos.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- MOOS.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.moos.no3.2.2019 <- left_join(HI.moos.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.no3.2.2019$TOTAL.TIME <- as.numeric(HI.moos.no3.2.2019$TOTAL.TIME)
HI.moos.no3.2.2019$Intensity <- HI.moos.no3.2.2019$precip/HI.moos.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.no3.2 <- lm(HI.moos.no3.2.2019$HI ~ HI.moos.no3.2.2019$precip + HI.moos.no3.2.2019$Intensity) # model one with total precip and intensity 

aq <- HI.moos.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2019 <- left_join(HI.moos.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.moos.fDOM.2.2019$TOTAL.TIME)
HI.moos.fDOM.2.2019$Intensity <- HI.moos.fDOM.2.2019$precip/HI.moos.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2.2019$HI ~ HI.moos.fDOM.2.2019$precip + HI.moos.fDOM.2.2019$Intensity) # model one with total precip and intensity 

ar <- HI.moos.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2019 <- left_join(HI.moos.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.moos.SPC.2.2019$TOTAL.TIME)
HI.moos.SPC.2.2019$Intensity <- HI.moos.SPC.2.2019$precip/HI.moos.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.SPC.2 <- lm(HI.moos.SPC.2.2019$HI ~ HI.moos.SPC.2.2019$precip + HI.moos.SPC.2.2019$Intensity) # model one with total precip and intensity 

as <- HI.moos.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2019 <- left_join(HI.moos.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.turb.2.2019$TOTAL.TIME <- as.numeric(HI.moos.turb.2.2019$TOTAL.TIME)
HI.moos.turb.2.2019$Intensity <- HI.moos.turb.2.2019$precip/HI.moos.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.turb.2 <- lm(HI.moos.turb.2.2019$HI ~ HI.moos.turb.2.2019$precip + HI.moos.turb.2.2019$Intensity) # model one with total precip and intensity 

at <- HI.moos.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at, 
          ncol = 4)

# day of year #
MOOS.2019.1$day <- julian(MOOS.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
MOOS.2019.1$day <- as.numeric(MOOS.2019.1$day)

MOOS.2019.per.storm.5 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.moos.no3.2.2019 <- left_join(HI.moos.no3.2.2019, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2019$HI ~ HI.moos.no3.2.2019$doy)

au <- HI.moos.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2019 <- left_join(HI.moos.fDOM.2.2019, MOOS.2019.per.storm.5, by = "storm.num")
moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2019$HI ~ HI.moos.fDOM.2.2019$doy)

av <- HI.moos.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2019 <- left_join(HI.moos.SPC.2.2019, MOOS.2019.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2019$HI ~ HI.moos.SPC.2.2019$doy)

aw <- HI.moos.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2019 <- left_join(HI.moos.turb.2.2019, MOOS.2019.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2019$HI ~ HI.moos.turb.2.2019$doy)

ax <- HI.moos.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.2019 <- rbind(HI.moos.no3.2.2019, HI.moos.fDOM.2.2019, HI.moos.SPC.2.2019, HI.moos.turb.2.2019) # merging all responses together 
HI.moos.2019$burn <- "burned" # adding a burn column
HI.moos.2019$pf <- "medium" # adding a pf column

write.csv(HI.moos.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2019.csv")


# FRCH # 
FRCHstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2019/AllSites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 993),
                          rep("storm10a", 121),
                          rep("storm10b", 95),
                          rep("storm10c", 207),
                          rep("storm11", 479),
                          rep("storm12a", 183),
                          rep("storm12b", 67),
                          rep("storm12c", 511),
                          rep("storm12d", 855),
                          rep("storm13", 391),
                          rep("storm14", 631),
                          rep("storm2", 113),
                          rep("storm3", 201),
                          rep("storm4", 193),
                          rep("storm5", 229),
                          rep("storm6", 257),
                          rep("storm7", 133),
                          rep("storm8", 105),
                          rep("storm9a", 61),
                          rep("storm9b", 149))
                          
                          

FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH.2019.storms.1<- left_join(FRCH_storms, FRCH_RainGauge_2019, by = "DateTime")

names(FRCH.2019.storms.1)[names(FRCH.2019.storms.1) == ''] <- 'x'

FRCH.2019.per.storm.1 <- FRCH.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

FRCH.2019 <- read.csv("~/Documents/Storms/Q_Chem/FRCH/FRCH_chem_2019.csv")
FRCH.2019$DateTime <- as.POSIXct(FRCH.2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
FRCH.2019 <- left_join(FRCH.2019, FRCH_RainGauge_2019, by = "DateTime")
FRCH.2019$week <- rollapplyr(FRCH.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2019$month <- rollapplyr(FRCH.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2019$ThreeMonth <- rollapplyr(FRCH.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

FRCH.2019.1 <- left_join(FRCH.2019.storms.1, FRCH.2019, by = "DateTime") # week month and 3 month precip totals 

FRCH.2019.per.storm.2 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2019.per.storm.3 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2019.per.storm.4 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "turb")

HI.frch.no3.2019 <- left_join(HI.mean.precip.frch.NO3, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.4, by = "storm.num")
HI.frch.no3.2019 <- HI.frch.no3.2019[,-6]
names(HI.frch.no3.2019) <- c("site.ID", "year", "storm.num","response", "HI", "precip","precip.week", 
                     "precip.month", "ThreeMonth")
frch.lm.no3 <- lm(HI.frch.no3.2019$HI ~ HI.frch.no3.2019$precip) # model one with just total precip

frch.formula <- y ~ x

ba <- HI.frch.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bb <- HI.frch.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bc <- HI.frch.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bd <- HI.frch.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.fDOM.2019 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.fDOM.2019 <- left_join(HI.frch.fDOM.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.fDOM.2019 <- left_join(HI.frch.fDOM.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.fDOM.2019 <- left_join(HI.frch.fDOM.2019, FRCH.2019.per.storm.4, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

be <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bf <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bg <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bh <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.SPC.2019 <- left_join(HI.mean.precip.frch.SPC, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.SPC.2019 <- left_join(HI.frch.SPC.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.SPC.2019 <- left_join(HI.frch.SPC.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.SPC.2019 <- left_join(HI.frch.SPC.2019, FRCH.2019.per.storm.4, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$ThreeMonth) # model one with just total precip


bi <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bj <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bk <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bl <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.turb.2019 <- left_join(HI.mean.precip.frch.turb, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.turb.2019 <- left_join(HI.frch.turb.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.turb.2019 <- left_join(HI.frch.turb.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.turb.2019 <- left_join(HI.frch.turb.2019, FRCH.2019.per.storm.4, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$ThreeMonth) # model one with just total precip

bm <- HI.frch.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bn <- HI.frch.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bo <- HI.frch.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bp <- HI.frch.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- FRCH.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.frch.no3.2.2019 <- left_join(HI.frch.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.no3.2.2019$TOTAL.TIME <- as.numeric(HI.frch.no3.2.2019$TOTAL.TIME)
HI.frch.no3.2.2019$Intensity <- HI.frch.no3.2.2019$precip/HI.frch.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.no3.2 <- lm(HI.frch.no3.2.2019$HI ~ HI.frch.no3.2.2019$precip + HI.frch.no3.2.2019$Intensity) # model one with total precip and intensity 

bq <- HI.frch.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2019 <- left_join(HI.frch.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2019$TOTAL.TIME)
HI.frch.fDOM.2.2019$Intensity <- HI.frch.fDOM.2.2019$precip/HI.frch.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2019$HI ~ HI.frch.fDOM.2.2019$precip + HI.frch.fDOM.2.2019$Intensity) # model one with total precip and intensity 

br <- HI.frch.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2019 <- left_join(HI.frch.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2019$TOTAL.TIME)
HI.frch.SPC.2.2019$Intensity <- HI.frch.SPC.2.2019$precip/HI.frch.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2019$HI ~ HI.frch.SPC.2.2019$precip + HI.frch.SPC.2.2019$Intensity) # model one with total precip and intensity 

bs <- HI.frch.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2019 <- left_join(HI.frch.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.turb.2.2019$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2019$TOTAL.TIME)
HI.frch.turb.2.2019$Intensity <- HI.frch.turb.2.2019$precip/HI.frch.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.turb.2 <- lm(HI.frch.turb.2.2019$HI ~ HI.frch.turb.2.2019$precip + HI.frch.turb.2.2019$Intensity) # model one with total precip and intensity 

bt <- HI.frch.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
FRCH.2019.1$day <- julian(FRCH.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
FRCH.2019.1$day <- as.numeric(FRCH.2019.1$day)

FRCH.2019.per.storm.5 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.frch.no3.2.2019 <- left_join(HI.frch.no3.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2019$HI ~ HI.frch.no3.2.2019$doy)

bu <- HI.frch.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2019 <- left_join(HI.frch.fDOM.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2019$HI ~ HI.frch.fDOM.2.2019$doy)

bv <- HI.frch.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2019 <- left_join(HI.frch.SPC.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2019$HI ~ HI.frch.SPC.2.2019$doy)

bw <- HI.frch.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2019 <- left_join(HI.frch.turb.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2019$HI ~ HI.frch.turb.2.2019$doy)

bx <- HI.frch.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.2019 <- rbind(HI.frch.no3.2.2019, HI.frch.fDOM.2.2019, HI.frch.SPC.2.2019, HI.frch.turb.2.2019) # merging all responses together 
HI.frch.2019$burn <- "unburned" # adding a burn column
HI.frch.2019$pf <- "medium" # adding a pf column

write.csv(HI.frch.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.frch.2019.csv")


# POKE # 
POKEstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2019/AllSites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(rep("storm1", 103),
                          rep("storm2", 91),
                          rep("storm3", 147),
                          rep("storm4", 115),
                          rep("storm5a", 87),
                          rep("storm5b", 239),
                          rep("storm5c", 111),
                          rep("storm5d", 99),
                          rep("storm6a", 51),
                          rep("storm6b", 227),
                          rep("storm7", 267),
                          rep("storm8", 95),
                          rep("storm9", 211))



POKE_storms$DateTime <- as.POSIXct(POKE_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
POKE.2019.storms.1<- left_join(POKE_storms, POKE_RainGauge_2019, by = "DateTime")

names(POKE.2019.storms.1)[names(POKE.2019.storms.1) == ''] <- 'x'

POKE.2019.per.storm.1 <- POKE.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

POKE.2019 <- read.csv("~/Documents/Storms/Q_Chem/POKE/POKE_chem_2019.csv")
POKE.2019$DateTime <- as.POSIXct(POKE.2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
POKE.2019 <- left_join(POKE.2019, POKE_RainGauge_2019, by = "DateTime")
POKE.2019$week <- rollapplyr(POKE.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2019$month <- rollapplyr(POKE.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2019$ThreeMonth <- rollapplyr(POKE.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

POKE.2019.1 <- left_join(POKE.2019.storms.1, POKE.2019, by = "DateTime") # week month and 3 month precip totals 

POKE.2019.per.storm.2 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
POKE.2019.per.storm.3 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
POKE.2019.per.storm.4 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.poke.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "NO3")
HI.mean.precip.poke.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "fDOM")
HI.mean.precip.poke.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "SPC")
HI.mean.precip.poke.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "turb")

HI.poke.no3.2019 <- left_join(HI.mean.precip.poke.NO3, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.no3.2019 <- left_join(HI.poke.no3.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.no3.2019 <- left_join(HI.poke.no3.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.no3.2019 <- left_join(HI.poke.no3.2019, POKE.2019.per.storm.4, by = "storm.num")

poke.lm.no3 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$precip) # model one with just total precip
poke.lm.no3.2 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$precip.week) # model one with just total precip
poke.lm.no3.3 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$precip.month) # model one with just total precip
poke.lm.no3.4 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$ThreeMonth) # model one with just total precip

poke.formula <- y ~ x

pa <- HI.poke.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pb <- HI.poke.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-weeek Precip") +
  ylab("HI-Solute Storage") # plot model 

pc <- HI.poke.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

pd <- HI.poke.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.fDOM.2019 <- left_join(HI.mean.precip.poke.fDOM, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.fDOM.2019 <- left_join(HI.poke.fDOM.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.fDOM.2019 <- left_join(HI.poke.fDOM.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.fDOM.2019 <- left_join(HI.poke.fDOM.2019, POKE.2019.per.storm.4, by = "storm.num")

poke.lm.fDOM <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$precip) # model one with just total precip
poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$precip.week) # model one with just total precip
poke.lm.fDOM.3 <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$precip.month) # model one with just total precip
poke.lm.fDOM.4 <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$ThreeMonth) # model one with just total precip

pe <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pf <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

pg <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ph <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.SPC.2019 <- left_join(HI.mean.precip.poke.SPC, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.SPC.2019 <- left_join(HI.poke.SPC.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.SPC.2019 <- left_join(HI.poke.SPC.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.SPC.2019 <- left_join(HI.poke.SPC.2019, POKE.2019.per.storm.4, by = "storm.num")

poke.lm.SPC <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$precip) # model one with just total precip
poke.lm.SPC.2 <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$precip.week) # model one with just total precip
poke.lm.SPC.3 <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$precip.month) # model one with just total precip
poke.lm.SPC.4 <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$ThreeMonth) # model one with just total precip

pi <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pj <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

pk <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

pl <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.poke.turb.2019 <- left_join(HI.mean.precip.poke.turb, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.turb.2019 <- left_join(HI.poke.turb.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.turb.2019 <- left_join(HI.poke.turb.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.turb.2019 <- left_join(HI.poke.turb.2019, POKE.2019.per.storm.4, by = "storm.num")

poke.lm.turb <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$precip) # model one with just total precip
poke.lm.turb.2 <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$precip.week) # model one with just total precip
poke.lm.turb.3 <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$precip.month) # model one with just total precip
poke.lm.turb.4 <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$ThreeMonth) # model one with just total precip

pm <- HI.poke.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pn <- HI.poke.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

po <- HI.poke.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

pp <- HI.poke.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- POKE.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.poke.no3.2.2019 <- left_join(HI.poke.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.no3.2.2019$TOTAL.TIME <- as.numeric(HI.poke.no3.2.2019$TOTAL.TIME)
HI.poke.no3.2.2019$Intensity <- HI.poke.no3.2.2019$precip/HI.poke.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.no3.2 <- lm(HI.poke.no3.2.2019$HI ~ HI.poke.no3.2.2019$precip + HI.poke.no3.2.2019$Intensity) # model one with total precip and intensity 

pq <- HI.poke.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2019 <- left_join(HI.poke.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.poke.fDOM.2.2019$TOTAL.TIME)
HI.poke.fDOM.2.2019$Intensity <- HI.poke.fDOM.2.2019$precip/HI.poke.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2.2019$HI ~ HI.poke.fDOM.2.2019$precip + HI.poke.fDOM.2.2019$Intensity) # model one with total precip and intensity 

pr <- HI.poke.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2019 <- left_join(HI.poke.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.poke.SPC.2.2019$TOTAL.TIME)
HI.poke.SPC.2.2019$Intensity <- HI.poke.SPC.2.2019$precip/HI.poke.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.SPC.2.2019 <- lm(HI.poke.SPC.2.2019$HI ~ HI.poke.SPC.2.2019$precip + HI.poke.SPC.2.2019$Intensity) # model one with total precip and intensity 

ps <- HI.poke.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2019 <- left_join(HI.poke.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.turb.2.2019$TOTAL.TIME <- as.numeric(HI.poke.turb.2.2019$TOTAL.TIME)
HI.poke.turb.2.2019$Intensity <- HI.poke.turb.2.2019$precip/HI.poke.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.turb.2 <- lm(HI.poke.turb.2.2019$HI ~ HI.poke.turb.2.2019$precip + HI.poke.turb.2.2019$Intensity) # model one with total precip and intensity 

pt <- HI.poke.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
POKE.2019.1$day <- julian(POKE.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
POKE.2019.1$day <- as.numeric(POKE.2019.1$day)

POKE.2019.per.storm.5 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.poke.no3.2.2019 <- left_join(HI.poke.no3.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.no3.5 <- lm(HI.poke.no3.2.2019$HI ~ HI.poke.no3.2.2019$doy)

pu <- HI.poke.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2019 <- left_join(HI.poke.fDOM.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.fDOM.5 <- lm(HI.poke.fDOM.2.2019$HI ~ HI.poke.fDOM.2.2019$doy)

pv <- HI.poke.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2019 <- left_join(HI.poke.SPC.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.SPC.5 <- lm(HI.poke.SPC.2.2019$HI ~ HI.poke.SPC.2.2019$doy)

pw <- HI.poke.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2019 <- left_join(HI.poke.turb.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.turb.5 <- lm(HI.poke.turb.2.2019$HI ~ HI.poke.turb.2.2019$doy)

px <- HI.poke.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pp,pq,pr,ps,pt,pu,pv,pw,px,
          ncol = 4)

HI.poke.2019 <- rbind(HI.poke.no3.2.2019, HI.poke.fDOM.2.2019, HI.poke.SPC.2.2019, HI.poke.turb.2.2019) # merging all responses together 
HI.poke.2019$burn <- "burned" # adding a burn column
HI.poke.2019$pf <- "medium" # adding a pf column

write.csv(HI.poke.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.poke.2019.csv")

# VAUL # 
VAULstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2019/AllSites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(rep("storm1", 191),
                          rep("storm2", 207),
                          rep("storm3", 191),
                          rep("storm4a", 83),
                          rep("storm4b", 219),
                          rep("storm4c", 707),
                          rep("storm5", 275),
                          rep("storm6", 263),
                          rep("storm7", 107),
                          rep("storm8a", 167),
                          rep("storm8b", 223),
                          rep("storm8c", 479))



VAUL_storms$DateTime <- as.POSIXct(VAUL_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
VAUL.2019.storms.1<- left_join(VAUL_storms, VAUL_RainGauge_2019, by = "DateTime")

names(VAUL.2019.storms.1)[names(VAUL.2019.storms.1) == ''] <- 'x'

VAUL.2019.per.storm.1 <- VAUL.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

VAUL.2019 <- read.csv("~/Documents/Storms/Q_Chem/VAUL/VAUL_chem_2019.csv")
VAUL.2019$DateTime <- as.POSIXct(VAUL.2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
VAUL.2019 <- left_join(VAUL.2019, VAUL_RainGauge_2019, by = "DateTime")
VAUL.2019$week <- rollapplyr(VAUL.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2019$month <- rollapplyr(VAUL.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2019$ThreeMonth <- rollapplyr(VAUL.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

VAUL.2019.1 <- left_join(VAUL.2019.storms.1, VAUL.2019, by = "DateTime") # week month and 3 month precip totals 

VAUL.2019.per.storm.2 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
VAUL.2019.per.storm.3 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
VAUL.2019.per.storm.4 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.vaul.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "NO3")
HI.mean.precip.vaul.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "fDOM")
HI.mean.precip.vaul.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "SPC")
HI.mean.precip.vaul.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "turb")

HI.vaul.no3.2019 <- left_join(HI.mean.precip.vaul.NO3, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.no3.2019 <- left_join(HI.vaul.no3.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.no3.2019 <- left_join(HI.vaul.no3.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.no3.2019 <- left_join(HI.vaul.no3.2019, VAUL.2019.per.storm.4, by = "storm.num")

vaul.lm.no3 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$precip) # model one with just total precip
vaul.lm.no3.2 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$precip.week) # model one with just total precip
vaul.lm.no3.3 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$precip.month) # model one with just total precip
vaul.lm.no3.4 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$ThreeMonth) # model one with just total precip

vaul.formula <- y ~ x

va <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vb <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vc <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vd <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.fDOM.2019 <- left_join(HI.mean.precip.vaul.fDOM, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.fDOM.2019 <- left_join(HI.vaul.fDOM.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.fDOM.2019 <- left_join(HI.vaul.fDOM.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.fDOM.2019 <- left_join(HI.vaul.fDOM.2019, VAUL.2019.per.storm.4, by = "storm.num")

vaul.lm.fDOM <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$precip) # model one with just total precip
vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$precip.week) # model one with just total precip
vaul.lm.fDOM.3 <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$precip.month) # model one with just total precip
vaul.lm.fDOM.4 <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$ThreeMonth) # model one with just total precip

ve <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vf <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vg <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vh <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.SPC.2019 <- left_join(HI.mean.precip.vaul.SPC, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.SPC.2019 <- left_join(HI.vaul.SPC.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.SPC.2019 <- left_join(HI.vaul.SPC.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.SPC.2019 <- left_join(HI.vaul.SPC.2019, VAUL.2019.per.storm.4, by = "storm.num")

vaul.lm.SPC <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$precip) # model one with just total precip
vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$precip.week) # model one with just total precip
vaul.lm.SPC.3 <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$precip.month) # model one with just total precip
vaul.lm.SPC.4 <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$ThreeMonth) # model one with just total precip

vi <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vj <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vk <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vl <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.turb.2019 <- left_join(HI.mean.precip.vaul.turb, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.turb.2019 <- left_join(HI.vaul.turb.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.turb.2019 <- left_join(HI.vaul.turb.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.turb.2019 <- left_join(HI.vaul.turb.2019, VAUL.2019.per.storm.4, by = "storm.num")

vaul.lm.turb <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$precip) # model one with just total precip
vaul.lm.turb.2 <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$precip.week) # model one with just total precip
vaul.lm.turb.3 <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$precip.month) # model one with just total precip
vaul.lm.turb.4 <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$ThreeMonth) # model one with just total precip

vm <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vn <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vo <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vp <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- VAUL.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.vaul.no3.2.2019 <- left_join(HI.vaul.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.no3.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.no3.2.2019$TOTAL.TIME)
HI.vaul.no3.2.2019$Intensity <- HI.vaul.no3.2.2019$precip/HI.vaul.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.no3.2 <- lm(HI.vaul.no3.2.2019$HI ~ HI.vaul.no3.2.2019$precip + HI.vaul.no3.2.2019$Intensity) # model one with total precip and intensity 

vq <- HI.vaul.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2019 <- left_join(HI.vaul.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.fDOM.2.2019$TOTAL.TIME)
HI.vaul.fDOM.2.2019$Intensity <- HI.vaul.fDOM.2.2019$precip/HI.vaul.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2.2019$HI ~ HI.vaul.fDOM.2.2019$precip + HI.vaul.fDOM.2.2019$Intensity) # model one with total precip and intensity 

vr <- HI.vaul.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2019 <- left_join(HI.vaul.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.SPC.2.2019$TOTAL.TIME)
HI.vaul.SPC.2.2019$Intensity <- HI.vaul.SPC.2.2019$precip/HI.vaul.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2.2019$HI ~ HI.vaul.SPC.2.2019$precip + HI.vaul.SPC.2.2019$Intensity) # model one with total precip and intensity 

vs <- HI.vaul.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2019 <- left_join(HI.vaul.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.turb.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.turb.2.2019$TOTAL.TIME)
HI.vaul.turb.2.2019$Intensity <- HI.vaul.turb.2.2019$precip/HI.vaul.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.turb.2 <- lm(HI.vaul.turb.2.2019$HI ~ HI.vaul.turb.2.2019$precip + HI.vaul.turb.2.2019$Intensity) # model one with total precip and intensity 

vt <- HI.vaul.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
VAUL.2019.1$day <- julian(VAUL.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
VAUL.2019.1$day <- as.numeric(VAUL.2019.1$day)

VAUL.2019.per.storm.5 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.vaul.no3.2.2019 <- left_join(HI.vaul.no3.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.no3.5 <- lm(HI.vaul.no3.2.2019$HI ~ HI.vaul.no3.2.2019$doy)

vu <- HI.vaul.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2019 <- left_join(HI.vaul.fDOM.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.fDOM.5 <- lm(HI.vaul.fDOM.2.2019$HI ~ HI.vaul.fDOM.2.2019$doy)

vv <- HI.vaul.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2019 <- left_join(HI.vaul.SPC.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.SPC.5 <- lm(HI.vaul.SPC.2.2019$HI ~ HI.vaul.SPC.2.2019$doy)

vw <- HI.vaul.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2019 <- left_join(HI.vaul.turb.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.turb.5 <- lm(HI.vaul.turb.2.2019$HI ~ HI.vaul.turb.2.2019$doy)

vx <- HI.vaul.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn,vo,vp,vq,vr,vs,vt,vu,vv,vw,vx,
          ncol = 4)

HI.vaul.2019 <- rbind(HI.vaul.no3.2.2019, HI.vaul.fDOM.2.2019, HI.vaul.SPC.2.2019, HI.vaul.turb.2.2019) # merging all responses together 
HI.vaul.2019$burn <- "unburned" # adding a burn column
HI.vaul.2019$pf <- "continuous" # adding a pf column

write.csv(HI.vaul.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.vaul.2019.csv")


# STRT # 
STRTstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2019/AllSites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm1", 642),
                          rep("storm2", 278),
                          rep("storm3a", 1039),
                          rep("storm3b", 290),
                          rep("storm3c", 178),
                          rep("storm4", 470),
                          rep("storm5", 102),
                          rep("storm6", 250),
                          rep("storm7", 538),
                          rep("storm7b", 270))



STRT_storms$DateTime <- as.POSIXct(STRT_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
STRT.2019.storms.1<- left_join(STRT_storms, FRCH_RainGauge_2019, by = "DateTime")

names(STRT.2019.storms.1)[names(STRT.2019.storms.1) == ''] <- 'x'

STRT.2019.per.storm.1 <- STRT.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

STRT.2019 <- read.csv("~/Documents/Storms/Q_Chem/STRT/STRT_chem_2019.csv")
STRT.2019$DateTime <- as.POSIXct(STRT.2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
STRT.2019 <- left_join(STRT.2019, FRCH_RainGauge_2019, by = "DateTime")
STRT.2019$week <- rollapplyr(STRT.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2019$month <- rollapplyr(STRT.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2019$ThreeMonth <- rollapplyr(STRT.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

STRT.2019.1 <- left_join(STRT.2019.storms.1, STRT.2019, by = "DateTime") # week month and 3 month precip totals 

STRT.2019.per.storm.2 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
STRT.2019.per.storm.3 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
STRT.2019.per.storm.4 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.strt.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "NO3")
HI.mean.precip.strt.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "fDOM")
HI.mean.precip.strt.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "SPC")
HI.mean.precip.strt.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "turb")

HI.strt.no3.2019 <- left_join(HI.mean.precip.strt.NO3, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.no3.2019 <- left_join(HI.strt.no3.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.no3.2019 <- left_join(HI.strt.no3.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.no3.2019 <- left_join(HI.strt.no3.2019, STRT.2019.per.storm.4, by = "storm.num")

strt.lm.no3 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$precip) # model one with just total precip
strt.lm.no3.2 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$precip.week) # model one with just total precip
strt.lm.no3.3 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$precip.month) # model one with just total precip
strt.lm.no3.4 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$ThreeMonth) # model one with just total precip

strt.formula <- y ~ x

sa <- HI.strt.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sb <- HI.strt.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sc <- HI.strt.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sd <- HI.strt.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


HI.strt.fDOM.2019 <- left_join(HI.mean.precip.strt.fDOM, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.fDOM.2019 <- left_join(HI.strt.fDOM.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.fDOM.2019 <- left_join(HI.strt.fDOM.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.fDOM.2019 <- left_join(HI.strt.fDOM.2019, STRT.2019.per.storm.4, by = "storm.num")

strt.lm.fDOM <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$precip) # model one with just total precip
strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$precip.week) # model one with just total precip
strt.lm.fDOM.3 <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$precip.month) # model one with just total precip
strt.lm.fDOM.4 <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$ThreeMonth) # model one with just total precip

se <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sf <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model

sg <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

sh <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.strt.SPC.2019 <- left_join(HI.mean.precip.strt.SPC, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.SPC.2019 <- left_join(HI.strt.SPC.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.SPC.2019 <- left_join(HI.strt.SPC.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.SPC.2019 <- left_join(HI.strt.SPC.2019, STRT.2019.per.storm.4, by = "storm.num")

strt.lm.SPC <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$precip) # model one with just total precip
strt.lm.SPC.2 <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$precip.week) # model one with just total precip
strt.lm.SPC.3 <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$precip.month) # model one with just total precip
strt.lm.SPC.4 <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$ThreeMonth) # model one with just total precip

si <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sj <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sk <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sl <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.turb.2019 <- left_join(HI.mean.precip.strt.turb, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.turb.2019 <- left_join(HI.strt.turb.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.turb.2019 <- left_join(HI.strt.turb.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.turb.2019 <- left_join(HI.strt.turb.2019, STRT.2019.per.storm.4, by = "storm.num")

strt.lm.turb <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$precip) # model one with just total precip
strt.lm.turb.2 <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$precip.week) # model one with just total precip
strt.lm.turb.3 <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$precip.month) # model one with just total precip
strt.lm.turb.4 <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$ThreeMonth) # model one with just total precip

sm <- HI.strt.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sn <- HI.strt.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

so <- HI.strt.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sp <- HI.strt.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

STRT.2019.storms.1 <- na.omit(STRT.2019.storms.1)

sum.time <- STRT.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.strt.no3.2.2019 <- left_join(HI.strt.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.no3.2.2019$TOTAL.TIME <- as.numeric(HI.strt.no3.2.2019$TOTAL.TIME)
HI.strt.no3.2.2019$Intensity <- HI.strt.no3.2.2019$precip/HI.strt.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.no3.2 <- lm(HI.strt.no3.2.2019$HI ~ HI.strt.no3.2.2019$precip + HI.strt.no3.2.2019$Intensity) # model one with total precip and intensity 

sq <- HI.strt.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2019 <- left_join(HI.strt.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.strt.fDOM.2.2019$TOTAL.TIME)
HI.strt.fDOM.2.2019$Intensity <- HI.strt.fDOM.2.2019$precip/HI.strt.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2.2019$HI ~ HI.strt.fDOM.2.2019$precip + HI.strt.fDOM.2.2019$Intensity) # model one with total precip and intensity 

sr <- HI.strt.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2019 <- left_join(HI.strt.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.strt.SPC.2.2019$TOTAL.TIME)
HI.strt.SPC.2.2019$Intensity <- HI.strt.SPC.2.2019$precip/HI.strt.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.SPC.2 <- lm(HI.strt.SPC.2.2019$HI ~ HI.strt.SPC.2.2019$precip + HI.strt.SPC.2.2019$Intensity) # model one with total precip and intensity 

ss <- HI.strt.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2019 <- left_join(HI.strt.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.turb.2.2019$TOTAL.TIME <- as.numeric(HI.strt.turb.2.2019$TOTAL.TIME)
HI.strt.turb.2.2019$Intensity <- HI.strt.turb.2.2019$precip/HI.strt.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.turb.2 <- lm(HI.strt.turb.2.2019$HI ~ HI.strt.turb.2.2019$precip + HI.strt.turb.2.2019$Intensity) # model one with total precip and intensity 

st <- HI.strt.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
STRT.2019.1$day <- julian(STRT.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
STRT.2019.1$day <- as.numeric(STRT.2019.1$day)

STRT.2019.per.storm.5 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.strt.no3.2.2019 <- left_join(HI.strt.no3.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.no3.5 <- lm(HI.strt.no3.2.2019$HI ~ HI.strt.no3.2.2019$doy)

su <- HI.strt.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2019 <- left_join(HI.strt.fDOM.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.fDOM.5 <- lm(HI.strt.fDOM.2.2019$HI ~ HI.strt.fDOM.2.2019$doy)

sv <- HI.strt.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2019 <- left_join(HI.strt.SPC.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.SPC.5 <- lm(HI.strt.SPC.2.2019$HI ~ HI.strt.SPC.2.2019$doy)

sw <- HI.strt.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2019 <- left_join(HI.strt.turb.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.turb.5 <- lm(HI.strt.turb.2.2019$HI ~ HI.strt.turb.2.2019$doy)

sx <- HI.strt.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so,sp,sq,sr,ss,st,su,sv,sw,sx,
          ncol = 4)

HI.strt.2019 <- rbind(HI.strt.no3.2.2019, HI.strt.fDOM.2.2019, HI.strt.SPC.2.2019, HI.strt.turb.2.2019) # merging all responses together 
HI.strt.2019$burn <- "burned" # adding a burn column
HI.strt.2019$pf <- "high" # adding a pf column

write.csv(HI.strt.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.strt.2019.csv")

HI.2019 <- rbind(HI.moos.2019, HI.frch.2019, HI.poke.2019, HI.vaul.2019, HI.strt.2019) # bind all 2019 together
write.csv(HI.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.csv")

storms.2019 <- rbind(FRCH_storms, MOOS_storms, VAUL_storms, POKE_storms, STRT_storms)
write.csv(storms.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/storms.2019.csv")
############################################# 2020 ################################################################
# import rain gauge data #
FRCH_RainGauge_2020 <- read.csv("~/Documents/DoD_2020/RainGauge/FRCH.RainGauge.2020.csv")
POKE_RainGauge_2020 <- read.csv("~/Documents/DoD_2020/RainGauge/POKE.RainGauge.2020.csv")
VAUL_RainGauge_2020 <- read.csv("~/Documents/DoD_2020/RainGauge/VAUL.RainGauge.2020.csv")
STRT_RainGauge_2020 <- read.csv("~/Documents/DoD_2020/RainGauge/STRT.RainGauge.2020.csv")

# convert to AK time 
FRCH_RainGauge_2020$DateTime <- as.POSIXct(FRCH_RainGauge_2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
POKE_RainGauge_2020$DateTime <- as.POSIXct(POKE_RainGauge_2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
VAUL_RainGauge_2020$DateTime <- as.POSIXct(VAUL_RainGauge_2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
STRT_RainGauge_2020$DateTime <- as.POSIXct(STRT_RainGauge_2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")

# round to nearest 15 min 
FRCH_RainGauge_2020$DateTime <- lubridate::floor_date(FRCH_RainGauge_2020$DateTime, "15 minutes")
POKE_RainGauge_2020$DateTime <- lubridate::floor_date(POKE_RainGauge_2020$DateTime, "15 minutes")
VAUL_RainGauge_2020$DateTime <- lubridate::floor_date(VAUL_RainGauge_2020$DateTime, "15 minutes")
STRT_RainGauge_2020$DateTime <- lubridate::floor_date(STRT_RainGauge_2020$DateTime, "15 minutes")

# MOOS # 
MOOSstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2020/AllSites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 723),
                          rep("storm2", 327),
                          rep("storm3", 129),
                          rep("storm4", 321),
                          rep("storm5", 240),
                          rep("storm6a", 108),
                          rep("storm6b", 272),
                          rep("storm7a", 276),
                          rep("storm7b", 186),
                          rep("storm8", 195),
                          rep("storm9", 405))

MOOS_storms$DateTime <- as.POSIXct(MOOS_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS.2020.storms.1<- left_join(MOOS_storms, FRCH_RainGauge_2020, by = "DateTime")

names(MOOS.2020.storms.1)[names(MOOS.2020.storms.1) == ''] <- 'x'

MOOS.2020.per.storm.1 <- MOOS.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

MOOS.2020 <- read.csv("~/Documents/Storms/Q_Chem/MOOS/MOOS_chem_2020.csv")
MOOS.2020$DateTime <- as.POSIXct(MOOS.2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
MOOS.2020 <- left_join(MOOS.2020, FRCH_RainGauge_2020, by = "DateTime")
MOOS.2020$week <- rollapplyr(MOOS.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2020$month <- rollapplyr(MOOS.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2020$ThreeMonth <- rollapplyr(MOOS.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

MOOS.2020.1 <- left_join(MOOS.2020.storms.1, MOOS.2020, by = "DateTime") # week month and 3 month precip totals 

MOOS.2020.per.storm.2 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2020.per.storm.3 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2020.per.storm.4 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "turb")

HI.moos.no3.2020 <- left_join(HI.mean.precip.moos.NO3, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.no3.2020 <- left_join(HI.moos.no3.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.no3.2020 <- left_join(HI.moos.no3.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.no3.2020 <- left_join(HI.moos.no3.2020, MOOS.2020.per.storm.4, by = "storm.num")

moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$precip) # model one with just total precip
moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$precip.week) # model one with just total precip
moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$precip.month) # model one with just total precip
moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

aaa <- HI.moos.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bbb <- HI.moos.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ccc <- HI.moos.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ddd <- HI.moos.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.fDOM.2020 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.fDOM.2020 <- left_join(HI.moos.fDOM.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.fDOM.2020 <- left_join(HI.moos.fDOM.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.fDOM.2020 <- left_join(HI.moos.fDOM.2020, MOOS.2020.per.storm.4, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$precip) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$precip.week) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$precip.month) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

eee <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

fff <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ggg <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

hhh <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.SPC.2020 <- left_join(HI.mean.precip.moos.SPC, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.SPC.2020 <- left_join(HI.moos.SPC.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.SPC.2020 <- left_join(HI.moos.SPC.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.SPC.2020 <- left_join(HI.moos.SPC.2020, MOOS.2020.per.storm.4, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$precip) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$precip.week) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$precip.month) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

iii <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

jjj <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

kkk <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

lll <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.turb.2020 <- left_join(HI.mean.precip.moos.turb, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.turb.2020 <- left_join(HI.moos.turb.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.turb.2020 <- left_join(HI.moos.turb.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.turb.2020 <- left_join(HI.moos.turb.2020, MOOS.2020.per.storm.4, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$precip) # model one with just total precip
moos.lm.turb.1 <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$precip.week) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$precip.month) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

mmm <- HI.moos.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

nnn <- HI.moos.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ooo <- HI.moos.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppp <- HI.moos.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- MOOS.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.moos.no3.2.2020 <- left_join(HI.moos.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.no3.2.2020$TOTAL.TIME <- as.numeric(HI.moos.no3.2.2020$TOTAL.TIME)
HI.moos.no3.2.2020$Intensity <- HI.moos.no3.2.2020$precip/HI.moos.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.no3.2 <- lm(HI.moos.no3.2.2020$HI ~ HI.moos.no3.2.2020$precip + HI.moos.no3.2.2020$Intensity) # model one with total precip and intensity 

qqq <- HI.moos.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2020 <- left_join(HI.moos.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.moos.fDOM.2.2020$TOTAL.TIME)
HI.moos.fDOM.2.2020$Intensity <- HI.moos.fDOM.2.2020$precip/HI.moos.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2.2020$HI ~ HI.moos.fDOM.2.2020$precip + HI.moos.fDOM.2.2020$Intensity) # model one with total precip and intensity 

rrr <- HI.moos.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2020 <- left_join(HI.moos.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.moos.SPC.2.2020$TOTAL.TIME)
HI.moos.SPC.2.2020$Intensity <- HI.moos.SPC.2.2020$precip/HI.moos.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.SPC.2 <- lm(HI.moos.SPC.2.2020$HI ~ HI.moos.SPC.2.2020$precip + HI.moos.SPC.2.2020$Intensity) # model one with total precip and intensity 

sss <- HI.moos.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2020 <- left_join(HI.moos.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.turb.2.2020$TOTAL.TIME <- as.numeric(HI.moos.turb.2.2020$TOTAL.TIME)
HI.moos.turb.2.2020$Intensity <- HI.moos.turb.2.2020$precip/HI.moos.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.turb.2 <- lm(HI.moos.turb.2.2020$HI ~ HI.moos.turb.2.2020$precip + HI.moos.turb.2.2020$Intensity) # model one with total precip and intensity 

ttt <- HI.moos.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
MOOS.2020.1$day <- julian(MOOS.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
MOOS.2020.1$day <- as.numeric(MOOS.2020.1$day)
MOOS.2020.per.storm.5 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.moos.no3.2.2020 <- left_join(HI.moos.no3.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2020$HI ~ HI.moos.no3.2.2020$doy)

uuu <- HI.moos.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2020 <- left_join(HI.moos.fDOM.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2020$HI ~ HI.moos.fDOM.2.2020$doy)

utb <- HI.moos.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2020 <- left_join(HI.moos.SPC.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2020$HI ~ HI.moos.SPC.2.2020$doy)

vvv <- HI.moos.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2020 <- left_join(HI.moos.turb.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2020$HI ~ HI.moos.turb.2.2020$doy)

www <- HI.moos.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(aaa,bbb,ccc,ddd,eee,fff,ggg,hhh,iii,jjj,kkk,lll,mmm,nnn,ooo,ppp,qqq,rrr,sss,ttt,uuu,utb,vvv,www,
          ncol = 4)


HI.moos.2020 <- rbind(HI.moos.no3.2.2020, HI.moos.fDOM.2.2020, HI.moos.SPC.2.2020, HI.moos.turb.2.2020) # merging all responses together 
HI.moos.2020$burn <- "unburned" # adding a burn column
HI.moos.2020$pf <- "medium" # adding a pf column

write.csv(HI.moos.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2020.csv")

# FRCH # 
FRCHstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2020/AllSites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 487),
                          rep("storm10a", 255),
                          rep("storm10b", 455),
                          rep("storm11", 91),
                          rep("storm12", 67),
                          rep("storm2", 107),
                          rep("storm3a", 159),
                          rep("storm3b", 431),
                          rep("storm3c", 159),
                          rep("storm4a", 187),
                          rep("storm4b", 203),
                          rep("storm5", 28),
                          rep("storm6", 103),
                          rep("storm7", 339),
                          rep("storm8", 383),
                          rep("storm9a", 139),
                          rep("storm9b", 272))
                          
FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH.2020.storms.1<- left_join(FRCH_storms, FRCH_RainGauge_2020, by = "DateTime")

names(FRCH.2020.storms.1)[names(FRCH.2020.storms.1) == ''] <- 'x'

FRCH.2020.per.storm.1 <- FRCH.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

FRCH.2020 <- read.csv("~/Documents/Storms/Q_Chem/FRCH/FRCH_chem_2020.csv")
FRCH.2020$DateTime <- as.POSIXct(FRCH.2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
FRCH.2020 <- left_join(FRCH.2020, FRCH_RainGauge_2020, by = "DateTime")
FRCH.2020$week <- rollapplyr(FRCH.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2020$month <- rollapplyr(FRCH.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2020$ThreeMonth <- rollapplyr(FRCH.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

FRCH.2020.1 <- left_join(FRCH.2020.storms.1, FRCH.2020, by = "DateTime") # week month and 3 month precip totals 

FRCH.2020.per.storm.2 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2020.per.storm.3 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2020.per.storm.4 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "turb")

HI.frch.no3.2020 <- left_join(HI.mean.precip.frch.NO3, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.no3.2020 <- left_join(HI.frch.no3.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.no3.2020 <- left_join(HI.frch.no3.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.no3.2020 <- left_join(HI.frch.no3.2020, FRCH.2020.per.storm.4, by = "storm.num")

frch.lm.no3 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$precip) # model one with just total precip
frch.lm.no3.2 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$precip.week) # model one with just total precip
frch.lm.no3.3 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$precip.month) # model one with just total precip
frch.lm.no3.4 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

baa <- HI.frch.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bab <- HI.frch.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bcc <- HI.frch.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bdd <- HI.frch.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.fDOM.2020 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.fDOM.2020 <- left_join(HI.frch.fDOM.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.fDOM.2020 <- left_join(HI.frch.fDOM.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.fDOM.2020 <- left_join(HI.frch.fDOM.2020, FRCH.2020.per.storm.4, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

bee <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bff <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bgg <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bhh <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.SPC.2020 <- left_join(HI.mean.precip.frch.SPC, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.SPC.2020 <- left_join(HI.frch.SPC.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.SPC.2020 <- left_join(HI.frch.SPC.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.SPC.2020 <- left_join(HI.frch.SPC.2020, FRCH.2020.per.storm.4, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$ThreeMonth) # model one with just total precip


bii <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bjj <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bkk <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bll <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model


HI.frch.turb.2020 <- left_join(HI.mean.precip.frch.turb, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.turb.2020 <- left_join(HI.frch.turb.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.turb.2020 <- left_join(HI.frch.turb.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.turb.2020 <- left_join(HI.frch.turb.2020, FRCH.2020.per.storm.4, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$ThreeMonth) # model one with just total precip

bmm <- HI.frch.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bnn <- HI.frch.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

boo <- HI.frch.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bpp <- HI.frch.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- FRCH.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.frch.no3.2.2020 <- left_join(HI.frch.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.no3.2.2020$TOTAL.TIME <- as.numeric(HI.frch.no3.2.2020$TOTAL.TIME)
HI.frch.no3.2.2020$Intensity <- HI.frch.no3.2.2020$precip/HI.frch.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.no3.2 <- lm(HI.frch.no3.2.2020$HI ~ HI.frch.no3.2.2020$precip + HI.frch.no3.2.2020$Intensity) # model one with total precip and intensity 

bqq <- HI.frch.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2020 <- left_join(HI.frch.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2020$TOTAL.TIME)
HI.frch.fDOM.2.2020$Intensity <- HI.frch.fDOM.2.2020$precip/HI.frch.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2020$HI ~ HI.frch.fDOM.2.2020$precip + HI.frch.fDOM.2.2020$Intensity) # model one with total precip and intensity 

brr <- HI.frch.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2020 <- left_join(HI.frch.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2020$TOTAL.TIME)
HI.frch.SPC.2.2020$Intensity <- HI.frch.SPC.2.2020$precip/HI.frch.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2020$HI ~ HI.frch.SPC.2.2020$precip + HI.frch.SPC.2.2020$Intensity) # model one with total precip and intensity 

bss <- HI.frch.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2020 <- left_join(HI.frch.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.turb.2.2020$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2020$TOTAL.TIME)
HI.frch.turb.2.2020$Intensity <- HI.frch.turb.2.2020$precip/HI.frch.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.turb.2 <- lm(HI.frch.turb.2.2020$HI ~ HI.frch.turb.2.2020$precip + HI.frch.turb.2.2020$Intensity) # model one with total precip and intensity 

btt <- HI.frch.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
FRCH.2020.1$day <- julian(FRCH.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
FRCH.2020.1$day <- as.numeric(FRCH.2020.1$day)
FRCH.2020.per.storm.5 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.frch.no3.2.2020 <- left_join(HI.frch.no3.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2020$HI ~ HI.frch.no3.2.2020$doy)

buu <- HI.frch.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2020 <- left_join(HI.frch.fDOM.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2020$HI ~ HI.frch.fDOM.2.2020$doy)

btb <- HI.frch.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2020 <- left_join(HI.frch.SPC.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2020$HI ~ HI.frch.SPC.2.2020$doy)

bvv <- HI.frch.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2020 <- left_join(HI.frch.turb.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2020$HI ~ HI.frch.turb.2.2020$doy)

bww <- HI.frch.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(baa,bab,bcc,bdd,bee,bff,bgg,bhh,bii,bjj,bkk,bll,bmm,bnn,boo,bpp,bqq,brr,bss,bt,buu,btb,bvv,bww,
          ncol = 4)

HI.frch.2020 <- rbind(HI.frch.no3.2.2020, HI.frch.fDOM.2.2020, HI.frch.SPC.2.2020, HI.frch.turb.2.2020) # merging all responses together 
HI.frch.2020$burn <- "unburned" # adding a burn column
HI.frch.2020$pf <- "medium" # adding a pf column

write.csv(HI.frch.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.frch.2020.csv")

# POKE # 
POKEstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2020/AllSites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(rep("storm1", 95),
                          rep("storm10", 107),
                          rep("storm11", 199),
                          rep("storm12", 311),
                          rep("storm13", 183),
                          rep("storm14", 383),
                          rep("storm15", 339),
                          rep("storm16", 95),
                          rep("storm17", 115),
                          rep("storm18", 95),
                          rep("storm19", 135),
                          rep("storm2", 87),
                          rep("storm20", 139),
                          rep("storm21", 251),
                          rep("storm22a", 111),
                          rep("storm22b", 208),
                          rep("storm3", 124),
                          rep("storm4a", 99),
                          rep("storm4b", 91),
                          rep("storm5", 371),
                          rep("storm6", 95),
                          rep("storm7", 131),
                          rep("storm8", 131),
                          rep("storm9", 262))

POKE_storms$DateTime <- as.POSIXct(POKE_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
POKE.2020.storms.1<- left_join(POKE_storms, POKE_RainGauge_2020, by = "DateTime")

names(POKE.2020.storms.1)[names(POKE.2020.storms.1) == ''] <- 'x'

POKE.2020.per.storm.1 <- POKE.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

POKE.2020 <- read.csv("~/Documents/Storms/Q_Chem/POKE/POKE_chem_2020.csv")
POKE.2020$DateTime <- as.POSIXct(POKE.2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
POKE.2020 <- left_join(POKE.2020, POKE_RainGauge_2020, by = "DateTime")
POKE.2020$week <- rollapplyr(POKE.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2020$month <- rollapplyr(POKE.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2020$ThreeMonth <- rollapplyr(POKE.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

POKE.2020.1 <- left_join(POKE.2020.storms.1, POKE.2020, by = "DateTime") # week month and 3 month precip totals 

POKE.2020.per.storm.2 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
POKE.2020.per.storm.3 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
POKE.2020.per.storm.4 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.poke.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "NO3")
HI.mean.precip.poke.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "fDOM")
HI.mean.precip.poke.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "SPC")
HI.mean.precip.poke.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "turb")

HI.poke.no3.2020 <- left_join(HI.mean.precip.poke.NO3, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.no3.2020 <- left_join(HI.poke.no3.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.no3.2020 <- left_join(HI.poke.no3.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.no3.2020 <- left_join(HI.poke.no3.2020, POKE.2020.per.storm.4, by = "storm.num")

poke.lm.no3 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$precip) # model one with just total precip
poke.lm.no3.2 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$precip.week) # model one with just total precip
poke.lm.no3.3 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$precip.month) # model one with just total precip
poke.lm.no3.4 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$ThreeMonth) # model one with just total precip

poke.formula <- y ~ x

ppa <- HI.poke.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppb <- HI.poke.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppc <- HI.poke.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppd <- HI.poke.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.fDOM.2020 <- left_join(HI.mean.precip.poke.fDOM, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.fDOM.2020 <- left_join(HI.poke.fDOM.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.fDOM.2020 <- left_join(HI.poke.fDOM.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.fDOM.2020 <- left_join(HI.poke.fDOM.2020, POKE.2020.per.storm.4, by = "storm.num")

poke.lm.fDOM <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$precip) # model one with just total precip
poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$precip.week) # model one with just total precip
poke.lm.fDOM.3 <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$precip.month) # model one with just total precip
poke.lm.fDOM.4 <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$ThreeMonth) # model one with just total precip

ppe <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppf <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppg <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

pph <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Three-week Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.SPC.2020 <- left_join(HI.mean.precip.poke.SPC, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.mean.precip.poke.SPC, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.mean.precip.poke.SPC, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.mean.precip.poke.SPC, POKE.2020.per.storm.4, by = "storm.num")

poke.lm.SPC <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$precip) # model one with just total precip
poke.lm.SPC.2 <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$precip.week) # model one with just total precip
poke.lm.SPC.3 <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$precip.month) # model one with just total precip
poke.lm.SPC.4 <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$ThreeMonth) # model one with just total precip

ppi <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppj <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppk <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppl <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.turb.2020 <- left_join(HI.mean.precip.poke.turb, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.mean.precip.poke.turb, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.mean.precip.poke.turb, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.mean.precip.poke.turb, POKE.2020.per.storm.4, by = "storm.num")

poke.lm.turb <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$precip) # model one with just total precip
poke.lm.turb.2 <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$precip.week) # model one with just total precip
poke.lm.turb.3 <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$precip.month) # model one with just total precip
poke.lm.turb.4 <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$ThreeMonth) # model one with just total precip

ppm <- HI.poke.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppn <- HI.poke.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppo <- HI.poke.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppp <- HI.poke.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- POKE.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.poke.no3.2.2020 <- left_join(HI.poke.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.no3.2.2020$TOTAL.TIME <- as.numeric(HI.poke.no3.2.2020$TOTAL.TIME)
HI.poke.no3.2.2020$Intensity <- HI.poke.no3.2.2020$precip/HI.poke.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.no3.2 <- lm(HI.poke.no3.2.2020$HI ~ HI.poke.no3.2.2020$precip + HI.poke.no3.2.2020$Intensity) # model one with total precip and intensity 

ppq <- HI.poke.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2020 <- left_join(HI.poke.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.poke.fDOM.2.2020$TOTAL.TIME)
HI.poke.fDOM.2.2020$Intensity <- HI.poke.fDOM.2.2020$precip/HI.poke.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2.2020$HI ~ HI.poke.fDOM.2.2020$precip + HI.poke.fDOM.2.2020$Intensity) # model one with total precip and intensity 

ppr <- HI.poke.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2020 <- left_join(HI.poke.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.poke.SPC.2.2020$TOTAL.TIME)
HI.poke.SPC.2.2020$Intensity <- HI.poke.SPC.2.2020$precip/HI.poke.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.SPC.2 <- lm(HI.poke.SPC.2.2020$HI ~ HI.poke.SPC.2.2020$precip + HI.poke.SPC.2.2020$Intensity) # model one with total precip and intensity 

pps <- HI.poke.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2020 <- left_join(HI.poke.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.turb.2.2020$TOTAL.TIME <- as.numeric(HI.poke.turb.2.2020$TOTAL.TIME)
HI.poke.turb.2.2020$Intensity <- HI.poke.turb.2.2020$precip/HI.poke.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.turb.2 <- lm(HI.poke.turb.2.2020$HI ~ HI.poke.turb.2.2020$precip + HI.poke.turb.2.2020$Intensity) # model one with total precip and intensity 

ppt <- HI.poke.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
POKE.2020.1$day <- julian(POKE.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
POKE.2020.1$day <- as.numeric(POKE.2020.1$day)
POKE.2020.per.storm.5 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.poke.no3.2.2020 <- left_join(HI.poke.no3.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.no3.5 <- lm(HI.poke.no3.2.2020$HI ~ HI.poke.no3.2.2020$doy)

ppu <- HI.poke.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2020 <- left_join(HI.poke.fDOM.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.fDOM.5 <- lm(HI.poke.fDOM.2.2020$HI ~ HI.poke.fDOM.2.2020$doy)

ppv <- HI.poke.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2020 <- left_join(HI.poke.SPC.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.SPC.5 <- lm(HI.poke.SPC.2.2020$HI ~ HI.poke.SPC.2.2020$doy)

ppw <- HI.poke.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2020 <- left_join(HI.poke.turb.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.turb.5 <- lm(HI.poke.turb.2.2020$HI ~ HI.poke.turb.2.2020$doy)

ppx <- HI.poke.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(ppa,ppb,ppc,ppd,ppe,ppf,ppg,pph,ppq,ppr,ppu,ppv,
          ncol = 4)

HI.poke.2020 <- rbind(HI.poke.no3.2.2020, HI.poke.fDOM.2.2020) # merging all responses together 
HI.poke.2020$burn <- "burned" # adding a burn column
HI.poke.2020$pf <- "medium" # adding a pf column

write.csv(HI.poke.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.poke.2020.csv")

# VAUL # 
VAULstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2020/AllSites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(rep("storm10", 195),
                          rep("storm11", 400),
                          rep("storm12", 171),
                          rep("storm13", 319),
                          rep("storm14", 227),
                          rep("storm1a", 107),
                          rep("storm1b", 238),
                          rep("storm1c", 502),
                          rep("storm2", 210),
                          rep("storm3", 338),
                          rep("storm4", 322),
                          rep("storm5", 222),
                          rep("storm6a", 107),
                          rep("storm6b", 507),
                          rep("storm7", 284),
                          rep("storm8", 91),
                          rep("storm9", 91))



VAUL_storms$DateTime <- as.POSIXct(VAUL_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
VAUL.2020.storms.1<- left_join(VAUL_storms, VAUL_RainGauge_2020, by = "DateTime")

names(VAUL.2020.storms.1)[names(VAUL.2020.storms.1) == ''] <- 'x'

VAUL.2020.per.storm.1 <- VAUL.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

VAUL.2020 <- read.csv("~/Documents/Storms/Q_Chem/VAUL/VAUL_chem_2020.csv")
VAUL.2020$DateTime <- as.POSIXct(VAUL.2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
VAUL.2020 <- left_join(VAUL.2020, FRCH_RainGauge_2020, by = "DateTime")
VAUL.2020$week <- rollapplyr(VAUL.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2020$month <- rollapplyr(VAUL.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2020$ThreeMonth <- rollapplyr(VAUL.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

VAUL.2020.1 <- left_join(VAUL.2020.storms.1, VAUL.2020, by = "DateTime") # week month and 3 month precip totals 

VAUL.2020.per.storm.2 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
VAUL.2020.per.storm.3 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
VAUL.2020.per.storm.4 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.vaul.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "NO3")
HI.mean.precip.vaul.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "fDOM")
HI.mean.precip.vaul.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "SPC")
HI.mean.precip.vaul.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "turb")

HI.vaul.no3.2020 <- left_join(HI.mean.precip.vaul.NO3, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.no3.2020 <- left_join(HI.vaul.no3.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.no3.2020 <- left_join(HI.vaul.no3.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.no3.2020 <- left_join(HI.vaul.no3.2020, VAUL.2020.per.storm.4, by = "storm.num")

vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$precip) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$precip.week) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$precip.month) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$ThreeMonth) # model one with just total precip

vaul.formula <- y ~ x

daa <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dbb <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dcc <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

dcd <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.vaul.fDOM.2020 <- left_join(HI.mean.precip.vaul.fDOM, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.fDOM.2020 <- left_join(HI.vaul.fDOM.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.fDOM.2020 <- left_join(HI.vaul.fDOM.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.fDOM.2020 <- left_join(HI.vaul.fDOM.2020, VAUL.2020.per.storm.4, by = "storm.num")

vaul.lm.fDOM <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$precip) # model one with just total precip
vaul.lm.fDOM.1 <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$precip.week) # model one with just total precip
vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$precip.month) # model one with just total precip
vaul.lm.fDOM.3 <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$ThreeMonth) # model one with just total precip

dee <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dff <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dgg <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

dhh <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.vaul.SPC.2020 <- left_join(HI.mean.precip.vaul.SPC, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.SPC.2020 <- left_join(HI.vaul.SPC.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.SPC.2020 <- left_join(HI.vaul.SPC.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.SPC.2020 <- left_join(HI.vaul.SPC.2020, VAUL.2020.per.storm.4, by = "storm.num")

vaul.lm.SPC <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$precip) # model one with just total precip
vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$precip.week) # model one with just total precip
vaul.lm.SPC.3 <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$precip.month) # model one with just total precip
vaul.lm.SPC.4 <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$ThreeMonth) # model one with just total precip

dii <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

djj <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dkk <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

dll <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.turb.2020 <- left_join(HI.mean.precip.vaul.turb, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.turb.2020 <- left_join(HI.vaul.turb.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.turb.2020 <- left_join(HI.vaul.turb.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.turb.2020 <- left_join(HI.vaul.turb.2020, VAUL.2020.per.storm.4, by = "storm.num")

vaul.lm.turb <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$precip) # model one with just total precip
vaul.lm.turb.2 <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$precip.week) # model one with just total precip
vaul.lm.turb.3 <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$precip.month) # model one with just total precip
vaul.lm.turb.4 <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$ThreeMonth) # model one with just total precip

dmm <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dnn <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

doo <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

dpp <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- VAUL.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.vaul.no3.2.2020 <- left_join(HI.vaul.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.no3.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.no3.2.2020$TOTAL.TIME)
HI.vaul.no3.2.2020$Intensity <- HI.vaul.no3.2.2020$precip/HI.vaul.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.no3.2 <- lm(HI.vaul.no3.2.2020$HI ~ HI.vaul.no3.2.2020$precip + HI.vaul.no3.2.2020$Intensity) # model one with total precip and intensity 

dqq <- HI.vaul.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2020 <- left_join(HI.vaul.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.fDOM.2.2020$TOTAL.TIME)
HI.vaul.fDOM.2.2020$Intensity <- HI.vaul.fDOM.2.2020$precip/HI.vaul.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2.2020$HI ~ HI.vaul.fDOM.2.2020$precip + HI.vaul.fDOM.2.2020$Intensity) # model one with total precip and intensity 

drr <- HI.vaul.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2020 <- left_join(HI.vaul.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.SPC.2.2020$TOTAL.TIME)
HI.vaul.SPC.2.2020$Intensity <- HI.vaul.SPC.2.2020$precip/HI.vaul.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2.2020$HI ~ HI.vaul.SPC.2.2020$precip + HI.vaul.SPC.2.2020$Intensity) # model one with total precip and intensity 

dss <- HI.vaul.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2020 <- left_join(HI.vaul.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.turb.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.turb.2.2020$TOTAL.TIME)
HI.vaul.turb.2.2020$Intensity <- HI.vaul.turb.2.2020$precip/HI.vaul.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.turb.2 <- lm(HI.vaul.turb.2.2020$HI ~ HI.vaul.turb.2.2020$precip + HI.vaul.turb.2.2020$Intensity) # model one with total precip and intensity 

dtt <- HI.vaul.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
VAUL.2020.1$day <- julian(VAUL.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
VAUL.2020.1$day <- as.numeric(VAUL.2020.1$day)
VAUL.2020.per.storm.5 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.vaul.no3.2.2020 <- left_join(HI.vaul.no3.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.no3.5 <- lm(HI.vaul.no3.2.2020$HI ~ HI.vaul.no3.2.2020$doy)

duu <- HI.vaul.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2020 <- left_join(HI.vaul.fDOM.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.fDOM.5 <- lm(HI.vaul.fDOM.2.2020$HI ~ HI.vaul.fDOM.2.2020$doy)

dtb <- HI.vaul.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2020 <- left_join(HI.vaul.SPC.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.SPC.5 <- lm(HI.vaul.SPC.2.2020$HI ~ HI.vaul.SPC.2.2020$doy)

dvv <- HI.vaul.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2020 <- left_join(HI.vaul.turb.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.turb.5 <- lm(HI.vaul.turb.2.2020$HI ~ HI.vaul.turb.2.2020$doy)

dww <- HI.vaul.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(daa,dbb,dcc,dcd,dee,dff,dgg,dhh,dii,djj,dkk,dll,dmm,dnn,doo,dpp,dqq,drr,dss,dtt, duu,dtb,dvv,dww,
          ncol = 4)

HI.vaul.2020 <- rbind(HI.vaul.no3.2.2020, HI.vaul.fDOM.2.2020, HI.vaul.SPC.2.2020, HI.vaul.turb.2.2020) # merging all responses together 
HI.vaul.2020$burn <- "unburned" # adding a burn column
HI.vaul.2020$pf <- "continuous" # adding a pf column

write.csv(HI.vaul.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.vaul.2020.csv")

# STRT # 
STRTstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2020/AllSites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm10", 246),
                          rep("storm1a", 95),
                          rep("storm1b", 157),
                          rep("storm1c", 105),
                          rep("storm1d", 78),
                          rep("storm1e", 476),
                          rep("storm2", 166),
                          rep("storm3", 418),
                          rep("storm4a", 152),
                          rep("storm4b", 322),
                          rep("storm5", 250),
                          rep("storm6", 90),
                          rep("storm7a", 98),
                          rep("storm7b", 95),
                          rep("storm8", 178),
                          rep("storm9a", 294),
                          rep("storm9b", 134),
                          rep("storm9c", 482))

STRT_storms$DateTime <- as.POSIXct(STRT_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
STRT.2020.storms.1<- left_join(STRT_storms, STRT_RainGauge_2020, by = "DateTime")

names(STRT.2020.storms.1)[names(STRT.2020.storms.1) == ''] <- 'x'

STRT.2020.per.storm.1 <- STRT.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

STRT.2020 <- read.csv("~/Documents/Storms/Q_Chem/STRT/STRT_chem_2020.csv")
STRT.2020$DateTime <- as.POSIXct(STRT.2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
STRT.2020 <- left_join(STRT.2020, STRT_RainGauge_2020, by = "DateTime")
STRT.2020$week <- rollapplyr(STRT.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2020$month <- rollapplyr(STRT.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2020$ThreeMonth <- rollapplyr(STRT.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)

STRT.2020.1 <- left_join(STRT.2020.storms.1, STRT.2020, by = "DateTime") # week month and 3 month precip totals 

STRT.2020.per.storm.2 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
STRT.2020.per.storm.3 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
STRT.2020.per.storm.4 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.strt.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "NO3")
HI.mean.precip.strt.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "fDOM")
HI.mean.precip.strt.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "SPC")
HI.mean.precip.strt.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "turb")

HI.strt.no3.2020 <- left_join(HI.mean.precip.strt.NO3, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.no3.2020 <- left_join(HI.strt.no3.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.no3.2020 <- left_join(HI.strt.no3.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.no3.2020 <- left_join(HI.strt.no3.2020, STRT.2020.per.storm.4, by = "storm.num")

strt.lm.no3 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$precip) # model one with just total precip
strt.lm.no3.2 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$precip.week) # model one with just total precip
strt.lm.no3.3 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$precip.month) # model one with just total precip
strt.lm.no3.4 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$ThreeMonth) # model one with just total precip

strt.formula <- y ~ x

eaa <- HI.strt.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ebb <- HI.strt.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ecc <- HI.strt.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

edd <- HI.strt.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Three Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.fDOM.2020 <- left_join(HI.mean.precip.strt.fDOM, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.fDOM.2020 <- left_join(HI.strt.fDOM.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.fDOM.2020 <- left_join(HI.strt.fDOM.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.fDOM.2020 <- left_join(HI.strt.fDOM.2020, STRT.2020.per.storm.4, by = "storm.num")

strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$precip) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$precip.week) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$precip.month) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$ThreeMonth) # model one with just total precip

ede <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

eff <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

egg <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ehh <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.SPC.2020 <- left_join(HI.mean.precip.strt.SPC, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.SPC.2020 <- left_join(HI.strt.SPC.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.SPC.2020 <- left_join(HI.strt.SPC.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.SPC.2020 <- left_join(HI.strt.SPC.2020, STRT.2020.per.storm.4, by = "storm.num")

strt.lm.SPC <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$precip) # model one with just total precip
strt.lm.SPC.2 <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$precip.week) # model one with just total precip
strt.lm.SPC.3 <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$precip.month) # model one with just total precip
strt.lm.SPC.4 <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$ThreeMonth) # model one with just total precip

eii <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ejj <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ekk <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ell <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.turb.2020 <- left_join(HI.mean.precip.strt.turb, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.turb.2020 <- left_join(HI.strt.turb.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.turb.2020 <- left_join(HI.strt.turb.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.turb.2020 <- left_join(HI.strt.turb.2020, STRT.2020.per.storm.4, by = "storm.num")

strt.lm.turb <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$precip) # model one with just total precip
strt.lm.turb.2 <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$precip.week) # model one with just total precip
strt.lm.turb.3 <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$precip.month) # model one with just total precip
strt.lm.turb.4 <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$ThreeMonth) # model one with just total precip

emm <- HI.strt.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

enn <- HI.strt.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

eoo <- HI.strt.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

epp <- HI.strt.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- STRT.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.strt.no3.2.2020 <- left_join(HI.strt.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.no3.2.2020$TOTAL.TIME <- as.numeric(HI.strt.no3.2.2020$TOTAL.TIME)
HI.strt.no3.2.2020$Intensity <- HI.strt.no3.2.2020$precip/HI.strt.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.no3.2 <- lm(HI.strt.no3.2.2020$HI ~ HI.strt.no3.2.2020$precip + HI.strt.no3.2.2020$Intensity) # model one with total precip and intensity 

eqq <- HI.strt.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2020 <- left_join(HI.strt.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.strt.fDOM.2.2020$TOTAL.TIME)
HI.strt.fDOM.2.2020$Intensity <- HI.strt.fDOM.2.2020$precip/HI.strt.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2.2020$HI ~ HI.strt.fDOM.2.2020$precip + HI.strt.fDOM.2.2020$Intensity) # model one with total precip and intensity 

err <- HI.strt.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2020 <- left_join(HI.strt.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.strt.SPC.2.2020$TOTAL.TIME)
HI.strt.SPC.2.2020$Intensity <- HI.strt.SPC.2.2020$precip/HI.strt.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.SPC.2 <- lm(HI.strt.SPC.2.2020$HI ~ HI.strt.SPC.2.2020$precip + HI.strt.SPC.2.2020$Intensity) # model one with total precip and intensity 

ess <- HI.strt.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2020 <- left_join(HI.strt.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.turb.2.2020$TOTAL.TIME <- as.numeric(HI.strt.turb.2.2020$TOTAL.TIME)
HI.strt.turb.2.2020$Intensity <- HI.strt.turb.2.2020$precip/HI.strt.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.turb.2 <- lm(HI.strt.turb.2.2020$HI ~ HI.strt.turb.2.2020$precip + HI.strt.turb.2.2020$Intensity) # model one with total precip and intensity 

ett <- HI.strt.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
STRT.2020.1$day <- julian(STRT.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
STRT.2020.1$day <- as.numeric(STRT.2020.1$day)
STRT.2020.per.storm.5 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.strt.no3.2.2020 <- left_join(HI.strt.no3.2.2020, STRT.2020.per.storm.5, by = "storm.num")
strt.lm.no3.5 <- lm(HI.strt.no3.2.2020$HI ~ HI.strt.no3.2.2020$doy)

euu <- HI.strt.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2020 <- left_join(HI.strt.fDOM.2.2020, STRT.2020.per.storm.5, by = "storm.num")
strt.lm.fDOM.5 <- lm(HI.strt.fDOM.2.2020$HI ~ HI.strt.fDOM.2.2020$doy)

etb <- HI.strt.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2020 <- left_join(HI.strt.SPC.2.2020, STRT.2020.per.storm.5, by = "storm.num")
sttrt.lm.SPC.5 <- lm(HI.strt.SPC.2.2020$HI ~ HI.strt.SPC.2.2020$doy)

evv <- HI.strt.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2020 <- left_join(HI.strt.turb.2.2020, STRT.2020.per.storm.5, by = "storm.num")
strt.lm.turb.5 <- lm(HI.strt.turb.2.2020$HI ~ HI.strt.turb.2.2020$doy)

eww <- HI.strt.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(eaa,ebb,ecc,edd,ede,eff,egg,ehh,eii,ejj,ekk,ell,emm,enn,eoo,epp,eqq,err,ess,ett,euu,etb,evv,eww,
          ncol = 4)

# HI.strt.2020 <- rbind(HI.strt.no3.2.2020, HI.strt.fDOM.2.2020, HI.strt.SPC.2.2020, HI.strt.turb.2.2020) # merging all responses together 
# HI.strt.2020$burn <- "burned" # adding a burn column
# HI.strt.2020$pf <- "high" # adding a pf column

write.csv(HI.strt.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.strt.2020.csv")

HI.2020 <- rbind(HI.moos.2020, HI.frch.2020, HI.vaul.2020, HI.strt.2020, HI.poke.2020)

################# combining data sets #######################
# combining years
HI.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.csv")
HI.2019 <- HI.2019[,-1]
HI.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.2020.csv")
HI.2020 <- HI.2020[,-1]

HI.all <- rbind(HI.2019, HI.2020)


# plot HI against antecedent conditions (week, intensity, month etc with burn and unburned catchments
HI.2019.1 <- HI.2019
HI.2019.1$burn <- as.factor(HI.2019.1$burn)

FRCH.fDOM <- subset(HI.mean, site.ID == "FRCH" & response == "fDOM")

HI.2019.1$burn <- as.character(HI.2019.1$burn)
HI.all$year <- as.character(HI.all$year)

alla <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allb <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allc <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

alld <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#precip month
alle <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allf <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allg <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allh <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# Intensity
alli <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allj <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allk <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

alll <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year
allm <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

alln <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allo <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

allp <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = burn,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

plot_grid(alla,allb,allc,alld,alle,allf,allg,allh,alli,allj,allk,alll,allm,alln,allo,allp,
          ncol = 4)
  
# permafrost #
pfa <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfb <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfc <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfd <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = precip.week, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#precip month
pfe <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pff <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfg <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfh <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = precip.month, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# Intensity
pfi <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfj <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfk <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfl <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = Intensity, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year
pfm <- HI.all %>% filter(response == "NO3") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("NO3") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfn <- HI.all %>% filter(response == "fDOM") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("fDOM") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfo <- HI.all %>% filter(response == "SPC") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("SPC") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

pfp <- HI.all %>% filter(response == "turb") %>% 
  ggplot(aes(x = doy, 
             y = HI,
             color = pf,
             shape = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("turb") +
  xlab("Day of Year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model

plot_grid(pfa,pfb,pfc,pfd,pfe,pff,pfg,pfh,pfi,pfj,pfk,pfl,pfm,pfn,pfo,pfp,
          ncol = 4)

########################### MOOS ###########################
HI.moos.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2019.csv") # just mean values of HI 
HI.moos.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2020.csv")# just mean values of HI 

MOOS.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/MOOS/MOOS.HI.df.csv") # has 2% intervals to generate error bars
MOOS.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/MOOS/MOOS.HI.df.csv") # has 2% intervals to generate error bars

moos.2019 <- left_join(HI.moos.2019, MOOS.HI.df.2019, by = "storm.num") # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

moos.2020 <- left_join(HI.moos.2020, MOOS.HI.df.2020, by = "storm.num")# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

HI.moos.2019.2020 <- rbind(moos.2019, moos.2020) # joining 2019 and 2020 data 
HI.moos.2019.2020$year <- as.character(HI.moos.2019.2020$year) # making year a character so it wont recognize it as a continuous variable

write.csv(HI.moos.2019.2020, "~/Documents/Storms/Output_from_analysis/HI.MOOS.csv")

median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

moos.formula <- y ~ x

a <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = precip,
             y = HI.y,
             shape = year)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("MOOS Storm-Precip") +
  xlab("Storm-Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = a + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
moos.storm.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               colour = "black", width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               colour = "black", size = 3)

b <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI.y,
             shape = year)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("MOOS One-Week Precip") +
  xlab("One-Week Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = b + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
moos.week.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               colour = "black", width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               colour = "black", size = 3)

c <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI.y,
             shape = year)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("MOOS One-Month Precip") +
  xlab("One-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = c + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
moos.month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               colour = "black", width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               colour = "black", size = 3)

d <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI.y,
             shape = year)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("MOOS Three-Month Precip") +
  xlab("Three-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = d + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
moos.3month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               colour = "black", width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               colour = "black", size = 3)

e <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI.y,
             shape = year)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("MOOS Storm Intensity") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = e + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
moos.intensity.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               colour = "black", width = 0.2, size=.2) + 
  stat_summary(fun.y = median, geom = "point", 
               colour = "black", size = 3)

f <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = doy,
             y = HI.y,
             shape = year)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("MOOS Day of Year") +
  xlab("doy") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = f + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
moos.doy.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               colour = "black", width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               colour = "black", size = 3)
plot_grid(moos.storm.precip, moos.week.precip, moos.month.precip,
          moos.3month.precip, moos.intensity.precip, moos.doy.precip,
          ncol = 3)


####################### FRCH #################################
HI.frch.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.frch.2019.csv") # just mean values of HI 
HI.frch.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.frch.2020.csv")# just mean values of HI 

FRCH.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/FRCH/FRCH.HI.doy.df.csv") # has 2% intervals to generate error bars
FRCH.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/FRCH/FRCH.HI.doy.df.csv") # has 2% intervals to generate error bars

frch.2019 <- left_join(HI.frch.2019, FRCH.HI.df.2019, by = "storm.num") # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

frch.2020 <- left_join(HI.frch.2020, FRCH.HI.df.2020, by = "storm.num")# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

HI.frch.2019.2020 <- rbind(frch.2019, frch.2020) # joining 2019 and 2020 data 
HI.frch.2019.2020$year <- as.character(HI.frch.2019.2020$year) # making year a character so it wont recognize it as a continuous variable

write.csv(HI.frch.2019.2020, "~/Documents/Storms/Output_from_analysis/07_Models/HI.FRCH.csv")

median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

frch.formula <- y ~ x

a <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = precip,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("FRCH Storm-Precip") +
  xlab("Storm-Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = a + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
frch.storm.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

b <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = frch.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("FRCH One-Week Precip") +
  xlab("One-Week Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = b + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
frch.week.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = frch.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("FRCH One-Month Precip") +
  xlab("One-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = c + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
frch.month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = frch.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("FRCH Three-Month Precip") +
  xlab("Three-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = d + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

frch.3month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("FRCH Storm Intensity") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = e + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

frch.intensity.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = doy.x,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = frch.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("FRCH Day of Year") +
  xlab("doy") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = f + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
frch.doy.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
plot_grid(frch.storm.precip, frch.week.precip, frch.month.precip,
          frch.3month.precip, frch.intensity.precip, frch.doy.precip,
          ncol = 3)

####################### VAUL #################################
HI.vaul.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.vaul.2019.csv") # just mean values of HI 
HI.vaul.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.vaul.2020.csv")# just mean values of HI 

VAUL.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/VAUL/VAUL.HI.doy.df.csv") # has 2% intervals to generate error bars
VAUL.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/VAUL/VAUL.HI.doy.df.csv") # has 2% intervals to generate error bars

vaul.2019 <- left_join(HI.vaul.2019, VAUL.HI.df.2019, by = "storm.num") # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

vaul.2020 <- left_join(HI.vaul.2020, VAUL.HI.df.2020, by = "storm.num")# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

HI.vaul.2019.2020 <- rbind(vaul.2019, vaul.2020) # joining 2019 and 2020 data 
HI.vaul.2019.2020$year <- as.character(HI.vaul.2019.2020$year) # making year a character so it wont recognize it as a continuous variable

write.csv(HI.vaul.2019.2020, "~/Documents/Storms/Output_from_analysis/07_Models/HI.VAUL.csv")

median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

vaul.formula <- y ~ x

a <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = precip,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("VAUL Storm-Precip") +
  xlab("Storm-Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = a + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
vaul.storm.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

b <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("VAUL One-Week Precip") +
  xlab("One-Week Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = b + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
vaul.week.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("VAUL One-Month Precip") +
  xlab("One-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = c + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
vaul.month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("VAUL Three-Month Precip") +
  xlab("Three-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = d + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

vaul.3month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("VAUL Storm Intensity") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = e + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

vaul.intensity.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = doy.x,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("VAUL Day of Year") +
  xlab("doy") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = f + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
vaul.doy.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
plot_grid(vaul.storm.precip, vaul.week.precip, vaul.month.precip,
          vaul.3month.precip, vaul.intensity.precip, vaul.doy.precip,
          ncol = 3)

####################### STRT #################################
HI.strt.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.strt.2019.csv") # just mean values of HI 
HI.strt.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.strt.2020.csv")# just mean values of HI 

STRT.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/STRT/STRT.HI.df.csv") # has 2% intervals to generate error bars
STRT.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/STRT/STRT.HI..df.csv") # has 2% intervals to generate error bars

strt.2019 <- left_join(HI.strt.2019, STRT.HI.df.2019, by = "storm.num") # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

strt.2020 <- left_join(HI.strt.2020, STRT.HI.df.2020, by = "storm.num")# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

HI.strt.2019.2020 <- rbind(strt.2019, strt.2020) # joining 2019 and 2020 data 
HI.strt.2019.2020$year <- as.character(HI.strt.2019.2020$year) # making year a character so it wont recognize it as a continuous variable

write.csv(HI.strt.2019.2020, "~/Documents/Storms/Output_from_analysis/07_Models/HI.STRT.csv")

median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

strt.formula <- y ~ x

a <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = precip,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("STRT Storm-Precip") +
  xlab("Storm-Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = a + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
vaul.storm.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

b <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = strt.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("STRT One-Week Precip") +
  xlab("One-Week Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = b + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
vaul.week.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = strt.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("STRT One-Month Precip") +
  xlab("One-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = c + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
vaul.month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = strt.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("STRT Three-Month Precip") +
  xlab("Three-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = d + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

strt.3month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = strt.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("STRT Storm Intensity") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = e + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

strt.intensity.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = doy.x,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = strt.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("STRT Day of Year") +
  xlab("doy") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = f + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
strt.doy.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
plot_grid(strt.storm.precip, strt.week.precip, strt.month.precip,
          strt.3month.precip, strt.intensity.precip, strt.doy.precip,
          ncol = 3)

################ POKE ###############################
HI.poke.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.poke.2019.csv") # just mean values of HI 
HI.poke.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.poke.2020.csv")# just mean values of HI 

POKE.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/POKE/POKE.HI.df.csv") # has 2% intervals to generate error bars
POKE.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/POKE/POKE.HI..df.csv") # has 2% intervals to generate error bars

poke.2019 <- left_join(HI.poke.2019, POKE.HI.df.2019, by = "storm.num") # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

poke.2020 <- left_join(HI.poke.2020, POKE.HI.df.2020, by = "storm.num")# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

HI.poke.2019.2020 <- rbind(poke.2019, poke.2020) # joining 2019 and 2020 data 
HI.poke.2019.2020$year <- as.character(HI.poke.2019.2020$year) # making year a character so it wont recognize it as a continuous variable

write.csv(HI.poke.2019.2020, "~/Documents/Storms/Output_from_analysis/07_Models/HI.POKE.csv")

median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

poke.formula <- y ~ x

a <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = precip,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE Storm-Precip") +
  xlab("Storm-Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = a + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
poke.storm.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

b <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = poke.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE One-Week Precip") +
  xlab("One-Week Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = b + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
poke.week.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = poke.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE One-Month Precip") +
  xlab("One-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = c + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
poke.month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = poke.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE Three-Month Precip") +
  xlab("Three-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = d + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

poke.3month.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = poke.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE Storm Intensity") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = e + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

poke.intensity.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = doy.x,
             y = HI.y)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = poke.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE Day of Year") +
  xlab("doy") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response.y)

g1 = f + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.00001, size=3) + 
  theme(axis.text.x = element_text(angle = 90))+  labs(x="")  +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
poke.doy.precip <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
plot_grid(poke.storm.precip, poke.week.precip, poke.month.precip,
          poke.3month.precip, poke.intensity.precip, poke.doy.precip,
          ncol = 3)

################# 2019 and 2020  chem ################
DOD.2019 <- read.csv("~/Documents/DoD_Discharge/Discharge_Chem/2019/DOD.2019.csv")

fa <- ggplot(data = DOD.2019, aes(x = DateTime , 
           y = nitrateuM,
           color = Site,
           group = 1)) +
  geom_line() +
  ggtitle("2019") +
  xlab("") +
  ylab("Nitrate (uM)") +
  theme_classic() # plot model 

fb <- ggplot(data = DOD.2019, aes(x = DateTime , 
                            y = fDOM.QSU.mn,
                            color = Site)) +
  geom_line() +
  xlab("") +
  ylab("fDOM (QSU)") +
  theme_classic() # plot model 

fc <- ggplot(data = DOD.2019, aes(x = DateTime, 
                            y = SpCond.uScm.mn,
                            color = Site)) +
  geom_line() +
  xlab("") +
  ylab("SPC") +
  theme_classic() # plot model 

fd <- ggplot(data = DOD.2019, aes(x = DateTime, 
                            y = Turbidity.FNU.mn,
                            color = Site)) +
  geom_line() +
  xlab("Date") +
  ylab("SPC") +
  theme_classic() # plot model 


DOD.2020 <- read.csv("~/Documents/DoD_Discharge/Discharge_Chem/2020/DOD.2020.csv")

fe <- ggplot(data = DOD.2020, aes(x = DateTime , 
                            y = nitrateuM,
                            color = Site)) +
  geom_line() +
  ggtitle("2020") +
  xlab("") +
  ylab("Nitrate (uM)") +
  theme_classic() # plot model 

ff <- ggplot(data = DOD.2020, aes(x = DateTime , 
                            y = fDOM.QSU,
                            color = Site)) +
  geom_line() +
  xlab("") +
  ylab("fDOM (QSU)") +
  theme_classic() # plot model 

fg <- ggplot(data = DOD.2020, aes(x = DateTime, 
                            y = SpCond.uScm,
                            color = Site)) +
  geom_line() +
  xlab("") +
  ylab("SPC") +
  theme_classic() # plot model 

fh <- ggplot(data = DOD.2020, aes(x = DateTime, 
                            y = Turbidity.FNU,
                            color = Site)) +
  geom_line() +
  xlab("Date") +
  ylab("SPC") +
  theme_classic() # plot model 


plot_grid(fa, fe,
          fb, ff,
          fc, fg,
          fd, fh,
          ncol = 2)


#### FI ? #####
storms.2019.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/storms.2019.2020.csv")
cols <- c("fDOM.QSU","nitrateuM", "SpCond.uScm", "Turbidity.FNU", "MeanDischarge")
storms.2019.2020[cols] <- log(storms.2019.2020[cols]) # making concentrations and Q log transformed
#storms.2019.2020 <-  storms.2019.2020[!is.na(storms.2019.2020$fDOM.QSU), ]
#storms.2019.2020 <-  storms.2019.2020[!is.na(storms.2019.2020$nitrateuM), ]
#storms.2019.2020 <-  storms.2019.2020[!is.na(storms.2019.2020$SpCond.uScm), ]
#storms.2019.2020 <-  storms.2019.2020[!is.na(storms.2019.2020$Turbidity.FNU), ]

MOOS.2019 <- storms.2019.2020 %>% filter(Site == "MOOS" & year == "2019") # filtering out Site and year
MOOS.2019.fDOM <- MOOS.2019[,-c(6,7,8)] # making it so it is just fDOM

#STRT.2019 <- storms.2019.2020 %>% filter(Site == "STRT" & year == "2019") # filtering out Site and year
#STRT.2019.fDOM <- STRT.2019[,-c(6,7,8)] # making it so it is just fDOM

#fDOM_T <- storms.2019.2020 %>% group_by(Site, year, storm.num) %>% 
 # mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

# storm 1 #
Moos_test_ascending_storm1 <- filter(Moos_test_ascending, storm.num == "storm1")
Moos_test_ascending_storm1 <-  Moos_test_ascending_storm1[!is.na(Moos_test_ascending_storm1$fDOM.QSU), ]
slope(Moos_test_ascending_storm1$MeanDischarge, Moos_test_ascending_storm1$fDOM.QSU)
summary(lm(Moos_test_ascending_storm1$fDOM.QSU ~ Moos_test_ascending_storm1$MeanDischarge))

# try it by all of moos #
Moos_test <- MOOS.2019.fDOM %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

Moos_test_ascending <- filter(Moos_test, limb == "ascending")
Moos_test_ascending <-  Moos_test_ascending[!is.na(Moos_test_ascending$fDOM.QSU), ]
beta <- Moos_test_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(MeanDischarge, fDOM.QSU))

# merge HI and beta
#HI_moos_2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2019.csv")
#HI.moos.2019.fdom <- HI_moos_2019 %>% filter(response == "fDOM")

#moos.2019 <- left_join(fDOM_Test_6, HI.moos.2019.fdom)  # joining beta and HI

# Try it by all sites per response
storms.2019.2020.NO3 <- storms.2019.2020[,-c(5,7,8)] # only have NO3 as the response
storms.2019.2020.fDOM <- storms.2019.2020[,-c(6:8)] # only have fdom as the response
storms.2019.2020.SPC <- storms.2019.2020[,-c(5,6,8)] # only have fdom as the response
storms.2019.2020.turb <- storms.2019.2020[,-c(5:7)] # only have fdom as the response

# NO3
all_NO3_test <- storms.2019.2020.NO3 %>% group_by(storm.num, year) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_NO3_ascending <- filter(all_NO3_test, limb == "ascending")
all_NO3_ascending <-  all_NO3_ascending[!is.na(all_NO3_ascending$nitrateuM), ]
beta.all.no3 <- all_NO3_ascending %>% group_by(storm.num, Site, year) %>% 
  summarize(beta = slope(MeanDischarge, nitrateuM)) # this works just like the beta one that is for an individual site

# fDOM
all_fdom_test <- storms.2019.2020.fDOM %>% group_by(storm.num, year) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_fDOM_ascending <- filter(all_fdom_test, limb == "ascending")
all_fDOM_ascending <-  all_fDOM_ascending[!is.na(all_fDOM_ascending$fDOM.QSU), ]
beta.all.fdom <- all_fDOM_ascending %>% group_by(storm.num, Site, year) %>% 
  summarize(beta = slope(MeanDischarge, fDOM.QSU)) # this works just like the beta one that is for an individual site

# SPC
all_SPC_test <- storms.2019.2020.SPC %>% group_by(storm.num, year) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_SPC_ascending <- filter(all_SPC_test, limb == "ascending")
all_SPC_ascending <-  all_SPC_ascending[!is.na(all_SPC_ascending$SpCond.uScm), ]
beta.all.spc <- all_SPC_ascending %>% group_by(storm.num, Site, year) %>% 
  summarize(beta = slope(MeanDischarge, SpCond.uScm)) # this works just like the beta one that is for an individual site

# turb
all_turb_test <- storms.2019.2020.turb %>% group_by(storm.num, year) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_turb_ascending <- filter(all_turb_test, limb == "ascending")
all_turb_ascending <-  all_turb_ascending[!is.na(all_turb_ascending$Turbidity.FNU), ]
beta.all.turb <- all_turb_ascending %>% group_by(storm.num, Site, year) %>% 
  summarize(beta = slope(MeanDischarge, Turbidity.FNU)) # this works just like the beta one that is for an individual site

# merge HI and beta 
HI.all <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.all.csv")
names(HI.all)[names(HI.all) == 'site.ID'] <- 'Site'
# filter by response
HI.all.NO3 <- filter(HI.all, response == "NO3")
HI.all.fDOM <- filter(HI.all, response == "fDOM")
HI.all.SPC <- filter(HI.all, response == "SPC")
HI.all.turb <- filter(HI.all, response == "turb")

# joining HI and beta 
beta.HI.NO3 <- left_join(HI.all.NO3, beta.all.no3)  # joining beta and HI for NO3
beta.HI.fDOM <- left_join(HI.all.fDOM, beta.all.fdom)  # joining beta and HI for fDOM
beta.HI.SPC <- left_join(HI.all.SPC, beta.all.spc)  # joining beta and HI for fDOM
beta.HI.turb <- left_join(HI.all.turb, beta.all.turb)  # joining beta and HI for fDOM

# plot #
HI_FI_NO3.p = 
  ggplot(beta.HI.NO3, aes(beta, HI)) + geom_point(aes(colour=factor(pf)), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_NO3.p

HI_FI_fDOM.p = 
  ggplot(beta.HI.fDOM, aes(beta, HI)) + geom_point(aes(colour=factor(pf)), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("b) fDOM")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_fDOM.p

HI_FI_SPC.p = 
  ggplot(beta.HI.SPC, aes(beta, HI)) + geom_point(aes(colour=factor(pf)), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("c) SPC")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_SPC.p

HI_FI_turb.p = 
  ggplot(beta.HI.turb, aes(beta, HI)) + geom_point(aes(colour=factor(pf)), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("d) Turbidity")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_turb.p

plot_grid(HI_FI_NO3.p, HI_FI_fDOM.p,
          HI_FI_SPC.p, HI_FI_turb.p,
          ncol = 2)















































































