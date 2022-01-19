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
library(lubridate)
library(data.table)
library(rio)
library(ggplot2)
library(scales)
library(psych)
library(googledrive)
library(readxl)
library(cowplot)
library(zoo)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(ggpmisc)
library(SLOPE)
library(wesanderson)
library(ggpubr)

# Import data #
FRCH_HI_doy_df <- read_csv("Output_from_analysis/HI_plots/2019/FRCH/FRCH.HI.doy.df.csv")
MOOS_HI_doy_df <- read_csv("Output_from_analysis/HI_plots/2019/MOOS/MOOS.HI.doy.df.csv")
POKE_HI_doy_df <- read_csv("Output_from_analysis/HI_plots/2019/POKE/POKE.HI.doy.df.csv")
STRT_HI_doy_df <- read_csv("Output_from_analysis/HI_plots/2019/STRT/STRT.HI.doy.df.csv")
VAUL_HI_doy_df <- read_csv("Output_from_analysis/HI_plots/2019/VAUL/VAUL.HI.doy.df.csv")

FRCH_HI_doy_df.2020 <- read_csv("Output_from_analysis/HI_plots/2020/FRCH/FRCH.HI.doy.df.csv")
MOOS_HI_doy_df.2020 <- read_csv("Output_from_analysis/HI_plots/2020/MOOS/MOOS.HI.doy.df.csv")
POKE_HI_df <- read_csv("Output_from_analysis/HI_plots/2020/POKE/POKE.HI.df.csv")
POKE_HI_df$date <- as.Date(with(POKE_HI_df, paste(month, day, sep = "-")), "%m-%d")
POKE_HI_df$doy <- yday(POKE_HI_df$date) 
POKE_HI_doy_df.2020 <- POKE_HI_df
STRT_HI_doy_df.2020 <- read_csv("Output_from_analysis/HI_plots/2020/STRT/STRT.HI.doy.df.csv")
VAUL_HI_doy_df.2020 <- read_csv("Output_from_analysis/HI_plots/2020/VAUL/VAUL.HI.doy.df.csv")

HI.dat_2019 <- rbind(FRCH_HI_doy_df, MOOS_HI_doy_df, POKE_HI_doy_df, STRT_HI_doy_df, VAUL_HI_doy_df)
HI.dat_2019$year <- "2019"
HI.dat_2020 <- rbind(FRCH_HI_doy_df.2020, MOOS_HI_doy_df.2020, POKE_HI_doy_df.2020, STRT_HI_doy_df.2020, VAUL_HI_doy_df.2020)
HI.dat_2020$year <- "2020"

HI.dat <- rbind(HI.dat_2019, HI.dat_2020)
write.csv(HI.dat, "~/Documents/Storms/Output_from_analysis/HI.dat.csv")

HI.dat <- read_csv("~/Documents/Storms/Output_from_analysis/HI.dat.csv")


HI.mean<- HI.dat %>% group_by(site.ID, response, year) %>%  
  summarise_at(vars(HI), list(HI = median)) # take mean by site response and year 

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

VAUL.fDOM$pf <- "high"

FRCH.NO3$pf <- "medium"

POKE.NO3$pf <- "medium"

MOOS.NO3$pf <- "medium"

STRT.NO3$pf <- "high"

VAUL.NO3$pf <- "high"


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
  summarise_at(vars(HI), list(HI = median)) # take mean by site response and year 

HI.mean.precip.response <- HI.dat %>% group_by(site.ID, year, storm.num, response) %>%  
  summarise_at(vars(HI), list(HI = median)) # take mean by site response and year 

######################################## 2018 #####################################################################
## Step 1) Read in list of all sites storms and filter by site
## Step 2) Assign storm number to each individual storm
## Step 3) read in Rain gauge data and summarize storm characteristics (Total precip/Intensity) and 
# antecedent conditions (week/month/3month/doy/time since peak SWE)
## Step 4) Separate by constituent 

FRCHstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2018/All_Sites/", 
                              recursive=F, 
                              pattern="FRCH", 
                              full.names=TRUE) # reading in individual storms by site 

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                  read.csv, 
                                  check.names = FALSE,
                                  stringsAsFactors=FALSE, 
                                  header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 142),
                        rep("storm10", 704),
                        rep("storm11a", 91),
                        rep("storm11b", 264),
                        rep("storm2a", 230),
                        rep("storm2b", 190),
                        rep("storm3", 212),
                        rep("storm4a", 72),
                        rep("storm4b", 383),
                        rep("storm5", 331),
                        rep("storm6", 303),
                        rep("storm7", 119),
                        rep("storm8a", 79),
                        rep("storm8b", 95),
                        rep("storm9", 115)) # naming each storm by the number of storm 

#write_csv(FRCH_storms, "~/Desktop/FRCH_2018_test_beta.csv")


POKE_RainGauge_2018 <- read_csv("~/Documents/DoD_2018/RainGauge/POKE.RainGauge.2018.csv") # Reading in rain gauge data in 
attributes(POKE_RainGauge_2018$DateTime)$tzone <- 'America/Anchorage' # converting to AK time 
FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") #making datetime column 
FRCH.2018.storms.1<- left_join(FRCH_storms, POKE_RainGauge_2018, by = "DateTime") # joining 

names(FRCH.2018.storms.1)[names(FRCH.2018.storms.1) == ''] <- 'x'

FRCH.2018.per.storm.1 <- FRCH.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE) # summing total precipitation within each storm event 

FRCH.2018 <- read_csv("Q_Chem/FRCH/FRCH_chem_2018.csv", 
                           col_types = cols(fDOM.QSU = col_double(), 
                                            nitrateuM = col_double(), SpCond.uScm = col_double(), 
                                            Turbidity.FNU = col_double()))
attributes(FRCH.2018$DateTime)$tzone <- 'America/Anchorage'
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
HI.vaul.2019$pf <- "high" # adding a pf column

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

# add time since peak  Q in chena #
HI.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.csv")
HI.2019$date <- as.Date(HI.2019$doy, origin = "2019-01-01")
origin_date <- as.Date("2019-05-18")
HI.2019$TimeSinceChena <- julian(HI.2019$date, origin_date)
#write.csv(HI.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.csv")


storms.2019 <- rbind(FRCH_storms, MOOS_storms, VAUL_storms, POKE_storms, STRT_storms)
#write.csv(storms.2019, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/storms.2019.csv")
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
HI.poke.SPC.2020 <- left_join(HI.poke.SPC.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.poke.SPC.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.poke.SPC.2020, POKE.2020.per.storm.4, by = "storm.num")

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
HI.poke.turb.2020 <- left_join(HI.poke.turb.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.poke.turb.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.poke.turb.2020, POKE.2020.per.storm.4, by = "storm.num")

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

HI.poke.2020 <- rbind(HI.poke.no3.2.2020, HI.poke.fDOM.2.2020, HI.poke.SPC.2.2020, HI.poke.turb.2.2020) # merging all responses together 
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
HI.vaul.2020$pf <- "high" # adding a pf column

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

HI.strt.2020 <- rbind(HI.strt.no3.2.2020, HI.strt.fDOM.2.2020, HI.strt.SPC.2.2020, HI.strt.turb.2.2020) # merging all responses together 
HI.strt.2020$burn <- "burned" # adding a burn column
HI.strt.2020$pf <- "high" # adding a pf column

write.csv(HI.strt.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.strt.2020.csv")

HI.2020 <- rbind(HI.moos.2020, HI.frch.2020, HI.vaul.2020, HI.strt.2020, HI.poke.2020)
write.csv(HI.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.2020.csv")

# time since peak chena flow (snow melt)  
  # May 13th 2020 was peak Q for the chena 
HI.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.2020.csv")
HI.2020$date <- as.Date(HI.2020$doy, origin = "2020-01-01")
origin_date <- as.Date("2020-05-13")
HI.2020$TimeSinceChena <- julian(HI.2020$date, origin_date)

write.csv(HI.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.2020.csv")

HI.2020 %>% filter(response == "NO3") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI)) +
  geom_point(aes(color = site.ID)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT no3") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.2020 %>% filter(response == "NO3") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI,
             color = site.ID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT no3") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.2020 %>% filter(response == "fDOM") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI)) +
  geom_point(aes(color = site.ID)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.2020 %>% filter(response == "fDOM") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI,
             color = site.ID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model

HI.2020 %>% filter(response == "SPC") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI)) +
  geom_point(aes(color = site.ID)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.2020 %>% filter(response == "SPC") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI,
             color = site.ID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.2020 %>% filter(response == "turb") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI)) +
  geom_point(aes(color = site.ID)) +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.2020 %>% filter(response == "turb") %>% 
  ggplot(aes(x=TimeSinceChena, 
             y=HI,
             color = site.ID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 


################# combining data sets #######################
# combining years
HI.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.csv")
HI.2019 <- HI.2019[,-c(1:2)]
HI.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.2020.csv")
HI.2020 <- HI.2020[,-1]

HI.2019.2020 <- rbind(HI.2019, HI.2020)
write.csv(HI.2019.2020, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.2020.csv")
HI.2019.2020$year <- as.character(HI.2019.2020$year)
HI.2019.2020 %>%
  ggplot(aes(x=TimeSinceChena, 
             y=HI,
             shape = site.ID)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE) +
  facet_wrap(~response) +
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Time since snowmelt") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 
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
names(HI.moos.2019)[names(HI.moos.2019) == 'HI'] <- 'HI.median'
HI.moos.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.moos.2020.csv")# just mean values of HI 
names(HI.moos.2020)[names(HI.moos.2020) == 'HI'] <- 'HI.median'

MOOS.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/MOOS/MOOS.HI.df.csv") # has 2% intervals to generate error bars
MOOS.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/MOOS/MOOS.HI.df.csv") # has 2% intervals to generate error bars

moos.2019 <- left_join(HI.moos.2019, MOOS.HI.df.2019, by = c("storm.num", "response", "site.ID")) # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

moos.2020 <- left_join(HI.moos.2020, MOOS.HI.df.2020, by = c("storm.num", "response", "site.ID"))# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

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
             y = HI)) +
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
  facet_wrap(~response)

moos.storm.precip <- a + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2)

b <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("Medium PF") +
  xlab("One-Week Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

moos.week.precip <- b + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2)

c <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Month Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

moos.month.precip <- c + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2)

d <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI)) +
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
  facet_wrap(~response)

moos.3month.precip <- d + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2)

e <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI)) +
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
  facet_wrap(~response)

moos.intensity.precip <- e + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2)

f <- HI.moos.2019.2020 %>% 
  ggplot(aes(x = doy,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = moos.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("doy") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

moos.doy.precip <- f + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2)
plot_grid(moos.storm.precip, moos.week.precip, moos.month.precip,
          moos.3month.precip, moos.intensity.precip, moos.doy.precip,
          ncol = 3)


####################### FRCH #################################
HI.frch.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.frch.2019.csv") # just mean values of HI 
names(HI.frch.2019)[names(HI.frch.2019) == 'HI'] <- 'HI.median'
HI.frch.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.frch.2020.csv")# just mean values of HI 
names(HI.frch.2020)[names(HI.frch.2020) == 'HI'] <- 'HI.median'

FRCH.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/FRCH/FRCH.HI.doy.df.csv") # has 2% intervals to generate error bars
FRCH.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/FRCH/FRCH.HI.doy.df.csv") # has 2% intervals to generate error bars

frch.2019 <- left_join(HI.frch.2019, FRCH.HI.df.2019, by = c("storm.num", "response", "site.ID")) # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

frch.2020 <- left_join(HI.frch.2020, FRCH.HI.df.2020, by = c("storm.num", "response", "site.ID"))# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

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
             y = HI)) +
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
  facet_wrap(~response)

frch.storm.precip <- a + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

b <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
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
  facet_wrap(~response)

frch.week.precip <- b + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI)) +
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
  facet_wrap(~response)

frch.month.precip <- c + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI)) +
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
  facet_wrap(~response)

frch.3month.precip <- d + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI)) +
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
  facet_wrap(~response)

frch.intensity.precip <- e + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.frch.2019.2020 %>% 
  ggplot(aes(x = doy.x,
             y = HI)) +
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
  facet_wrap(~response)

frch.doy.precip <- f + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
plot_grid(frch.storm.precip, frch.week.precip, frch.month.precip,
          frch.3month.precip, frch.intensity.precip, frch.doy.precip,
          ncol = 3)

####################### VAUL #################################
HI.vaul.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.vaul.2019.csv") # just mean values of HI 
names(HI.vaul.2019)[names(HI.vaul.2019) == 'HI'] <- 'HI.median'
HI.vaul.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.vaul.2020.csv")# just mean values of HI 
names(HI.vaul.2020)[names(HI.vaul.2020) == 'HI'] <- 'HI.median'

VAUL.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/VAUL/VAUL.HI.doy.df.csv") # has 2% intervals to generate error bars
VAUL.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/VAUL/VAUL.HI.doy.df.csv") # has 2% intervals to generate error bars

vaul.2019 <- left_join(HI.vaul.2019, VAUL.HI.df.2019, by = c("storm.num", "response", "site.ID")) # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

vaul.2020 <- left_join(HI.vaul.2020, VAUL.HI.df.2020, by = c("storm.num", "response", "site.ID"))# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

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
             y = HI)) +
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
  facet_wrap(~response)

vaul.storm.precip <- a + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

b <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("High PF") +
  xlab("One-Week Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

vaul.week.precip <- b + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Month Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

vaul.month.precip <- c + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI)) +
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
  facet_wrap(~response)

vaul.3month.precip <- d + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI)) +
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
  facet_wrap(~response)

vaul.intensity.precip <- e + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.vaul.2019.2020 %>% 
  ggplot(aes(x = doy.x,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = vaul.formula,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("doy") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

vaul.doy.precip <- f + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
plot_grid(vaul.storm.precip, vaul.week.precip, vaul.month.precip,
          vaul.3month.precip, vaul.intensity.precip, vaul.doy.precip,
          ncol = 3)

####################### STRT #################################
HI.strt.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.strt.2019.csv") # just mean values of HI 
names(HI.strt.2019)[names(HI.strt.2019) == 'HI'] <- 'HI.median'
HI.strt.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.strt.2020.csv")# just mean values of HI 
names(HI.strt.2020)[names(HI.strt.2020) == 'HI'] <- 'HI.median'

STRT.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/STRT/STRT.HI.df.csv") # has 2% intervals to generate error bars
STRT.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/STRT/STRT.HI.df.csv") # has 2% intervals to generate error bars

strt.2019 <- left_join(HI.strt.2019, STRT.HI.df.2019, by = c("storm.num", "response", "site.ID")) # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

strt.2020 <- left_join(HI.strt.2020, STRT.HI.df.2020, by = c("storm.num", "response", "site.ID"))# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars

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
             y = HI)) +
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
  facet_wrap(~response)

strt.storm.precip <- a + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

b <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
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
  facet_wrap(~response)

strt.week.precip <- b + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI)) +
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
  facet_wrap(~response)

strt.month.precip <- c + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI)) +
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
  facet_wrap(~response)

strt.3month.precip <- d + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI)) +
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
  facet_wrap(~response)

strt.intensity.precip <- e + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.strt.2019.2020 %>% 
  ggplot(aes(x = doy,
             y = HI)) +
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
  facet_wrap(~response)

strt.doy.precip <- f + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
plot_grid(strt.storm.precip, strt.week.precip, strt.month.precip,
          strt.3month.precip, strt.intensity.precip, strt.doy.precip,
          ncol = 3)

################ POKE ###############################
HI.poke.2019 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.poke.2019.csv") # just mean values of HI 
names(HI.poke.2019)[names(HI.poke.2019) == 'HI'] <- 'HI.median'
HI.poke.2020 <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.poke.2020.csv")# just mean values of HI 
names(HI.poke.2020)[names(HI.poke.2020) == 'HI'] <- 'HI.median'

POKE.HI.df.2019 <- read.csv("Output_from_analysis/HI_plots/2019/POKE/POKE.HI.df.csv") # has 2% intervals to generate error bars
POKE.HI.df.2020 <- read.csv("Output_from_analysis/HI_plots/2020/POKE/POKE.HI.df.csv") # has 2% intervals to generate error bars

poke.2019 <- left_join(HI.poke.2019, POKE.HI.df.2019, by = c("storm.num", "response", "site.ID")) # joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars
poke.2019 <- poke.2019[,-24]
poke.2020 <- left_join(HI.poke.2020, POKE.HI.df.2020, by = c("storm.num", "response", "site.ID"))# joining 2% inervals with the storm characteristics (Intensity) and antecedent conditions (one week, one month etc.) to generate error bars
names(poke.2020)[names(poke.2020) == 'doy'] <- 'doy.x'
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


a <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = precip,
             y = HI)) +
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
  facet_wrap(~response)

poke.storm.precip <- a + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 
  

b <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("Low PF") +
  xlab("One-Week Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

poke.week.precip <- b + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

c <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = precip.month,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE One-Month Precip") +
  xlab("") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

poke.month.precip <- c + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

d <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = ThreeMonth,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE Three-Month Precip") +
  xlab("Three-Month Precip") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

poke.3month.precip <- d + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

e <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = Intensity,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("POKE Storm Intensity") +
  xlab("Intensity") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

poke.intensity.precip <- e + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) 

f <- HI.poke.2019.2020 %>% 
  ggplot(aes(x = doy.x,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("doy") +
  ylab("HI-Solute Storage") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() +
  facet_wrap(~response)

poke.doy.precip <- f + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2)




# AGU plot #
HI.moos.2019.2020.fdom <- HI.moos.2019.2020 %>% filter(response == "fDOM")
HI.frch.2019.2020.fdom <- HI.frch.2019.2020 %>% filter(response == "fDOM")
HI.poke.2019.2020.fdom <- HI.poke.2019.2020 %>% filter(response == "fDOM")
HI.strt.2019.2020.fdom <- HI.strt.2019.2020 %>% filter(response == "fDOM")
HI.vaul.2019.2020.fdom <- HI.vaul.2019.2020 %>% filter(response == "fDOM")

HI.moos.2019.2020.fdom <- HI.moos.2019.2020 %>% filter(response == "NO3")
HI.frch.2019.2020.fdom <- HI.frch.2019.2020 %>% filter(response == "NO3")
HI.poke.2019.2020.fdom <- HI.poke.2019.2020 %>% filter(response == "NO3")
HI.strt.2019.2020.fdom <- HI.strt.2019.2020 %>% filter(response == "NO3")
HI.vaul.2019.2020.fdom <- HI.vaul.2019.2020 %>% filter(response == "NO3")

a <- HI.frch.2019.2020.fdom %>% 
  ggplot(aes(x = precip.month,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Month Precip") +
  ylab("HI") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

frch.precip.month.fdom <- a + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

a <- HI.frch.2019.2020.no3 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Week Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

frch.precip.week.no3 <- a + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

b <- HI.moos.2019.2020.fDOM %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Week Precip") +
  ylab("HI") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

moos.precip.week.fDOM <- b + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

b <- HI.moos.2019.2020.no3 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Week Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

moos.precip.week.no3 <- b + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

c <- HI.poke.2019.2020.fdom %>% 
  ggplot(aes(x = doy.x,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("Day of Year") +
  ylab("HI") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

poke.doy.fdom <- c + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

d <- HI.poke.2019.2020.no3 %>% 
  ggplot(aes(x = precip.month,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Month Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

poke.precip.month <- d + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

e <- HI.strt.2019.2020.fdom %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Week Precip") +
  ylab("HI") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

strt.precip.week.fdom <- e + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

e <- HI.strt.2019.2020.no3 %>% 
  ggplot(aes(x = precip.week,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("One-Week Precip") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

strt.precip.week.no3 <- e + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

f <- HI.vaul.2019.2020.fdom %>% 
  ggplot(aes(x = doy.x,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("Day of Year") +
  ylab("HI") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

vaul.doy.fdom <- f + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 

f <- HI.vaul.2019.2020.no3 %>% 
  ggplot(aes(x = doy.x,
             y = HI)) +
  geom_point(alpha = 0.00001) +
  geom_smooth(method = "lm", formula= y~x, na.rm = TRUE, fullrange = TRUE, aes(group = 1)) +
  ylim(-1,1) + 
  ggtitle("") +
  xlab("Day of Year") +
  ylab("") + 
  geom_hline(yintercept = 0, linetype = "dotted", col = 'red') +
  theme_classic() 

vaul.doy.no3 <- f + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               aes(color = year), width  = 0.2, size = 1) + 
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) +
  stat_summary(fun.y = median, geom = "point", 
               aes(color = year), size = 2) +
  scale_colour_manual(values = wes_palette(n=2, name="Royal1")) 




ggarrange(frch.precip.month.fdom, frch.precip.week.no3, 
          moos.precip.week.fDOM, moos.precip.week.no3,
          poke.doy.fdom, poke.precip.month,
          strt.precip.week.fdom, strt.precip.week.no3,
          vaul.doy.fdom, vaul.doy.no3,
          ncol = 2, nrow = 5,
          common.legend = TRUE, legend = "bottom")

# removing columns that are unneeded to match all together 
#HI.frch.2019.2020 <- HI.frch.2019.2020[,-c(25:26)]
#HI.poke.2019.2020 <- HI.frch.2019.2020[,-25]
#HI.vaul.2019.2020 <- HI.vaul.2019.2020[,-c(25:26)]

#names(HI.moos.2019.2020)[names(HI.moos.2019.2020) == 'doy'] <- 'doy.x'
 
#HI.2019.2020 <- rbind(HI.vaul.2019.2020, HI.frch.2019.2020, HI.moos.2019.2020, HI.strt.2019.2020, HI.poke.2019.2020)

#write.csv(HI.2019.2020, "~/Documents/Storms/Output_from_analysis/07_Models/HI.2019.2020.csv")

################# 2019 and 2020  chem ################
# when i import from my local machine it gets rid of nitrate for some reason

VAUL_chem_2019 <- read_csv("Q_Chem/VAUL/VAUL_chem_2019.csv")
VAUL <- VAUL_chem_2019
VAUL <- VAUL[,-1]
chem_2019 <- rbind(FRCH, MOOS, POKE, STRT,VAUL)

names(chem_2019) <- c("Site", "DateTime", "MeanDischarge", "day", "fDOM", "NO3", "SPC", "Turb")
chem_2019 <- chem_2019[,-4]
chem_2019$year <- "2019"
chem_2019[chem_2019$Site==1:15687, "Site"] <- "French"

write.csv(chem_2019, "~/Documents/Storms/Q_Chem/DOD.2019.mean.csv")
chem_2019 <- read_csv("Q_Chem/DOD.2019.mean.csv")

FRCH_chem_2020 <- read_csv("Q_Chem/FRCH/FRCH_chem_2020.csv")
MOOS_chem_2020 <- read_csv("Q_Chem/MOOS/MOOS_chem_2020.csv")
MOOS_chem_2020 <- MOOS_chem_2020[,-c(2,6)]
POKE_chem_2020 <- read_csv("Q_Chem/POKE/POKE_chem_2020.csv")
names(POKE_chem_2020)[names(POKE_chem_2020) == 'fDOM.RFU'] <- 'fDOM.QSU'

STRT_chem_2020 <- read_csv("Q_Chem/STRT/STRT_chem_2020.csv")
VAUL_chem_2020 <- read_csv("Q_Chem/VAUL/VAUL_chem_2020.csv")

which(VAUL_chem_2020$fDOM.QSU > 1000)
which(VAUL_chem_2020$Turbidity.FNU < 0)


VAUL_chem_2020 <- VAUL_chem_2020[-c(5307,5308), ]

chem_2020 <- rbind(FRCH_chem_2020, MOOS_chem_2020, POKE_chem_2020, STRT_chem_2020, VAUL_chem_2020)
chem_2020 <- chem_2020[,-1]
chem_2020$year <- "2020"
names(chem_2020) <- c("Site", "DateTime", "MeanDischarge", "fDOM", "NO3", "SPC", "Turb", "year")

chem_2019_2020 <- rbind(chem_2019, chem_2020)

Q_2019 <- read_csv("~/Documents/DoD_Discharge/Predicted_Discharge/2019/Q_2019.csv")
Q_2020 <- read_csv("~/Documents/DoD_Discharge/Predicted_Discharge/2020/Q_2020.csv")
Q_2019$year <- "2019"
Q_2020$year <- "2020"

Q_2019_2020 <- rbind(Q_2019, Q_2020)

no3 <- ggplot(data = chem_2019_2020, aes(x = Site, y = NO3, fill = as.factor(year))) +
  geom_violin() + 
  xlab("") +
  ylab("Nitrate (µM)") +
  theme_classic() +
  scale_fill_manual(values=wes_palette(n=2, name="Royal1")) +
  labs(fill = "Year") + 
  theme(text = element_text(size = 15)) 

fDOM <- ggplot(data = chem_2019_2020, aes(x = Site, y = fDOM, fill = as.factor(year))) +
  geom_violin() + 
  xlab("") +
  ylab("fDOM (QSU)") +
  theme_classic() +
  scale_fill_manual(values=wes_palette(n=2, name="Royal1")) +
  labs(fill = "Year") + 
  theme(text = element_text(size = 15)) 
fDOM

SPC <- ggplot(data = chem_2019_2020, aes(x = Site, y = SPC, fill = as.factor(year))) +
  geom_violin() + 
  xlab("") +
  ylab("SPC (µScm)") +
  theme_classic() +
  scale_fill_manual(values=wes_palette(n=2, name="Royal1")) +
  labs(fill = "Year") + 
  theme(text = element_text(size = 15)) 

SPC
options(scipen = 999)
turb <- ggplot(data = chem_2019_2020, aes(x = Site, y = Turb, fill = as.factor(year))) +
  geom_violin() + 
  xlab("") +
  ylab("Turbidity (FNU)") +
  theme_classic() +
  scale_fill_manual(values=wes_palette(n=2, name="Royal1")) +
  labs(fill = "Year") + 
  theme(text = element_text(size = 15)) +
  scale_y_continuous(trans='log10', labels = scales::label_number(accuracy = 1))

turb

Q <- ggplot(data = Q_2019_2020, aes(x = Site, y = MeanDischarge, fill = as.factor(year))) +
  geom_violin() + 
  xlab("") +
  ylab("Discharge (L/s)") +
  theme_classic() +
  scale_fill_manual(values=wes_palette(n=2, name="Royal1")) +
  labs(fill = "Year") + 
  theme(text = element_text(size = 15)) +
  scale_y_continuous(trans='log10')
Q

ggarrange(Q, no3,
          fDOM, SPC, 
          turb, NA,
          ncol = 2, nrow = 3,
          common.legend = TRUE, legend = "bottom")


##############################################################################################################
##############################################################################################################
##############################################################################################################

#########################################################################################################
########################################### FI ##########################################################
#########################################################################################################

########################################### 2018 ##########################################################

# load storm data to R #

### Q dat ###
FRCH_storm1_06_21_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm1_06_21_Q.csv", header = T, row.names = 1)
FRCH_storm2a_06_29_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm2a_06_29_Q.csv", header = T, row.names = 1)
FRCH_storm2b_07_04_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm2b_07_04_Q.csv", header = T, row.names = 1)
FRCH_storm3_07_10_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm3_07_10_Q.csv", header = T, row.names = 1)
FRCH_storm4a_07_15_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm4a_07_15_Q.csv", header = T, row.names = 1)
FRCH_storm4b_07_16_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm4b_07_16_Q.csv", header = T, row.names = 1)
FRCH_storm5_08_04_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm5_08_04_Q.csv", header = T, row.names = 1)
FRCH_storm6_08_13_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm6_08_13_Q.csv", header = T, row.names = 1)
FRCH_storm7_08_23_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm7_08_23_Q.csv", header = T, row.names = 1)
FRCH_storm8a_08_26_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm8a_08_26_Q.csv", header = T, row.names = 1)
FRCH_storm8b_08_27_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm8b_08_27_Q.csv", header = T, row.names = 1)
FRCH_storm9_08_29_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm9_08_29_Q.csv", header = T, row.names = 1)
FRCH_storm10_09_01_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm10_09_01_Q.csv", header = T, row.names = 1)
FRCH_storm11a_09_22_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm11a_09_22_Q.csv", header = T, row.names = 1)
FRCH_storm11b_09_24_Q <- read.csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm11b_09_24_Q.csv", header = T, row.names = 1)

### solute data with bursts ###
FRCH_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/FRCH_NO3_storm_list.RData")
FRCH_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/FRCH_fDOM_storm_list.RData")
FRCH_SpCond_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/FRCH_SpCond_storm_list.RData")

# normalize Q data #

FRCH_storm1_06_21_Q$datavalue.norm = 
  (FRCH_storm1_06_21_Q$datavalue - min(FRCH_storm1_06_21_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm1_06_21_Q$datavalue, na.rm=T) - min(FRCH_storm1_06_21_Q$datavalue, na.rm=T))
FRCH_storm2a_06_29_Q$datavalue.norm = 
  (FRCH_storm2a_06_29_Q$datavalue - min(FRCH_storm2a_06_29_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm2a_06_29_Q$datavalue, na.rm=T) - min(FRCH_storm2a_06_29_Q$datavalue, na.rm=T))
FRCH_storm2b_07_04_Q$datavalue.norm = 
  (FRCH_storm2b_07_04_Q$datavalue - min(FRCH_storm2b_07_04_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm2b_07_04_Q$datavalue, na.rm=T) - min(FRCH_storm2b_07_04_Q$datavalue, na.rm=T))
FRCH_storm3_07_10_Q$datavalue.norm = 
  (FRCH_storm3_07_10_Q$datavalue - min(FRCH_storm3_07_10_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm3_07_10_Q$datavalue, na.rm=T) - min(FRCH_storm3_07_10_Q$datavalue, na.rm=T))
FRCH_storm4a_07_15_Q$datavalue.norm = 
  (FRCH_storm4a_07_15_Q$datavalue - min(FRCH_storm4a_07_15_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm4a_07_15_Q$datavalue, na.rm=T) - min(FRCH_storm4a_07_15_Q$datavalue, na.rm=T))
FRCH_storm4b_07_16_Q$datavalue.norm = 
  (FRCH_storm4b_07_16_Q$datavalue - min(FRCH_storm4b_07_16_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm4b_07_16_Q$datavalue, na.rm=T) - min(FRCH_storm4b_07_16_Q$datavalue, na.rm=T))
FRCH_storm5_08_04_Q$datavalue.norm = 
  (FRCH_storm5_08_04_Q$datavalue - min(FRCH_storm5_08_04_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm5_08_04_Q$datavalue, na.rm=T) - min(FRCH_storm5_08_04_Q$datavalue, na.rm=T))
FRCH_storm6_08_13_Q$datavalue.norm = 
  (FRCH_storm6_08_13_Q$datavalue - min(FRCH_storm6_08_13_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm6_08_13_Q$datavalue, na.rm=T) - min(FRCH_storm6_08_13_Q$datavalue, na.rm=T))
FRCH_storm7_08_23_Q$datavalue.norm = 
  (FRCH_storm7_08_23_Q$datavalue - min(FRCH_storm7_08_23_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm7_08_23_Q$datavalue, na.rm=T) - min(FRCH_storm7_08_23_Q$datavalue, na.rm=T))
FRCH_storm8a_08_26_Q$datavalue.norm = 
  (FRCH_storm8a_08_26_Q$datavalue - min(FRCH_storm8a_08_26_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm8a_08_26_Q$datavalue, na.rm=T) - min(FRCH_storm8a_08_26_Q$datavalue, na.rm=T))
FRCH_storm8b_08_27_Q$datavalue.norm = 
  (FRCH_storm8b_08_27_Q$datavalue - min(FRCH_storm8b_08_27_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm8b_08_27_Q$datavalue, na.rm=T) - min(FRCH_storm8b_08_27_Q$datavalue, na.rm=T))
FRCH_storm9_08_29_Q$datavalue.norm = 
  (FRCH_storm9_08_29_Q$datavalue - min(FRCH_storm9_08_29_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm9_08_29_Q$datavalue, na.rm=T) - min(FRCH_storm9_08_29_Q$datavalue, na.rm=T))
FRCH_storm10_09_01_Q$datavalue.norm = 
  (FRCH_storm10_09_01_Q$datavalue - min(FRCH_storm10_09_01_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm10_09_01_Q$datavalue, na.rm=T) - min(FRCH_storm10_09_01_Q$datavalue, na.rm=T))
FRCH_storm11a_09_22_Q$datavalue.norm = 
  (FRCH_storm11a_09_22_Q$datavalue - min(FRCH_storm11a_09_22_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm11a_09_22_Q$datavalue, na.rm=T) - min(FRCH_storm11a_09_22_Q$datavalue, na.rm=T))
FRCH_storm11b_09_24_Q$datavalue.norm = 
  (FRCH_storm11b_09_24_Q$datavalue - min(FRCH_storm11b_09_24_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm11b_09_24_Q$datavalue, na.rm=T) - min(FRCH_storm11b_09_24_Q$datavalue, na.rm=T))


# normalize solute data #

### remove burst-complied data ###

for(i in 1:length(FRCH_NO3_storm_list)){
  FRCH_NO3_storm_list[[i]][["datavalue"]] = FRCH_NO3_storm_list[[i]][["nitrateuM"]]
  FRCH_NO3_storm_list[[i]][["nitrateuM"]] = NULL
}

for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]][["datavalue"]] = FRCH_fDOM_storm_list[[i]][["fDOM.QSU.x"]]
  FRCH_fDOM_storm_list[[i]][["fDOM.QSU.x"]] = NULL
}

for(i in 1:length(FRCH_SpCond_storm_list)){
  FRCH_SpCond_storm_list[[i]][["datavalue"]] = FRCH_SpCond_storm_list[[i]][["SpCond.µS.cm"]]
  FRCH_SpCond_storm_list[[i]][["SpCond.µS.cm"]] = NULL
}


### normalize burst data ###

for(i in 1:length(FRCH_NO3_storm_list)){
  FRCH_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list[[i]][["datavalue"]]-min(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}
for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_fDOM_storm_list[[i]][["datavalue"]]-min(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(FRCH_SpCond_storm_list)){
  FRCH_SpCond_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_SpCond_storm_list[[i]][["datavalue"]]-min(FRCH_SpCond_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_SpCond_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_SpCond_storm_list[[i]][["datavalue"]], na.rm=T))
}




# replace missing values in storms missing the first point with the second point
head(C4_fDOM_storm_list[["C4_storm1_fDOM"]], 20)
C4_fDOM_storm_list[["C4_storm1_fDOM"]][1:6,2] = C4_fDOM_storm_list[["C4_storm1_fDOM"]][7:12,2] 
C4_fDOM_storm_list[["C4_storm1_fDOM"]][1:6,3] = C4_fDOM_storm_list[["C4_storm1_fDOM"]][7:12,3] 

# #compile bursts #
# 
# C2_NO3_storm_list.st = list()
# 
# for(i in 1:length(C2_NO3_storm_list)){
# min<-cut(C2_NO3_storm_list[[i]][["valuedatetime"]], breaks="1 min")
# C2_NO3_storm_list.st[[i]] <- as.data.frame(as.list(aggregate(cbind(datavalue, nitrateuM,
#                                                                    datavalue.norm, burst.norm)
#                                                ~ min, data=C2_NO3_storm_list[[i]],
#                                                na.action=na.pass,
#                                                FUN=function(x) c(mn=mean(x), SD=sd(x)))))
# C2_NO3_storm_list.st[[i]][["valuedatetime"]]<-as.POSIXct(C2_NO3_storm_list.st[[i]][["min"]], "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
# }

#fxn: calculate FI by difference and bootstrap CIs #

FI_diff = function(dat_Q, dat_response) {
  FI_dat = rbind(dat_response[as.POSIXct(dat_response$valuedatetime) == min(as.POSIXct(dat_response$valuedatetime)),], 
                 dat_response[as.POSIXct(dat_response$valuedatetime) == as.POSIXct(dat_Q$valuedatetime[dat_Q$datavalue.norm == max(dat_Q$datavalue.norm)]),])
  
  FI_dat$valuedatetime = as.character(as.POSIXct(FI_dat$valuedatetime))
  
  dat_Q$valuedatetime = as.character(as.POSIXct(dat_Q$valuedatetime))
  
  FI_dat = left_join(FI_dat, 
                     subset(dat_Q, select=c("valuedatetime", "datavalue.norm")),
                     by="valuedatetime")
  
  names(FI_dat) = c("valuedatetime", "datavalue", "datavalue.norm", "Q")
  
  FI_dat$datavalue.norm = as.numeric(FI_dat$datavalue.norm)
  FI_dat$Q = as.numeric(FI_dat$Q)
  
  FI = mean(FI_dat$datavalue.norm[FI_dat$valuedatetime == max(FI_dat$valuedatetime)]) - mean(FI_dat$datavalue.norm[FI_dat$valuedatetime == min(FI_dat$valuedatetime)])
  
  meanDiff = function(data, indices) { 
    d <- data[indices,] # allows boot to select sample
    m1 = mean(d$datavalue.norm[d$valuedatetime == max(d$valuedatetime)])
    m2 = mean(d$datavalue.norm[d$valuedatetime == min(d$valuedatetime)])
    m = m1 - m2
    return(m)
  }
  
  FI_boot = boot(FI_dat, meanDiff, R = 10000, strata = as.factor(FI_dat[,1]))
  FI_bootCI = boot.ci(FI_boot, type="bca")
  
  FI_bootCI = data.frame(cbind(FI_boot$t0, FI_bootCI[["bca"]][4], FI_bootCI[["bca"]][5]))
  names(FI_bootCI) = c("FI", "lower", "upper")
  
  FI_results = list(FI_dat, FI_bootCI)
  
  return(FI_results)
}

# calculate FI by difference and bootstrap CIs #

FRCH_storm1_06_21_NO3_FI = FI_diff(FRCH_storm1_06_21_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_NO3`)
#FRCH_storm2a_06_29_NO3_FI = FI_diff(FRCH_storm2a_06_29_Q,FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2a_06_29_Q`)
#FRCH_storm2b_07_04_NO3_FI = FI_diff(FRCH_storm2b_07_04_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2b_07_04_NO3`)
FRCH_storm3_07_10_NO3_FI = FI_diff(FRCH_storm3_07_10_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm3_07_10_NO3`)

# gather results and save #


FI_results = rbind(
  c("FRCH_storm1_06_21_NO3_FI",FRCH_storm1_06_21_NO3_FI[[2]]),
  c("FRCH_storm3_07_10_NO3_FI",FRCH_storm3_07_10_NO3_FI[[2]]))

FI_results = as.data.frame(FI_results)

names(FI_results) = c("ID", "Flushing_index", "percCI_2.5", "percCI_97.5")

FI_results$ID = unlist(FI_results$ID)
FI_results$Flushing_index = round(as.numeric(as.character(FI_results$Flushing_index)), 4)
FI_results$`percCI_2.5` = round(as.numeric(as.character(FI_results$`percCI_2.5`)), 4)
FI_results$`percCI_97.5` = round(as.numeric(as.character(FI_results$`percCI_97.5`)), 4)

write.csv(FI_results, "/Users/alexwebster/Dropbox/Harms Lab/CPCRW 2017/new tidy files/CPCRW_2017/Output from analyses/24_25_CPCRW_2017/all.FI.diff.results.csv")

# calculate 95% bootstrap around median of Hyst. Indicies for each site and storm #

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

FRCH.HI.df <- read.csv("Output_from_analysis/HI_plots/2018/FRCH/FRCH.HI.median.boot.csv")

storm.list = unique(FRCH.HI.df$storm.ID)
FRCH.HI.boot <- do.call(rbind.data.frame,
                      lapply(storm.list, function(i){
                        dat = subset(FRCH.HI.df, storm.ID == i)
                        median_cl_boot(dat$HI)
                      }))
FRCH.HI.boot$storm.ID = storm.list

FRCH.HI.boot$site.ID = "FRCH"

HI <- FRCH.HI.boot


FI = subset(FI_results, select=c("Flushing_index", "percCI_2.5", "percCI_97.5", "ID"))
FI$ID = as.character(FI$ID)
library(tidyverse)
FI = separate(FI, ID, into=c("site.ID", "storm.ID", "month", "day", "response_var", NA), sep = "_")
names(FI) = c("Flush_index", "FI_ymin", "FI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI$site.ID=NULL
HI = separate(HI, storm.ID, into=c("site.ID", "storm.ID", "month", "day", "response_var"), sep = "_")
names(HI) = c("Hyst_index", "HI_ymin", "HI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI_FI = left_join(HI, FI, by=c("site.ID", "storm.ID", "response_var"))

HI_FI_NO3 = subset(HI_FI, response_var == "NO3")
HI_FI_NO3$site.ID <- factor(HI_FI_NO3$site.ID, levels = c('FRCH'))

HI_FI_NO3.p = 
  ggplot(HI_FI_NO3, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("#0571B0", "#CA0020"), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_NO3.p




































































########################################### 2019 ########################################################
# load storm data to R #

### Q dat ###
# FRCH #
FRCH_storm1_05_31_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm1_05_31_Q.csv", header=T, row.names = 1) 
FRCH_storm2_06_15_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm2_06_15_Q.csv", header=T, row.names = 1) 
FRCH_storm3_06_18_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm3_06_18_Q.csv", header=T, row.names = 1) 
FRCH_storm4_06_20_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm4_06_20_Q.csv", header=T, row.names = 1) 
FRCH_storm5_06_22_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm5_06_22_Q.csv", header=T, row.names = 1) 
FRCH_storm6_07_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm6_07_12_Q.csv", header=T, row.names = 1) 
FRCH_storm7_07_25_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm7_07_25_Q.csv", header=T, row.names = 1) 
FRCH_storm8_07_28_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm8_07_28_Q.csv", header=T, row.names = 1) 
FRCH_storm9a_07_29_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm9a_07_29_Q.csv", header=T, row.names = 1) 
FRCH_storm9b_07_30_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm9b_07_30_Q.csv", header=T, row.names = 1) 
FRCH_storm10a_08_01_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm10a_08_01_Q.csv", header=T, row.names = 1) 
FRCH_storm10b_08_02_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm10b_08_02_Q.csv", header=T, row.names = 1) 
FRCH_storm10c_08_03_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm10c_08_03_Q.csv", header=T, row.names = 1) 
FRCH_storm11_08_05_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm11_08_05_Q.csv", header=T, row.names = 1) 
FRCH_storm12a_08_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm12a_08_12_Q.csv", header=T, row.names = 1) 
FRCH_storm12b_08_14_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm12b_08_14_Q.csv", header=T, row.names = 1) 
FRCH_storm12c_08_15_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm12c_08_15_Q.csv", header=T, row.names = 1) 
FRCH_storm12d_08_21_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm12d_08_21_Q.csv", header=T, row.names = 1) 
FRCH_storm12e_08_23_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm12e_08_23_Q.csv", header=T, row.names = 1) 
FRCH_storm13_09_20_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm13_09_20_Q.csv", header=T, row.names = 1) 
FRCH_storm14_10_01_Q = read.csv("~/Documents/Storms/Storm_Events/2019/FRCH/FRCH_storm14_10_01_Q.csv", header=T, row.names = 1) 

# MOOS #
MOOS_storm1_06_01_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm1_06_01_Q.csv", header=T, row.names = 1) 
MOOS_storm3_07_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm3_07_12_Q.csv", header=T, row.names = 1) 
MOOS_storm4_07_25_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm4_07_25_Q.csv", header=T, row.names = 1) 
MOOS_storm5_07_29_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm5_07_29_Q.csv", header=T, row.names = 1) 
MOOS_storm6a_08_01_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm6a_08_01_Q.csv", header=T, row.names = 1) 
MOOS_storm6b_08_02_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm6b_08_02_Q.csv", header=T, row.names = 1) 
MOOS_storm6c_08_03_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm6c_08_03_Q.csv", header=T, row.names = 1) 
MOOS_storm6d_08_05_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm6d_08_05_Q.csv", header=T, row.names = 1) 
MOOS_storm7a_08_13_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm7a_08_13_Q.csv", header=T, row.names = 1) 
MOOS_storm7b_08_14_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm7b_08_14_Q.csv", header=T, row.names = 1) 
MOOS_storm7c_08_15_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm7c_08_15_Q.csv", header=T, row.names = 1) 
MOOS_storm8_09_21_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm8_09_21_Q.csv", header=T, row.names = 1) 
MOOS_storm9_10_02_Q = read.csv("~/Documents/Storms/Storm_Events/2019/MOOS/MOOS_storm9_10_02_Q.csv", header=T, row.names = 1) 

# POKE #
POKE_storm1_06_30_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm1_06_30_Q.csv", header=T, row.names = 1) 
POKE_storm2_07_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm2_07_12_Q.csv", header=T, row.names = 1) 
POKE_storm3_07_26_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm3_07_26_Q.csv", header=T, row.names = 1) 
POKE_storm4_07_31_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm4_07_31_Q.csv", header=T, row.names = 1) 
POKE_storm5a_08_02_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm5a_08_02_Q.csv", header=T, row.names = 1) 
POKE_storm5b_08_03_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm5b_08_03_Q.csv", header=T, row.names = 1) 
POKE_storm5c_08_05_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm5c_08_05_Q.csv", header=T, row.names = 1) 
POKE_storm5d_08_10_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm5d_08_10_Q.csv", header=T, row.names = 1) 
POKE_storm6a_08_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm6a_08_12_Q.csv", header=T, row.names = 1) 
POKE_storm6b_08_13_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm6b_08_13_Q.csv", header=T, row.names = 1) 
POKE_storm7_08_15_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm7_08_15_Q.csv", header=T, row.names = 1) 
POKE_storm8_09_29_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm8_09_29_Q.csv", header=T, row.names = 1) 
POKE_storm9_10_04_Q = read.csv("~/Documents/Storms/Storm_Events/2019/POKE/POKE_storm9_10_04_Q.csv", header=T, row.names = 1) 

# STRT #
STRT_storm1_05_31_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm1_05_31_Q.csv", header=T, row.names = 1) 
STRT_storm2_07_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm2_07_12_Q.csv", header=T, row.names = 1) 
STRT_storm3a_07_25_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm3a_07_25_Q.csv", header=T, row.names = 1) 
STRT_storm3b_08_05_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm3b_08_05_Q.csv", header=T, row.names = 1) 
STRT_storm3c_08_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm3c_08_12_Q.csv", header=T, row.names = 1) 
STRT_storm4_08_15_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm4_08_15_Q.csv", header=T, row.names = 1) 
STRT_storm5_08_20_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm5_08_20_Q.csv", header=T, row.names = 1) 
STRT_storm6_09_20_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm6_09_20_Q.csv", header=T, row.names = 1) 
STRT_storm7_10_01_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm7_10_01_Q.csv", header=T, row.names = 1) 
STRT_storm7b_10_04_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm7b_10_04_Q.csv", header=T, row.names = 1) 
STRT_storm7c_10_09_Q = read.csv("~/Documents/Storms/Storm_Events/2019/STRT/STRT_storm7c_10_09_Q.csv", header=T, row.names = 1) 

# VAUL #
VAUL_storm1_07_13_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm1_07_13_Q.csv", header=T, row.names = 1) 
VAUL_storm2_07_26_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm2_07_26_Q.csv", header=T, row.names = 1) 
VAUL_storm3_07_29_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm3_07_29_Q.csv", header=T, row.names = 1) 
VAUL_storm4a_08_02_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm4a_08_02_Q.csv", header=T, row.names = 1) 
VAUL_storm4b_08_03_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm4b_08_03_Q.csv", header=T, row.names = 1) 
VAUL_storm4c_08_05_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm4c_08_05_Q.csv", header=T, row.names = 1) 
VAUL_storm5_08_12_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm5_08_12_Q.csv", header=T, row.names = 1) 
VAUL_storm6_08_15_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm6_08_15_Q.csv", header=T, row.names = 1) 
VAUL_storm7_09_19_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm7_09_19_Q.csv", header=T, row.names = 1) 
VAUL_storm8a_09_29_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm8a_09_29_Q.csv", header=T, row.names = 1) 
VAUL_storm8b_10_01_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm8b_10_01_Q.csv", header=T, row.names = 1) 
VAUL_storm8c_10_04_Q = read.csv("~/Documents/Storms/Storm_Events/2019/VAUL/VAUL_storm8c_10_04_Q.csv", header=T, row.names = 1) 

### solute data with bursts ###
#NO3
FRCH_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/FRCH_NO3_storm_list.RData")
MOOS_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/MOOS_NO3_storm_list.RData")
POKE_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/POKE_NO3_storm_list.RData")
STRT_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/STRT_NO3_storm_list.RData")
VAUL_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/VAUL_NO3_storm_list.RData")

#fDOM
FRCH_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/FRCH_fDOM_storm_list.RData")
MOOS_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/MOOS_fDOM_storm_list.RData")
POKE_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/POKE_fDOM_storm_list.RData")
STRT_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/STRT_fDOM_storm_list.RData")
VAUL_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/VAUL_fDOM_storm_list.RData")

#SPC
FRCH_SPC_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/FRCH_SPC_storm_list.RData")
MOOS_SPC_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/MOOS_SPC_storm_list.RData")
POKE_SPC_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/POKE_SPC_storm_list.RData")
STRT_SPC_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/STRT_SPC_storm_list.RData")
VAUL_SPC_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/VAUL_SPC_storm_list.RData")

#turb
FRCH_turb_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/FRCH_turb_storm_list.RData")
MOOS_turb_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/MOOS_turb_storm_list.RData")
POKE_turb_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/POKE_turb_storm_list.RData")
STRT_turb_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/STRT_turb_storm_list.RData")
VAUL_turb_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2019/VAUL_turb_storm_list.RData")

# normalize Q data #
# FRCH
FRCH_storm1_05_31_Q$datavalue.norm = 
  (FRCH_storm1_05_31_Q$datavalue - min(FRCH_storm1_05_31_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm1_05_31_Q$datavalue, na.rm=T) - min(FRCH_storm1_05_31_Q$datavalue, na.rm=T))
FRCH_storm2_06_15_Q$datavalue.norm = 
  (FRCH_storm2_06_15_Q$datavalue - min(FRCH_storm2_06_15_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm2_06_15_Q$datavalue, na.rm=T) - min(FRCH_storm2_06_15_Q$datavalue, na.rm=T))
FRCH_storm3_06_18_Q$datavalue.norm = 
  (FRCH_storm3_06_18_Q$datavalue - min(FRCH_storm3_06_18_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm3_06_18_Q$datavalue, na.rm=T) - min(FRCH_storm3_06_18_Q$datavalue, na.rm=T))
FRCH_storm4_06_20_Q$datavalue.norm = 
  (FRCH_storm4_06_20_Q$datavalue - min(FRCH_storm4_06_20_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm4_06_20_Q$datavalue, na.rm=T) - min(FRCH_storm4_06_20_Q$datavalue, na.rm=T))
FRCH_storm5_06_22_Q$datavalue.norm = 
  (FRCH_storm5_06_22_Q$datavalue - min(FRCH_storm5_06_22_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm5_06_22_Q$datavalue, na.rm=T) - min(FRCH_storm5_06_22_Q$datavalue, na.rm=T))
FRCH_storm6_07_12_Q$datavalue.norm = 
  (FRCH_storm6_07_12_Q$datavalue - min(FRCH_storm6_07_12_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm6_07_12_Q$datavalue, na.rm=T) - min(FRCH_storm6_07_12_Q$datavalue, na.rm=T))
FRCH_storm7_07_25_Q$datavalue.norm = 
  (FRCH_storm7_07_25_Q$datavalue - min(FRCH_storm7_07_25_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm7_07_25_Q$datavalue, na.rm=T) - min(FRCH_storm7_07_25_Q$datavalue, na.rm=T))
FRCH_storm8_07_28_Q$datavalue.norm = 
  (FRCH_storm8_07_28_Q$datavalue - min(FRCH_storm8_07_28_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm8_07_28_Q$datavalue, na.rm=T) - min(FRCH_storm8_07_28_Q$datavalue, na.rm=T))
FRCH_storm9a_07_29_Q$datavalue.norm = 
  (FRCH_storm9a_07_29_Q$datavalue - min(FRCH_storm9a_07_29_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm9a_07_29_Q$datavalue, na.rm=T) - min(FRCH_storm9a_07_29_Q$datavalue, na.rm=T))
FRCH_storm9b_07_30_Q$datavalue.norm = 
  (FRCH_storm9b_07_30_Q$datavalue - min(FRCH_storm9b_07_30_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm9b_07_30_Q$datavalue, na.rm=T) - min(FRCH_storm9b_07_30_Q$datavalue, na.rm=T))
FRCH_storm10a_08_01_Q$datavalue.norm = 
  (FRCH_storm10a_08_01_Q$datavalue - min(FRCH_storm10a_08_01_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm10a_08_01_Q$datavalue, na.rm=T) - min(FRCH_storm10a_08_01_Q$datavalue, na.rm=T))
FRCH_storm10b_08_02_Q$datavalue.norm = 
  (FRCH_storm10b_08_02_Q$datavalue - min(FRCH_storm10b_08_02_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm10b_08_02_Q$datavalue, na.rm=T) - min(FRCH_storm10b_08_02_Q$datavalue, na.rm=T))
FRCH_storm10c_08_03_Q$datavalue.norm = 
  (FRCH_storm10c_08_03_Q$datavalue - min(FRCH_storm10c_08_03_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm10c_08_03_Q$datavalue, na.rm=T) - min(FRCH_storm10c_08_03_Q$datavalue, na.rm=T))
FRCH_storm11_08_05_Q$datavalue.norm = 
  (FRCH_storm11_08_05_Q$datavalue - min(FRCH_storm11_08_05_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm11_08_05_Q$datavalue, na.rm=T) - min(FRCH_storm11_08_05_Q$datavalue, na.rm=T))
FRCH_storm12a_08_12_Q$datavalue.norm = 
  (FRCH_storm12a_08_12_Q$datavalue - min(FRCH_storm12a_08_12_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm12a_08_12_Q$datavalue, na.rm=T) - min(FRCH_storm12a_08_12_Q$datavalue, na.rm=T))
FRCH_storm12b_08_14_Q$datavalue.norm = 
  (FRCH_storm12b_08_14_Q$datavalue - min(FRCH_storm12b_08_14_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm12b_08_14_Q$datavalue, na.rm=T) - min(FRCH_storm12b_08_14_Q$datavalue, na.rm=T))
FRCH_storm12c_08_15_Q$datavalue.norm = 
  (FRCH_storm12c_08_15_Q$datavalue - min(FRCH_storm12c_08_15_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm12c_08_15_Q$datavalue, na.rm=T) - min(FRCH_storm12c_08_15_Q$datavalue, na.rm=T))
FRCH_storm12d_08_21_Q$datavalue.norm = 
  (FRCH_storm12d_08_21_Q$datavalue - min(FRCH_storm12d_08_21_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm12d_08_21_Q$datavalue, na.rm=T) - min(FRCH_storm12d_08_21_Q$datavalue, na.rm=T))
FRCH_storm12e_08_23_Q$datavalue.norm = 
  (FRCH_storm12e_08_23_Q$datavalue - min(FRCH_storm12e_08_23_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm12e_08_23_Q$datavalue, na.rm=T) - min(FRCH_storm12e_08_23_Q$datavalue, na.rm=T))
FRCH_storm13_09_20_Q$datavalue.norm = 
  (FRCH_storm13_09_20_Q$datavalue - min(FRCH_storm13_09_20_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm13_09_20_Q$datavalue, na.rm=T) - min(FRCH_storm13_09_20_Q$datavalue, na.rm=T))
FRCH_storm14_10_01_Q$datavalue.norm = 
  (FRCH_storm14_10_01_Q$datavalue - min(FRCH_storm14_10_01_Q$datavalue, na.rm=T)) / 
  (max(FRCH_storm14_10_01_Q$datavalue, na.rm=T) - min(FRCH_storm14_10_01_Q$datavalue, na.rm=T))

#MOOS
MOOS_storm1_06_01_Q$datavalue.norm = 
  (MOOS_storm1_06_01_Q$datavalue - min(MOOS_storm1_06_01_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm1_06_01_Q$datavalue, na.rm=T) - min(MOOS_storm1_06_01_Q$datavalue, na.rm=T))
MOOS_storm3_07_12_Q$datavalue.norm = 
  (MOOS_storm3_07_12_Q$datavalue - min(MOOS_storm3_07_12_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm3_07_12_Q$datavalue, na.rm=T) - min(MOOS_storm3_07_12_Q$datavalue, na.rm=T))
MOOS_storm4_07_25_Q$datavalue.norm = 
  (MOOS_storm4_07_25_Q$datavalue - min(MOOS_storm4_07_25_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm4_07_25_Q$datavalue, na.rm=T) - min(MOOS_storm4_07_25_Q$datavalue, na.rm=T))
MOOS_storm5_07_29_Q$datavalue.norm = 
  (MOOS_storm5_07_29_Q$datavalue - min(MOOS_storm5_07_29_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm5_07_29_Q$datavalue, na.rm=T) - min(MOOS_storm5_07_29_Q$datavalue, na.rm=T))
MOOS_storm6a_08_01_Q$datavalue.norm = 
  (MOOS_storm6a_08_01_Q$datavalue - min(MOOS_storm6a_08_01_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm6a_08_01_Q$datavalue, na.rm=T) - min(MOOS_storm6a_08_01_Q$datavalue, na.rm=T))
MOOS_storm6b_08_02_Q$datavalue.norm = 
  (MOOS_storm6b_08_02_Q$datavalue - min(MOOS_storm6b_08_02_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm6b_08_02_Q$datavalue, na.rm=T) - min(MOOS_storm6b_08_02_Q$datavalue, na.rm=T))
MOOS_storm6c_08_03_Q$datavalue.norm = 
  (MOOS_storm6c_08_03_Q$datavalue - min(MOOS_storm6c_08_03_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm6c_08_03_Q$datavalue, na.rm=T) - min(MOOS_storm6c_08_03_Q$datavalue, na.rm=T))
MOOS_storm6d_08_05_Q$datavalue.norm = 
  (MOOS_storm6d_08_05_Q$datavalue - min(MOOS_storm6d_08_05_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm6d_08_05_Q$datavalue, na.rm=T) - min(MOOS_storm6d_08_05_Q$datavalue, na.rm=T))
MOOS_storm7a_08_13_Q$datavalue.norm = 
  (MOOS_storm7a_08_13_Q$datavalue - min(MOOS_storm7a_08_13_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm7a_08_13_Q$datavalue, na.rm=T) - min(MOOS_storm7a_08_13_Q$datavalue, na.rm=T))
MOOS_storm7b_08_14_Q$datavalue.norm = 
  (MOOS_storm7b_08_14_Q$datavalue - min(MOOS_storm7b_08_14_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm7b_08_14_Q$datavalue, na.rm=T) - min(MOOS_storm7b_08_14_Q$datavalue, na.rm=T))
MOOS_storm7c_08_15_Q$datavalue.norm = 
  (MOOS_storm7c_08_15_Q$datavalue - min(MOOS_storm7c_08_15_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm7c_08_15_Q$datavalue, na.rm=T) - min(MOOS_storm7c_08_15_Q$datavalue, na.rm=T))
MOOS_storm8_09_21_Q$datavalue.norm = 
  (MOOS_storm8_09_21_Q$datavalue - min(MOOS_storm8_09_21_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm8_09_21_Q$datavalue, na.rm=T) - min(MOOS_storm8_09_21_Q$datavalue, na.rm=T))
MOOS_storm9_10_02_Q$datavalue.norm = 
  (MOOS_storm9_10_02_Q$datavalue - min(MOOS_storm9_10_02_Q$datavalue, na.rm=T)) / 
  (max(MOOS_storm9_10_02_Q$datavalue, na.rm=T) - min(MOOS_storm9_10_02_Q$datavalue, na.rm=T))

#POKE
POKE_storm1_06_30_Q$datavalue.norm = 
  (POKE_storm1_06_30_Q$datavalue - min(POKE_storm1_06_30_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm1_06_30_Q$datavalue, na.rm=T) - min(POKE_storm1_06_30_Q$datavalue, na.rm=T))
POKE_storm2_07_12_Q$datavalue.norm = 
  (POKE_storm2_07_12_Q$datavalue - min(POKE_storm2_07_12_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm2_07_12_Q$datavalue, na.rm=T) - min(POKE_storm2_07_12_Q$datavalue, na.rm=T))
POKE_storm3_07_26_Q$datavalue.norm = 
  (POKE_storm3_07_26_Q$datavalue - min(POKE_storm3_07_26_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm3_07_26_Q$datavalue, na.rm=T) - min(POKE_storm3_07_26_Q$datavalue, na.rm=T))
POKE_storm4_07_31_Q$datavalue.norm = 
  (POKE_storm4_07_31_Q$datavalue - min(POKE_storm4_07_31_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm4_07_31_Q$datavalue, na.rm=T) - min(POKE_storm4_07_31_Q$datavalue, na.rm=T))
POKE_storm5a_08_02_Q$datavalue.norm = 
  (POKE_storm5a_08_02_Q$datavalue - min(POKE_storm5a_08_02_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm5a_08_02_Q$datavalue, na.rm=T) - min(POKE_storm5a_08_02_Q$datavalue, na.rm=T))
POKE_storm5b_08_03_Q$datavalue.norm = 
  (POKE_storm5b_08_03_Q$datavalue - min(POKE_storm5b_08_03_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm5b_08_03_Q$datavalue, na.rm=T) - min(POKE_storm5b_08_03_Q$datavalue, na.rm=T))
POKE_storm5c_08_05_Q$datavalue.norm = 
  (POKE_storm5c_08_05_Q$datavalue - min(POKE_storm5c_08_05_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm5c_08_05_Q$datavalue, na.rm=T) - min(POKE_storm5c_08_05_Q$datavalue, na.rm=T))
POKE_storm5d_08_10_Q$datavalue.norm = 
  (POKE_storm5d_08_10_Q$datavalue - min(POKE_storm5d_08_10_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm5d_08_10_Q$datavalue, na.rm=T) - min(POKE_storm5d_08_10_Q$datavalue, na.rm=T))
POKE_storm6a_08_12_Q$datavalue.norm = 
  (POKE_storm6a_08_12_Q$datavalue - min(POKE_storm6a_08_12_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm6a_08_12_Q$datavalue, na.rm=T) - min(POKE_storm6a_08_12_Q$datavalue, na.rm=T))
#POKE_storm6b_08_13_Q$datavalue.norm = 
 ## (POKE_storm6b_08_13_Q$datavalue - min(POKE_storm6b_08_13_Q$datavalue, na.rm=T)) / 
  #(max(POKE_storm6b_08_13_Q$datavalue, na.rm=T) - min(POKE_storm6b_08_13_Q$datavalue, na.rm=T))
POKE_storm7_08_15_Q$datavalue.norm = 
  (POKE_storm7_08_15_Q$datavalue - min(POKE_storm7_08_15_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm7_08_15_Q$datavalue, na.rm=T) - min(POKE_storm7_08_15_Q$datavalue, na.rm=T))
POKE_storm8_09_29_Q$datavalue.norm = 
  (POKE_storm8_09_29_Q$datavalue - min(POKE_storm8_09_29_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm8_09_29_Q$datavalue, na.rm=T) - min(POKE_storm8_09_29_Q$datavalue, na.rm=T))
POKE_storm9_10_04_Q$datavalue.norm = 
  (POKE_storm9_10_04_Q$datavalue - min(POKE_storm9_10_04_Q$datavalue, na.rm=T)) / 
  (max(POKE_storm9_10_04_Q$datavalue, na.rm=T) - min(POKE_storm9_10_04_Q$datavalue, na.rm=T))

#STRT
STRT_storm1_05_31_Q$datavalue.norm = 
  (STRT_storm1_05_31_Q$datavalue - min(STRT_storm1_05_31_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm1_05_31_Q$datavalue, na.rm=T) - min(STRT_storm1_05_31_Q$datavalue, na.rm=T))
STRT_storm2_07_12_Q$datavalue.norm = 
  (STRT_storm2_07_12_Q$datavalue - min(STRT_storm2_07_12_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm2_07_12_Q$datavalue, na.rm=T) - min(STRT_storm2_07_12_Q$datavalue, na.rm=T))
STRT_storm3a_07_25_Q$datavalue.norm = 
  (STRT_storm3a_07_25_Q$datavalue - min(STRT_storm3a_07_25_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm3a_07_25_Q$datavalue, na.rm=T) - min(STRT_storm3a_07_25_Q$datavalue, na.rm=T))
STRT_storm3b_08_05_Q$datavalue.norm = 
  (STRT_storm3b_08_05_Q$datavalue - min(STRT_storm3b_08_05_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm3b_08_05_Q$datavalue, na.rm=T) - min(STRT_storm3b_08_05_Q$datavalue, na.rm=T))
STRT_storm3c_08_12_Q$datavalue.norm = 
  (STRT_storm3c_08_12_Q$datavalue - min(STRT_storm3c_08_12_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm3c_08_12_Q$datavalue, na.rm=T) - min(STRT_storm3c_08_12_Q$datavalue, na.rm=T))
STRT_storm4_08_15_Q$datavalue.norm = 
  (STRT_storm4_08_15_Q$datavalue - min(STRT_storm4_08_15_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm4_08_15_Q$datavalue, na.rm=T) - min(STRT_storm4_08_15_Q$datavalue, na.rm=T))
STRT_storm5_08_20_Q$datavalue.norm = 
  (STRT_storm5_08_20_Q$datavalue - min(STRT_storm5_08_20_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm5_08_20_Q$datavalue, na.rm=T) - min(STRT_storm5_08_20_Q$datavalue, na.rm=T))
STRT_storm6_09_20_Q$datavalue.norm = 
  (STRT_storm6_09_20_Q$datavalue - min(STRT_storm6_09_20_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm6_09_20_Q$datavalue, na.rm=T) - min(STRT_storm6_09_20_Q$datavalue, na.rm=T))
STRT_storm7_10_01_Q$datavalue.norm = 
  (STRT_storm7_10_01_Q$datavalue - min(STRT_storm7_10_01_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm7_10_01_Q$datavalue, na.rm=T) - min(STRT_storm7_10_01_Q$datavalue, na.rm=T))
STRT_storm7b_10_04_Q$datavalue.norm = 
  (STRT_storm7b_10_04_Q$datavalue - min(STRT_storm7b_10_04_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm7b_10_04_Q$datavalue, na.rm=T) - min(STRT_storm7b_10_04_Q$datavalue, na.rm=T))
STRT_storm7c_10_09_Q$datavalue.norm = 
  (STRT_storm7c_10_09_Q$datavalue - min(STRT_storm7c_10_09_Q$datavalue, na.rm=T)) / 
  (max(STRT_storm7c_10_09_Q$datavalue, na.rm=T) - min(STRT_storm7c_10_09_Q$datavalue, na.rm=T))

#VAUL
VAUL_storm1_07_13_Q$datavalue.norm = 
  (VAUL_storm1_07_13_Q$datavalue - min(VAUL_storm1_07_13_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm1_07_13_Q$datavalue, na.rm=T) - min(VAUL_storm1_07_13_Q$datavalue, na.rm=T))
VAUL_storm2_07_26_Q$datavalue.norm = 
  (VAUL_storm2_07_26_Q$datavalue - min(VAUL_storm2_07_26_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm2_07_26_Q$datavalue, na.rm=T) - min(VAUL_storm2_07_26_Q$datavalue, na.rm=T))
VAUL_storm3_07_29_Q$datavalue.norm = 
  (VAUL_storm3_07_29_Q$datavalue - min(VAUL_storm3_07_29_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm3_07_29_Q$datavalue, na.rm=T) - min(VAUL_storm3_07_29_Q$datavalue, na.rm=T))
VAUL_storm4a_08_02_Q$datavalue.norm = 
  (VAUL_storm4a_08_02_Q$datavalue - min(VAUL_storm4a_08_02_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm4a_08_02_Q$datavalue, na.rm=T) - min(VAUL_storm4a_08_02_Q$datavalue, na.rm=T))
VAUL_storm4b_08_03_Q$datavalue.norm = 
  (VAUL_storm4b_08_03_Q$datavalue - min(VAUL_storm4b_08_03_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm4b_08_03_Q$datavalue, na.rm=T) - min(VAUL_storm4b_08_03_Q$datavalue, na.rm=T))
VAUL_storm4c_08_05_Q$datavalue.norm = 
  (VAUL_storm4c_08_05_Q$datavalue - min(VAUL_storm4c_08_05_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm4c_08_05_Q$datavalue, na.rm=T) - min(VAUL_storm4c_08_05_Q$datavalue, na.rm=T))
VAUL_storm5_08_12_Q$datavalue.norm = 
  (VAUL_storm5_08_12_Q$datavalue - min(VAUL_storm5_08_12_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm5_08_12_Q$datavalue, na.rm=T) - min(VAUL_storm5_08_12_Q$datavalue, na.rm=T))
VAUL_storm6_08_15_Q$datavalue.norm = 
  (VAUL_storm6_08_15_Q$datavalue - min(VAUL_storm6_08_15_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm6_08_15_Q$datavalue, na.rm=T) - min(VAUL_storm6_08_15_Q$datavalue, na.rm=T))
VAUL_storm7_09_19_Q$datavalue.norm = 
  (VAUL_storm7_09_19_Q$datavalue - min(VAUL_storm7_09_19_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm7_09_19_Q$datavalue, na.rm=T) - min(VAUL_storm7_09_19_Q$datavalue, na.rm=T))
VAUL_storm8a_09_29_Q$datavalue.norm = 
  (VAUL_storm8a_09_29_Q$datavalue - min(VAUL_storm8a_09_29_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm8a_09_29_Q$datavalue, na.rm=T) - min(VAUL_storm8a_09_29_Q$datavalue, na.rm=T))
VAUL_storm8b_10_01_Q$datavalue.norm = 
  (VAUL_storm8b_10_01_Q$datavalue - min(VAUL_storm8b_10_01_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm8b_10_01_Q$datavalue, na.rm=T) - min(VAUL_storm8b_10_01_Q$datavalue, na.rm=T))
VAUL_storm8c_10_04_Q$datavalue.norm = 
  (VAUL_storm8c_10_04_Q$datavalue - min(VAUL_storm8c_10_04_Q$datavalue, na.rm=T)) / 
  (max(VAUL_storm8c_10_04_Q$datavalue, na.rm=T) - min(VAUL_storm8c_10_04_Q$datavalue, na.rm=T))

# normalize solute data #

### remove burst-complied data ###

#NO3
for(i in 1:length(FRCH_NO3_storm_list)){
  FRCH_NO3_storm_list[[i]][["datavalue"]] = FRCH_NO3_storm_list[[i]][["nitrateuM"]]
  FRCH_NO3_storm_list[[i]][["nitrateuM"]] = NULL
}

for(i in 1:length(MOOS_NO3_storm_list)){
  MOOS_NO3_storm_list[[i]][["datavalue"]] = MOOS_NO3_storm_list[[i]][["nitrateuM"]]
  MOOS_NO3_storm_list[[i]][["nitrateuM"]] = NULL
}

for(i in 1:length(POKE_NO3_storm_list)){
  POKE_NO3_storm_list[[i]][["datavalue"]] = POKE_NO3_storm_list[[i]][["nitrateuM"]]
  POKE_NO3_storm_list[[i]][["nitrateuM"]] = NULL
}

for(i in 1:length(STRT_NO3_storm_list)){
  STRT_NO3_storm_list[[i]][["datavalue"]] = STRT_NO3_storm_list[[i]][["nitrateuM"]]
  STRT_NO3_storm_list[[i]][["nitrateuM"]] = NULL
}

for(i in 1:length(VAUL_NO3_storm_list)){
  VAUL_NO3_storm_list[[i]][["datavalue"]] = VAUL_NO3_storm_list[[i]][["nitrateuM"]]
  VAUL_NO3_storm_list[[i]][["nitrateuM"]] = NULL
}

#fDOM
for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]][["datavalue"]] = FRCH_fDOM_storm_list[[i]][["fDOM.QSU.mn"]]
  FRCH_fDOM_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  FRCH_fDOM_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  FRCH_fDOM_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(MOOS_fDOM_storm_list)){
  MOOS_fDOM_storm_list[[i]][["datavalue"]] = MOOS_fDOM_storm_list[[i]][["fDOM.QSU.mn"]]
  MOOS_fDOM_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  MOOS_fDOM_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  MOOS_fDOM_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(POKE_fDOM_storm_list)){
  POKE_fDOM_storm_list[[i]][["datavalue"]] = POKE_fDOM_storm_list[[i]][["fDOM.QSU.mn"]]
  POKE_fDOM_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  POKE_fDOM_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  POKE_fDOM_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(STRT_fDOM_storm_list)){
  STRT_fDOM_storm_list[[i]][["datavalue"]] = STRT_fDOM_storm_list[[i]][["fDOM.QSU.mn"]]
  STRT_fDOM_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  STRT_fDOM_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  STRT_fDOM_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(VAUL_fDOM_storm_list)){
  VAUL_fDOM_storm_list[[i]][["datavalue"]] = VAUL_fDOM_storm_list[[i]][["fDOM.QSU.mn"]]
  VAUL_fDOM_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  VAUL_fDOM_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  VAUL_fDOM_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

#SPC
for(i in 1:length(FRCH_SPC_storm_list)){
  FRCH_SPC_storm_list[[i]][["datavalue"]] = FRCH_SPC_storm_list[[i]][["SpCond.µS.cm"]]
  FRCH_SPC_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  FRCH_SPC_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  FRCH_SPC_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(MOOS_SPC_storm_list)){
  MOOS_SPC_storm_list[[i]][["datavalue"]] = MOOS_SPC_storm_list[[i]][["SpCond.µS.cm"]]
  MOOS_SPC_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  MOOS_SPC_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  MOOS_SPC_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(POKE_SPC_storm_list)){
  POKE_SPC_storm_list[[i]][["datavalue"]] = POKE_SPC_storm_list[[i]][["SpCond.µS.cm"]]
  POKE_SPC_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  POKE_SPC_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  POKE_SPC_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(STRT_SPC_storm_list)){
  STRT_SPC_storm_list[[i]][["datavalue"]] = STRT_SPC_storm_list[[i]][["SpCond.µS.cm"]]
  STRT_SPC_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  STRT_SPC_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  STRT_SPC_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

for(i in 1:length(VAUL_SPC_storm_list)){
  VAUL_SPC_storm_list[[i]][["datavalue"]] = VAUL_SPC_storm_list[[i]][["SpCond.µS.cm"]]
  VAUL_SPC_storm_list[[i]][["SpCond.µS.cm"]] = NULL
  VAUL_SPC_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  VAUL_SPC_storm_list[[i]][["Turbidity.FNU"]] = NULL
}

#turb
for(i in 1:length(FRCH_turb_storm_list)){
  FRCH_turb_storm_list[[i]][["datavalue"]] = FRCH_turb_storm_list[[i]][["Turbidity.FNU"]]
  FRCH_turb_storm_list[[i]][["Turbidity.FNU"]] = NULL
  FRCH_turb_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  FRCH_turb_storm_list[[i]][["SpCond.µS.cm"]] = NULL
}

for(i in 1:length(MOOS_turb_storm_list)){
  MOOS_turb_storm_list[[i]][["datavalue"]] = MOOS_turb_storm_list[[i]][["Turbidity.FNU"]]
  MOOS_turb_storm_list[[i]][["Turbidity.FNU"]] = NULL
  MOOS_turb_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  MOOS_turb_storm_list[[i]][["SpCond.µS.cm"]] = NULL
}

for(i in 1:length(POKE_turb_storm_list)){
  POKE_turb_storm_list[[i]][["datavalue"]] = POKE_turb_storm_list[[i]][["Turbidity.FNU"]]
  POKE_turb_storm_list[[i]][["Turbidity.FNU"]] = NULL
  POKE_turb_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  POKE_turb_storm_list[[i]][["SpCond.µS.cm"]] = NULL
}

for(i in 1:length(STRT_turb_storm_list)){
  STRT_turb_storm_list[[i]][["datavalue"]] = STRT_turb_storm_list[[i]][["Turbidity.FNU"]]
  STRT_turb_storm_list[[i]][["Turbidity.FNU"]] = NULL
  STRT_turb_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  STRT_turb_storm_list[[i]][["SpCond.µS.cm"]] = NULL
}

for(i in 1:length(VAUL_turb_storm_list)){
  VAUL_turb_storm_list[[i]][["datavalue"]] = VAUL_turb_storm_list[[i]][["Turbidity.FNU"]]
  VAUL_turb_storm_list[[i]][["Turbidity.FNU"]] = NULL
  VAUL_turb_storm_list[[i]][["fDOM.QSU.mn"]] = NULL
  VAUL_turb_storm_list[[i]][["SpCond.µS.cm"]] = NULL
}

### normalize burst data ###

#NO3
for(i in 1:length(FRCH_NO3_storm_list)){
  FRCH_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list[[i]][["datavalue"]]-min(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_NO3_storm_list)){
  MOOS_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_NO3_storm_list[[i]][["datavalue"]]-min(MOOS_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_NO3_storm_list)){
  POKE_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (POKE_NO3_storm_list[[i]][["datavalue"]]-min(POKE_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(POKE_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_NO3_storm_list)){
  STRT_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (STRT_NO3_storm_list[[i]][["datavalue"]]-min(STRT_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(STRT_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_NO3_storm_list)){
  VAUL_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (VAUL_NO3_storm_list[[i]][["datavalue"]]-min(VAUL_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(VAUL_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}


#fDOM
for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_fDOM_storm_list[[i]][["datavalue"]]-min(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_fDOM_storm_list)){
  MOOS_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_fDOM_storm_list[[i]][["datavalue"]]-min(MOOS_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_fDOM_storm_list)){
  POKE_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (POKE_fDOM_storm_list[[i]][["datavalue"]]-min(POKE_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(POKE_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_fDOM_storm_list)){
  STRT_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (STRT_fDOM_storm_list[[i]][["datavalue"]]-min(STRT_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(STRT_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_fDOM_storm_list)){
  VAUL_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (VAUL_fDOM_storm_list[[i]][["datavalue"]]-min(VAUL_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(VAUL_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

#SPC
for(i in 1:length(FRCH_SPC_storm_list)){
  FRCH_SPC_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_SPC_storm_list[[i]][["datavalue"]]-min(FRCH_SPC_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_SPC_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_SPC_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_SPC_storm_list)){
  MOOS_SPC_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_SPC_storm_list[[i]][["datavalue"]]-min(MOOS_SPC_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_SPC_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_SPC_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_SPC_storm_list)){
  POKE_SPC_storm_list[[i]][["datavalue.norm"]] = 
    (POKE_SPC_storm_list[[i]][["datavalue"]]-min(POKE_SPC_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_SPC_storm_list[[i]][["datavalue"]], na.rm=T)-min(POKE_SPC_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_SPC_storm_list)){
  STRT_SPC_storm_list[[i]][["datavalue.norm"]] = 
    (STRT_SPC_storm_list[[i]][["datavalue"]]-min(STRT_SPC_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_SPC_storm_list[[i]][["datavalue"]], na.rm=T)-min(STRT_SPC_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_SPC_storm_list)){
  VAUL_SPC_storm_list[[i]][["datavalue.norm"]] = 
    (VAUL_SPC_storm_list[[i]][["datavalue"]]-min(VAUL_SPC_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_SPC_storm_list[[i]][["datavalue"]], na.rm=T)-min(VAUL_SPC_storm_list[[i]][["datavalue"]], na.rm=T))
}

#turb
for(i in 1:length(FRCH_turb_storm_list)){
  FRCH_turb_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_turb_storm_list[[i]][["datavalue"]]-min(FRCH_turb_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_turb_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_turb_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_turb_storm_list)){
  MOOS_turb_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_turb_storm_list[[i]][["datavalue"]]-min(MOOS_turb_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_turb_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_turb_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_turb_storm_list)){
  POKE_turb_storm_list[[i]][["datavalue.norm"]] = 
    (POKE_turb_storm_list[[i]][["datavalue"]]-min(POKE_turb_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_turb_storm_list[[i]][["datavalue"]], na.rm=T)-min(POKE_turb_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_turb_storm_list)){
  STRT_turb_storm_list[[i]][["datavalue.norm"]] = 
    (STRT_turb_storm_list[[i]][["datavalue"]]-min(STRT_turb_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_turb_storm_list[[i]][["datavalue"]], na.rm=T)-min(STRT_turb_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_turb_storm_list)){
  VAUL_turb_storm_list[[i]][["datavalue.norm"]] = 
    (VAUL_turb_storm_list[[i]][["datavalue"]]-min(VAUL_turb_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_turb_storm_list[[i]][["datavalue"]], na.rm=T)-min(VAUL_turb_storm_list[[i]][["datavalue"]], na.rm=T))
}

# fxn: calculate FI by difference and bootstrap CIs #

FI_diff = function(dat_Q, dat_response) {
  FI_dat = rbind(dat_response[as.POSIXct(dat_response$valuedatetime) == min(as.POSIXct(dat_response$valuedatetime)),], 
                 dat_response[as.POSIXct(dat_response$valuedatetime) == as.POSIXct(dat_Q$valuedatetime[dat_Q$datavalue.norm == max(dat_Q$datavalue.norm)]),])
  
  FI_dat$valuedatetime = as.character(as.POSIXct(FI_dat$valuedatetime))
  
  dat_Q$valuedatetime = as.character(as.POSIXct(dat_Q$valuedatetime))
  
  FI_dat = left_join(FI_dat, 
                     subset(dat_Q, select=c("valuedatetime", "datavalue.norm")),
                     by="valuedatetime")
  
  names(FI_dat) = c("valuedatetime", "datavalue", "datavalue.norm", "Q")
  
  FI_dat$datavalue.norm = as.numeric(FI_dat$datavalue.norm)
  FI_dat$Q = as.numeric(FI_dat$Q)
  
  FI = mean(FI_dat$datavalue.norm[FI_dat$valuedatetime == max(FI_dat$valuedatetime)]) - mean(FI_dat$datavalue.norm[FI_dat$valuedatetime == min(FI_dat$valuedatetime)])
  
  meanDiff = function(data, indices) { 
    d <- data[indices,] # allows boot to select sample
    m1 = mean(d$datavalue.norm[d$valuedatetime == max(d$valuedatetime)])
    m2 = mean(d$datavalue.norm[d$valuedatetime == min(d$valuedatetime)])
    m = m1 - m2
    return(m)
  }
  
  FI_boot = boot(FI_dat, meanDiff, R = 10000, strata = as.factor(FI_dat[,1]))
  FI_bootCI = boot.ci(FI_boot, type="bca")
  
  FI_bootCI = data.frame(cbind(FI_boot$t0, FI_bootCI[["bca"]][4], FI_bootCI[["bca"]][5]))
  names(FI_bootCI) = c("FI", "lower", "upper")
  
  FI_results = list(FI_dat, FI_bootCI)
  
  return(FI_results)
}
FRCH_storm1_05_31_NO3_test = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm1_05_31_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm1_05_31_NO3`)

# calculate FI by difference and bootstrap CIs #
# FRCH # 
#NO3 #
FRCH_storm1_05_31_NO3_FI = FI_diff(FRCH_storm1_05_31_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm1_05_31_NO3`)
FRCH_storm2_06_15_NO3_FI = FI_diff(FRCH_storm2_06_15_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm2_06_15_NO3`)
FRCH_storm3_06_18_NO3_FI = FI_diff(FRCH_storm3_06_18_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm3_06_18_NO3`)
FRCH_storm4_06_20_NO3_FI = FI_diff(FRCH_storm4_06_20_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm4_06_20_NO3`)
FRCH_storm5_06_22_NO3_FI =  FI_diff(FRCH_storm5_06_22_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm5_06_22_NO3`)
FRCH_storm6_07_12_NO3_FI = FI_diff(FRCH_storm6_07_12_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm6_07_12_NO3`)
FRCH_storm7_07_25_NO3_FI = FI_diff(FRCH_storm7_07_25_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm7_07_25_NO3`)
#FRCH_storm8_07_28_NO3_FI = FI_diff(FRCH_storm8_07_28_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm8_07_28_NO3`)
FRCH_storm9a_07_29_NO3_FI = FI_diff(FRCH_storm9a_07_29_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9a_07_29_NO3`)
FRCH_storm9b_07_30_NO3_FI = FI_diff(FRCH_storm9b_07_30_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9b_07_30_NO3`)
FRCH_storm10a_08_01_NO3_FI =FI_diff(FRCH_storm10a_08_01_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10a_08_01_NO3`)
FRCH_storm10b_08_02_NO3_FI = FI_diff(FRCH_storm10b_08_02_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10b_08_02_NO3`)
#FRCH_storm10c_08_03_NO3_FI = FI_diff(FRCH_storm10c_08_03_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10c_08_03_NO3`)
FRCH_storm11_08_05_NO3_FI = FI_diff(FRCH_storm11_08_05_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm11_08_05_NO3`)
FRCH_storm12a_08_12_NO3_FI =  FI_diff(FRCH_storm12a_08_12_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12a_08_12_NO3`)
FRCH_storm12b_08_14_NO3_FI = FI_diff(FRCH_storm12b_08_14_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12b_08_14_NO3`)
FRCH_storm12c_08_15_NO3_FI =  FI_diff(FRCH_storm12c_08_15_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12c_08_15_NO3`)
FRCH_storm12d_08_21_NO3_FI = FI_diff(FRCH_storm12d_08_21_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12d_08_21_NO3`)
FRCH_storm12e_08_23_NO3_FI = FI_diff(FRCH_storm12e_08_23_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12e_08_23_NO3`)
FRCH_storm13_09_20_NO3_FI = FI_diff(FRCH_storm13_09_20_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm13_09_20_NO3`)
FRCH_storm14_10_01_NO3_FI =FI_diff(FRCH_storm14_10_01_Q, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm14_10_01_NO3`)
  
#fDOM #
FRCH_storm1_05_31_fDOM_FI = FI_diff(FRCH_storm1_05_31_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm1_05_31_fDOM`)
FRCH_storm2_06_15_fDOM_FI = FI_diff(FRCH_storm2_06_15_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm2_06_15_fDOM`)
FRCH_storm3_06_18_fDOM_FI = FI_diff(FRCH_storm3_06_18_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm3_06_18_fDOM`)
FRCH_storm4_06_20_fDOM_FI = FI_diff(FRCH_storm4_06_20_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm4_06_20_fDOM`)
FRCH_storm5_06_22_fDOM_FI =  FI_diff(FRCH_storm5_06_22_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm5_06_22_fDOM`)
#FRCH_storm6_07_12_fDOM_FI = FI_diff(FRCH_storm6_07_12_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm6_07_12_fDOM`)
FRCH_storm7_07_25_fDOM_FI = FI_diff(FRCH_storm7_07_25_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm7_07_25_fDOM`)
#FRCH_storm8_07_28_fDOM_FI = FI_diff(FRCH_storm8_07_28_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm8_07_28_fDOM`)
FRCH_storm9a_07_29_fDOM_FI = FI_diff(FRCH_storm9a_07_29_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9a_07_29_fDOM`)
FRCH_storm9b_07_30_fDOM_FI = FI_diff(FRCH_storm9b_07_30_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9b_07_30_fDOM`)
FRCH_storm10a_08_01_fDOM_FI =FI_diff(FRCH_storm10a_08_01_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10a_08_01_fDOM`)
FRCH_storm10b_08_02_fDOM_FI = FI_diff(FRCH_storm10b_08_02_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10b_08_02_fDOM`)
FRCH_storm10c_08_03_fDOM_FI = FI_diff(FRCH_storm10c_08_03_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10c_08_03_fDOM`)
FRCH_storm11_08_05_fDOM_FI = FI_diff(FRCH_storm11_08_05_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm11_08_05_fDOM`)
FRCH_storm12a_08_12_fDOM_FI =  FI_diff(FRCH_storm12a_08_12_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12a_08_12_fDOM`)
FRCH_storm12b_08_14_fDOM_FI = FI_diff(FRCH_storm12b_08_14_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12b_08_14_fDOM`)
FRCH_storm12c_08_15_fDOM_FI =  FI_diff(FRCH_storm12c_08_15_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12c_08_15_fDOM`)
FRCH_storm12d_08_21_fDOM_FI = FI_diff(FRCH_storm12d_08_21_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12d_08_21_fDOM`)
FRCH_storm12e_08_23_fDOM_FI = FI_diff(FRCH_storm12e_08_23_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12e_08_23_fDOM`)
#FRCH_storm13_09_20_fDOM_FI = FI_diff(FRCH_storm13_09_20_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm13_09_20_fDOM`)
FRCH_storm14_10_01_fDOM_FI =FI_diff(FRCH_storm14_10_01_Q, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm14_10_01_fDOM`)

# SPC #
FRCH_storm1_05_31_SPC_FI = FI_diff(FRCH_storm1_05_31_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm1_05_31_SPC`)
FRCH_storm2_06_15_SPC_FI = FI_diff(FRCH_storm2_06_15_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm2_06_15_SPC`)
FRCH_storm3_06_18_SPC_FI = FI_diff(FRCH_storm3_06_18_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm3_06_18_SPC`)
FRCH_storm4_06_20_SPC_FI = FI_diff(FRCH_storm4_06_20_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm4_06_20_SPC`)
FRCH_storm5_06_22_SPC_FI =  FI_diff(FRCH_storm5_06_22_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm5_06_22_SPC`)
#FRCH_storm6_07_12_SPC_FI = FI_diff(FRCH_storm6_07_12_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm6_07_12_SPC`)
FRCH_storm7_07_25_SPC_FI = FI_diff(FRCH_storm7_07_25_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm7_07_25_SPC`)
#FRCH_storm8_07_28_SPC_FI = FI_diff(FRCH_storm8_07_28_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm8_07_28_SPC`)
FRCH_storm9a_07_29_SPC_FI = FI_diff(FRCH_storm9a_07_29_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9a_07_29_SPC`)
FRCH_storm9b_07_30_SPC_FI = FI_diff(FRCH_storm9b_07_30_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9b_07_30_SPC`)
FRCH_storm10a_08_01_SPC_FI =FI_diff(FRCH_storm10a_08_01_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10a_08_01_SPC`)
FRCH_storm10b_08_02_SPC_FI = FI_diff(FRCH_storm10b_08_02_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10b_08_02_SPC`)
FRCH_storm10c_08_03_SPC_FI = FI_diff(FRCH_storm10c_08_03_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10c_08_03_SPC`)
FRCH_storm11_08_05_SPC_FI = FI_diff(FRCH_storm11_08_05_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm11_08_05_SPC`)
FRCH_storm12a_08_12_SPC_FI =  FI_diff(FRCH_storm12a_08_12_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12a_08_12_SPC`)
FRCH_storm12b_08_14_SPC_FI = FI_diff(FRCH_storm12b_08_14_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12b_08_14_SPC`)
FRCH_storm12c_08_15_SPC_FI =  FI_diff(FRCH_storm12c_08_15_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12c_08_15_SPC`)
FRCH_storm12d_08_21_SPC_FI = FI_diff(FRCH_storm12d_08_21_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12d_08_21_SPC`)
FRCH_storm12e_08_23_SPC_FI = FI_diff(FRCH_storm12e_08_23_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12e_08_23_SPC`)
#FRCH_storm13_09_20_SPC_FI = FI_diff(FRCH_storm13_09_20_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm13_09_20_SPC`)
FRCH_storm14_10_01_SPC_FI =FI_diff(FRCH_storm14_10_01_Q, FRCH_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm14_10_01_SPC`)

# turb #
FRCH_storm1_05_31_turb_FI = FI_diff(FRCH_storm1_05_31_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm1_05_31_Turb`)
FRCH_storm2_06_15_turb_FI = FI_diff(FRCH_storm2_06_15_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm2_06_15_Turb`)
FRCH_storm3_06_18_turb_FI = FI_diff(FRCH_storm3_06_18_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm3_06_18_Turb`)
FRCH_storm4_06_20_turb_FI = FI_diff(FRCH_storm4_06_20_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm4_06_20_Turb`)
FRCH_storm5_06_22_turb_FI =  FI_diff(FRCH_storm5_06_22_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm5_06_22_Turb`)
#FRCH_storm6_07_12_turb_FI = FI_diff(FRCH_storm6_07_12_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm6_07_12_Turb`)
FRCH_storm7_07_25_turb_FI = FI_diff(FRCH_storm7_07_25_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm7_07_25_Turb`)
#FRCH_storm8_07_28_turb_FI = FI_diff(FRCH_storm8_07_28_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm8_07_28_Turb`)
FRCH_storm9a_07_29_turb_FI = FI_diff(FRCH_storm9a_07_29_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9a_07_29_Turb`)
FRCH_storm9b_07_30_turb_FI = FI_diff(FRCH_storm9b_07_30_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm9b_07_30_Turb`)
FRCH_storm10a_08_01_turb_FI =FI_diff(FRCH_storm10a_08_01_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10a_08_01_Turb`)
FRCH_storm10b_08_02_turb_FI = FI_diff(FRCH_storm10b_08_02_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10b_08_02_Turb`)
FRCH_storm10c_08_03_turb_FI = FI_diff(FRCH_storm10c_08_03_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm10c_08_03_Turb`)
FRCH_storm11_08_05_turb_FI = FI_diff(FRCH_storm11_08_05_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm11_08_05_Turb`)
FRCH_storm12a_08_12_turb_FI =  FI_diff(FRCH_storm12a_08_12_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12a_08_12_Turb`)
FRCH_storm12b_08_14_turb_FI = FI_diff(FRCH_storm12b_08_14_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12b_08_14_Turb`)
FRCH_storm12c_08_15_turb_FI =  FI_diff(FRCH_storm12c_08_15_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12c_08_15_Turb`)
FRCH_storm12d_08_21_turb_FI = FI_diff(FRCH_storm12d_08_21_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12d_08_21_Turb`)
FRCH_storm12e_08_23_turb_FI = FI_diff(FRCH_storm12e_08_23_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm12e_08_23_Turb`)
#FRCH_storm13_09_20_turb_FI = FI_diff(FRCH_storm13_09_20_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm13_09_20_Turb`)
FRCH_storm14_10_01_turb_FI =FI_diff(FRCH_storm14_10_01_Q, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//FRCH_storm14_10_01_Turb`)

# MOOS # 
#NO3 #
MOOS_storm1_06_01_NO3_FI = FI_diff(MOOS_storm1_06_01_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm1_06_01_NO3`)
MOOS_storm3_07_12_NO3_FI = FI_diff(MOOS_storm3_07_12_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm3_07_12_NO3`)
#MOOS_storm4_07_25_NO3_FI= FI_diff(MOOS_storm4_07_25_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm4_07_25_NO3`)
MOOS_storm5_07_29_NO3_FI= FI_diff(MOOS_storm5_07_29_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm5_07_29_NO3`)
MOOS_storm6a_08_01_NO3_FI= FI_diff(MOOS_storm6a_08_01_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6a_08_01_NO3`)
#MOOS_storm6b_08_02_NO3_FI= FI_diff(MOOS_storm6b_08_02_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6b_08_02_NO3`)
#MOOS_storm6c_08_03_NO3_FI= FI_diff(MOOS_storm6c_08_03_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6c_08_03_NO3`)
MOOS_storm7a_08_13_NO3_FI= FI_diff(MOOS_storm7a_08_13_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7a_08_13_NO3`)
MOOS_storm7b_08_14_NO3_FI= FI_diff(MOOS_storm7b_08_14_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7b_08_14_NO3`)
MOOS_storm7c_08_15_NO3_FI= FI_diff(MOOS_storm7c_08_15_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7c_08_15_NO3`)
MOOS_storm8_09_21_NO3_FI= FI_diff(MOOS_storm8_09_21_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm8_09_21_NO3`)
MOOS_storm9_10_02_NO3_FI= FI_diff(MOOS_storm9_10_02_Q, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm9_10_02_NO3`)

#fDOM #
MOOS_storm1_06_01_fDOM_FI = FI_diff(MOOS_storm1_06_01_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm1_06_01_fDOM`)
#MOOS_storm3_07_12_fDOM_FI = FI_diff(MOOS_storm3_07_12_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm3_07_12_fDOM`)
#MOOS_storm4_07_25_fDOM_FI= FI_diff(MOOS_storm4_07_25_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm4_07_25_fDOM`)
#MOOS_storm5_07_29_fDOM_FI= FI_diff(MOOS_storm5_07_29_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm5_07_29_fDOM`)
MOOS_storm6a_08_01_fDOM_FI= FI_diff(MOOS_storm6a_08_01_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6a_08_01_fDOM`)
#MOOS_storm6b_08_02_fDOM_FI= FI_diff(MOOS_storm6b_08_02_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6b_08_02_fDOM`)
MOOS_storm6c_08_03_fDOM_FI= FI_diff(MOOS_storm6c_08_03_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6c_08_03_fDOM`)
MOOS_storm7a_08_13_fDOM_FI= FI_diff(MOOS_storm7a_08_13_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7a_08_13_fDOM`)
MOOS_storm7b_08_14_fDOM_FI= FI_diff(MOOS_storm7b_08_14_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7b_08_14_fDOM`)
MOOS_storm7c_08_15_fDOM_FI= FI_diff(MOOS_storm7c_08_15_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7c_08_15_fDOM`)
#MOOS_storm8_09_21_fDOM_FI= FI_diff(MOOS_storm8_09_21_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm8_09_21_fDOM`)
#MOOS_storm9_10_02_fDOM_FI= FI_diff(MOOS_storm9_10_02_Q, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm9_10_02_fDOM`)

#SPC #
MOOS_storm1_06_01_SPC_FI = FI_diff(MOOS_storm1_06_01_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm1_06_01_SPC`)
#MOOS_storm3_07_12_SPC_FI = FI_diff(MOOS_storm3_07_12_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm3_07_12_SPC`)
#MOOS_storm4_07_25_SPC_FI= FI_diff(MOOS_storm4_07_25_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm4_07_25_SPC`)
#MOOS_storm5_07_29_SPC_FI= FI_diff(MOOS_storm5_07_29_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm5_07_29_SPC`)
MOOS_storm6a_08_01_SPC_FI= FI_diff(MOOS_storm6a_08_01_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6a_08_01_SPC`)
#MOOS_storm6b_08_02_SPC_FI= FI_diff(MOOS_storm6b_08_02_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6b_08_02_SPC`)
MOOS_storm6c_08_03_SPC_FI= FI_diff(MOOS_storm6c_08_03_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6c_08_03_SPC`)
MOOS_storm7a_08_13_SPC_FI= FI_diff(MOOS_storm7a_08_13_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7a_08_13_SPC`)
MOOS_storm7b_08_14_SPC_FI= FI_diff(MOOS_storm7b_08_14_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7b_08_14_SPC`)
MOOS_storm7c_08_15_SPC_FI= FI_diff(MOOS_storm7c_08_15_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7c_08_15_SPC`)
#MOOS_storm8_09_21_SPC_FI= FI_diff(MOOS_storm8_09_21_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm8_09_21_SPC`)
#MOOS_storm9_10_02_SPC_FI= FI_diff(MOOS_storm9_10_02_Q, MOOS_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm9_10_02_SPC`)

# turb #
MOOS_storm1_06_01_turb_FI = FI_diff(MOOS_storm1_06_01_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm1_06_01_Turb`)
#MOOS_storm3_07_12_turb_FI = FI_diff(MOOS_storm3_07_12_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm3_07_12_Turb`) # empty 
#MOOS_storm4_07_25_turb_FI= FI_diff(MOOS_storm4_07_25_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm4_07_25_Turb`)
#MOOS_storm5_07_29_turb_FI= FI_diff(MOOS_storm5_07_29_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm5_07_29_Turb`)
MOOS_storm6a_08_01_turb_FI= FI_diff(MOOS_storm6a_08_01_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6a_08_01_Turb`)
#MOOS_storm6b_08_02_turb_FI= FI_diff(MOOS_storm6b_08_02_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6b_08_02_Turb`)
MOOS_storm6c_08_03_turb_FI= FI_diff(MOOS_storm6c_08_03_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm6c_08_03_Turb`)
MOOS_storm7a_08_13_turb_FI= FI_diff(MOOS_storm7a_08_13_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7a_08_13_Turb`)
MOOS_storm7b_08_14_turb_FI= FI_diff(MOOS_storm7b_08_14_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7b_08_14_Turb`)
MOOS_storm7c_08_15_turb_FI= FI_diff(MOOS_storm7c_08_15_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm7c_08_15_Turb`)
#MOOS_storm8_09_21_turb_FI= FI_diff(MOOS_storm8_09_21_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm8_09_21_Turb`)
#MOOS_storm9_10_02_turb_FI= FI_diff(MOOS_storm9_10_02_Q, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//MOOS_storm9_10_02_Turb`)

# POKE # 
#NO3 #
POKE_storm1_06_30_NO3_FI = FI_diff(POKE_storm1_06_30_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm1_06_30_NO3`)
POKE_storm2_07_12_NO3_FI = FI_diff(POKE_storm2_07_12_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm2_07_12_NO3`) 
POKE_storm3_07_26_NO3_FI = FI_diff(POKE_storm3_07_26_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm3_07_26_NO3`) # all values are the same 0.5564
POKE_storm4_07_31_NO3_FI = FI_diff(POKE_storm4_07_31_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm4_07_31_NO3`)# all values are the same  0.60909
POKE_storm5a_08_02_NO3_FI = FI_diff(POKE_storm5a_08_02_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5a_08_02_NO3`)# all values are the same  0.357142
POKE_storm5b_08_03_NO3_FI = FI_diff(POKE_storm5b_08_03_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5b_08_03_NO3`)# all values are the same  0.1878755
#POKE_storm5c_08_05_NO3_FI = FI_diff(POKE_storm5c_08_05_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5c_08_05_NO3`) # didnt work 
POKE_storm5d_08_10_NO3_FI = FI_diff(POKE_storm5d_08_10_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5d_08_10_NO3`)# all values are the same 0.06321839
#POKE_storm6a_08_12_NO3_FI = FI_diff(POKE_storm6a_08_12_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6a_08_12_NO3`) # didnt work 
#POKE_storm6b_08_13_NO3_FI = FI_diff(POKE_storm6b_08_13_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6b_08_13_NO3`) # didnt work 
#POKE_storm7_08_15_NO3_FI = FI_diff(POKE_storm7_08_15_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm7_08_15_NO3`) # didnt work 
POKE_storm8_09_29_NO3_FI = FI_diff(POKE_storm8_09_29_Q, POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm8_09_29_NO3`)
#POKE_storm9_10_04_NO3_FI = FI_diff(POKE_storm9_10_04_Q,POKE_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm9_10_04_NO3`) # didnt work 

#fDOM #
POKE_storm1_06_30_fDOM_FI = FI_diff(POKE_storm1_06_30_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm1_06_30_fDOM`)
POKE_storm2_07_12_fDOM_FI = FI_diff(POKE_storm2_07_12_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm2_07_12_fDOM`) 
POKE_storm3_07_26_fDOM_FI = FI_diff(POKE_storm3_07_26_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm3_07_26_fDOM`) 
POKE_storm4_07_31_fDOM_FI = FI_diff(POKE_storm4_07_31_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm4_07_31_fDOM`)
POKE_storm5a_08_02_fDOM_FI = FI_diff(POKE_storm5a_08_02_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5a_08_02_fDOM`)
POKE_storm5b_08_03_fDOM_FI = FI_diff(POKE_storm5b_08_03_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5b_08_03_fDOM`)
POKE_storm5c_08_05_fDOM_FI = FI_diff(POKE_storm5c_08_05_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5c_08_05_fDOM`) 
POKE_storm5d_08_10_fDOM_FI = FI_diff(POKE_storm5d_08_10_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5d_08_10_fDOM`)
POKE_storm6a_08_12_fDOM_FI = FI_diff(POKE_storm6a_08_12_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6a_08_12_fDOM`) 
#POKE_storm6b_08_13_fDOM_FI = FI_diff(POKE_storm6b_08_13_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6b_08_13_fDOM`) # didnt work 
POKE_storm7_08_15_fDOM_FI = FI_diff(POKE_storm7_08_15_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm7_08_15_fDOM`)
POKE_storm8_09_29_fDOM_FI = FI_diff(POKE_storm8_09_29_Q, POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm8_09_29_fDOM`)
#POKE_storm9_10_04_fDOM_FI = FI_diff(POKE_storm9_10_04_Q,POKE_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm9_10_04_fDOM`) # didnt work 

#SPC #
POKE_storm1_06_30_SPC_FI = FI_diff(POKE_storm1_06_30_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm1_06_30_SPC`)
POKE_storm2_07_12_SPC_FI = FI_diff(POKE_storm2_07_12_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm2_07_12_SPC`) 
POKE_storm3_07_26_SPC_FI = FI_diff(POKE_storm3_07_26_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm3_07_26_SPC`) 
POKE_storm4_07_31_SPC_FI = FI_diff(POKE_storm4_07_31_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm4_07_31_SPC`)
POKE_storm5a_08_02_SPC_FI = FI_diff(POKE_storm5a_08_02_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5a_08_02_SPC`)
POKE_storm5b_08_03_SPC_FI = FI_diff(POKE_storm5b_08_03_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5b_08_03_SPC`)
POKE_storm5c_08_05_SPC_FI = FI_diff(POKE_storm5c_08_05_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5c_08_05_SPC`) 
POKE_storm5d_08_10_SPC_FI = FI_diff(POKE_storm5d_08_10_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5d_08_10_SPC`)
POKE_storm6a_08_12_SPC_FI = FI_diff(POKE_storm6a_08_12_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6a_08_12_SPC`) 
#POKE_storm6b_08_13_SPC_FI = FI_diff(POKE_storm6b_08_13_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6b_08_13_SPC`) # didnt work 
POKE_storm7_08_15_SPC_FI = FI_diff(POKE_storm7_08_15_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm7_08_15_SPC`)
POKE_storm8_09_29_SPC_FI = FI_diff(POKE_storm8_09_29_Q, POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm8_09_29_SPC`)
#POKE_storm9_10_04_SPC_FI = FI_diff(POKE_storm9_10_04_Q,POKE_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm9_10_04_SPC`) # didnt work 

#turb #
POKE_storm1_06_30_turb_FI = FI_diff(POKE_storm1_06_30_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm1_06_30_Turb`)
POKE_storm2_07_12_turb_FI = FI_diff(POKE_storm2_07_12_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm2_07_12_Turb`) 
POKE_storm3_07_26_turb_FI = FI_diff(POKE_storm3_07_26_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm3_07_26_Turb`) 
POKE_storm4_07_31_turb_FI = FI_diff(POKE_storm4_07_31_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm4_07_31_Turb`)
POKE_storm5a_08_02_turb_FI = FI_diff(POKE_storm5a_08_02_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5a_08_02_Turb`)
POKE_storm5b_08_03_turb_FI = FI_diff(POKE_storm5b_08_03_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5b_08_03_Turb`)
POKE_storm5c_08_05_turb_FI = FI_diff(POKE_storm5c_08_05_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5c_08_05_Turb`) 
POKE_storm5d_08_10_turb_FI = FI_diff(POKE_storm5d_08_10_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm5d_08_10_Turb`)
POKE_storm6a_08_12_turb_FI = FI_diff(POKE_storm6a_08_12_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6a_08_12_Turb`) 
#POKE_storm6b_08_13_turb_FI = FI_diff(POKE_storm6b_08_13_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm6b_08_13_Turb`) # didnt work 
POKE_storm7_08_15_turb_FI = FI_diff(POKE_storm7_08_15_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm7_08_15_Turb`)
POKE_storm8_09_29_turb_FI = FI_diff(POKE_storm8_09_29_Q, POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm8_09_29_Turb`)
#POKE_storm9_10_04_turb_FI = FI_diff(POKE_storm9_10_04_Q,POKE_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//POKE_storm9_10_04_Turb`) # didnt work 

# STRT # 
#NO3 #
STRT_storm1_05_31_NO3_FI = FI_diff(STRT_storm1_05_31_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm1_05_31_NO3`)
STRT_storm2_07_12_NO3_FI = FI_diff(STRT_storm2_07_12_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm2_07_12_NO3`)
#STRT_storm3a_07_25_NO3_FI = FI_diff(STRT_storm3a_07_25_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3a_07_25_NO3`) # didnt work 
#STRT_storm3b_08_05_NO3_FI = FI_diff(STRT_storm3b_08_05_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3b_08_05_NO3`) # didnt work 
STRT_storm3c_08_12_NO3_FI = FI_diff(STRT_storm3c_08_12_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3c_08_12_NO3`)
#STRT_storm4_08_15_NO3_FI = FI_diff(STRT_storm4_08_15_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm4_08_15_NO3`) # didnt work 
#STRT_storm5_08_20_NO3_FI = FI_diff(STRT_storm5_08_20_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm5_08_20_NO3`) # didnt work 
STRT_storm6_09_20_NO3_FI = FI_diff(STRT_storm6_09_20_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm6_09_20_NO3`)
STRT_storm7_10_01_NO3_FI = FI_diff(STRT_storm7_10_01_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7_10_01_NO3`)
STRT_storm7b_10_04_NO3_FI = FI_diff(STRT_storm7b_10_04_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7b_10_04_NO3`)
STRT_storm7c_10_09_NO3_FI = FI_diff(STRT_storm7c_10_09_Q, STRT_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7c_10_09_NO3`)


# fDOM #
#STRT_storm1_05_31_fDOM_FI = FI_diff(STRT_storm1_05_31_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm1_05_31_fDOM`) # didnt work 
STRT_storm2_07_12_fDOM_FI = FI_diff(STRT_storm2_07_12_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm2_07_12_fDOM`)
STRT_storm3a_07_25_fDOM_FI = FI_diff(STRT_storm3a_07_25_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3a_07_25_fDOM`)
STRT_storm3b_08_05_fDOM_FI = FI_diff(STRT_storm3b_08_05_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3b_08_05_fDOM`)
STRT_storm3c_08_12_fDOM_FI = FI_diff(STRT_storm3c_08_12_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3c_08_12_fDOM`)
STRT_storm4_08_15_fDOM_FI = FI_diff(STRT_storm4_08_15_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm4_08_15_fDOM`)
STRT_storm5_08_20_fDOM_FI = FI_diff(STRT_storm5_08_20_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm5_08_20_fDOM`)
#STRT_storm6_09_20_fDOM_FI = FI_diff(STRT_storm6_09_20_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm6_09_20_fDOM`) # didnt work 
#STRT_storm7_10_01_fDOM_FI = FI_diff(STRT_storm7_10_01_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7_10_01_fDOM`) # didnt work 
STRT_storm7b_10_04_fDOM_FI = FI_diff(STRT_storm7b_10_04_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7b_10_04_fDOM`)
#STRT_storm7c_10_09_fDOM_FI = FI_diff(STRT_storm7c_10_09_Q, STRT_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7c_10_09_fDOM`) # didnt work 

# SPC #
#STRT_storm1_05_31_SPC_FI = FI_diff(STRT_storm1_05_31_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm1_05_31_SPC`) # didnt work 
STRT_storm2_07_12_SPC_FI = FI_diff(STRT_storm2_07_12_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm2_07_12_SPC`)
STRT_storm3a_07_25_SPC_FI = FI_diff(STRT_storm3a_07_25_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3a_07_25_SPC`)
STRT_storm3b_08_05_SPC_FI = FI_diff(STRT_storm3b_08_05_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3b_08_05_SPC`)
STRT_storm3c_08_12_SPC_FI = FI_diff(STRT_storm3c_08_12_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3c_08_12_SPC`)
STRT_storm4_08_15_SPC_FI = FI_diff(STRT_storm4_08_15_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm4_08_15_SPC`)
STRT_storm5_08_20_SPC_FI = FI_diff(STRT_storm5_08_20_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm5_08_20_SPC`)
#STRT_storm6_09_20_SPC_FI = FI_diff(STRT_storm6_09_20_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm6_09_20_SPC`) # didnt work 
#STRT_storm7_10_01_SPC_FI = FI_diff(STRT_storm7_10_01_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7_10_01_SPC`) # didnt work 
STRT_storm7b_10_04_SPC_FI = FI_diff(STRT_storm7b_10_04_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7b_10_04_SPC`)
#STRT_storm7c_10_09_SPC_FI = FI_diff(STRT_storm7c_10_09_Q, STRT_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7c_10_09_SPC`) # didnt work 

# Turb #
#STRT_storm1_05_31_turb_FI = FI_diff(STRT_storm1_05_31_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm1_05_31_Turb`) # didnt work 
STRT_storm2_07_12_turb_FI = FI_diff(STRT_storm2_07_12_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm2_07_12_Turb`)
STRT_storm3a_07_25_turb_FI = FI_diff(STRT_storm3a_07_25_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3a_07_25_Turb`)
STRT_storm3b_08_05_turb_FI = FI_diff(STRT_storm3b_08_05_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3b_08_05_Turb`)
STRT_storm3c_08_12_turb_FI = FI_diff(STRT_storm3c_08_12_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm3c_08_12_Turb`)
STRT_storm4_08_15_turb_FI = FI_diff(STRT_storm4_08_15_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm4_08_15_Turb`)
STRT_storm5_08_20_turb_FI = FI_diff(STRT_storm5_08_20_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm5_08_20_Turb`)
#STRT_storm6_09_20_turb_FI = FI_diff(STRT_storm6_09_20_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm6_09_20_Turb`) # didnt work 
#STRT_storm7_10_01_turb_FI = FI_diff(STRT_storm7_10_01_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7_10_01_Turb`) # didnt work 
STRT_storm7b_10_04_turb_FI = FI_diff(STRT_storm7b_10_04_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7b_10_04_Turb`)
#STRT_storm7c_10_09_turb_FI = FI_diff(STRT_storm7c_10_09_Q, STRT_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//STRT_storm7c_10_09_Turb`) # didnt work 

# VAUL # 
#NO3 #
VAUL_storm1_07_13_NO3_FI = FI_diff(VAUL_storm1_07_13_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm1_07_13_NO3`)
VAUL_storm2_07_26_NO3_FI = FI_diff(VAUL_storm2_07_26_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm2_07_26_NO3`)
VAUL_storm3_07_29_NO3_FI = FI_diff(VAUL_storm3_07_29_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm3_07_29_NO3`)
VAUL_storm4a_08_02_NO3_FI = FI_diff(VAUL_storm4a_08_02_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4a_08_02_NO3`)
VAUL_storm4b_08_03_NO3_FI = FI_diff(VAUL_storm4b_08_03_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4b_08_03_NO3`)
VAUL_storm4c_08_05_NO3_FI = FI_diff(VAUL_storm4c_08_05_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4c_08_05_NO3`)
VAUL_storm5_08_12_NO3_FI = FI_diff(VAUL_storm5_08_12_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm5_08_12_NO3`)
VAUL_storm6_08_15_NO3_FI = FI_diff(VAUL_storm6_08_15_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm6_08_15_NO3`)
VAUL_storm7_09_19_NO3_FI = FI_diff(VAUL_storm7_09_19_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm7_09_19_NO3`)
VAUL_storm8a_09_29_NO3_FI = FI_diff(VAUL_storm8a_09_29_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8a_09_29_NO3`)
#VAUL_storm8b_10_01_NO3_FI = FI_diff(VAUL_storm8b_10_01_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8b_10_01_NO3`) # didnt work 
#VAUL_storm8c_10_04_NO3_FI = FI_diff(VAUL_storm8c_10_04_Q, VAUL_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8c_10_04_NO3`) # didnt work 

#fDOM #
VAUL_storm1_07_13_fDOM_FI = FI_diff(VAUL_storm1_07_13_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm1_07_13_fDOM`)
VAUL_storm2_07_26_fDOM_FI = FI_diff(VAUL_storm2_07_26_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm2_07_26_fDOM`)
VAUL_storm3_07_29_fDOM_FI = FI_diff(VAUL_storm3_07_29_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm3_07_29_fDOM`)
VAUL_storm4a_08_02_fDOM_FI = FI_diff(VAUL_storm4a_08_02_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4a_08_02_fDOM`)
VAUL_storm4b_08_03_fDOM_FI = FI_diff(VAUL_storm4b_08_03_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4b_08_03_fDOM`)
VAUL_storm4c_08_05_fDOM_FI = FI_diff(VAUL_storm4c_08_05_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4c_08_05_fDOM`)
#VAUL_storm5_08_12_fDOM_FI = FI_diff(VAUL_storm5_08_12_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm5_08_12_fDOM`)# didnt work 
#VAUL_storm6_08_15_fDOM_FI = FI_diff(VAUL_storm6_08_15_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm6_08_15_fDOM`)# didnt work 
#VAUL_storm7_09_19_fDOM_FI = FI_diff(VAUL_storm7_09_19_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm7_09_19_fDOM`)# didnt work 
#VAUL_storm8a_09_29_fDOM_FI = FI_diff(VAUL_storm8a_09_29_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8a_09_29_fDOM`)# didnt work 
#VAUL_storm8b_10_01_fDOM_FI = FI_diff(VAUL_storm8b_10_01_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8b_10_01_fDOM`) # didnt work 
#VAUL_storm8c_10_04_fDOM_FI = FI_diff(VAUL_storm8c_10_04_Q, VAUL_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8c_10_04_fDOM`) # didnt work 

#SPC #
VAUL_storm1_07_13_SPC_FI = FI_diff(VAUL_storm1_07_13_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm1_07_13_SPC`)
VAUL_storm2_07_26_SPC_FI = FI_diff(VAUL_storm2_07_26_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm2_07_26_SPC`)
VAUL_storm3_07_29_SPC_FI = FI_diff(VAUL_storm3_07_29_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm3_07_29_SPC`)
VAUL_storm4a_08_02_SPC_FI = FI_diff(VAUL_storm4a_08_02_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4a_08_02_SPC`)
VAUL_storm4b_08_03_SPC_FI = FI_diff(VAUL_storm4b_08_03_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4b_08_03_SPC`)
VAUL_storm4c_08_05_SPC_FI = FI_diff(VAUL_storm4c_08_05_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4c_08_05_SPC`)
#VAUL_storm5_08_12_SPC_FI = FI_diff(VAUL_storm5_08_12_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm5_08_12_SPC`)# didnt work 
#VAUL_storm6_08_15_SPC_FI = FI_diff(VAUL_storm6_08_15_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm6_08_15_SPC`)# didnt work 
#VAUL_storm7_09_19_SPC_FI = FI_diff(VAUL_storm7_09_19_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm7_09_19_SPC`)# didnt work 
#VAUL_storm8a_09_29_SPC_FI = FI_diff(VAUL_storm8a_09_29_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8a_09_29_SPC`)# didnt work 
#VAUL_storm8b_10_01_SPC_FI = FI_diff(VAUL_storm8b_10_01_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8b_10_01_SPC`) # didnt work 
#VAUL_storm8c_10_04_SPC_FI = FI_diff(VAUL_storm8c_10_04_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8c_10_04_SPC`) # didnt work 

#turb #
VAUL_storm1_07_13_turb_FI = FI_diff(VAUL_storm1_07_13_Q, VAUL_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm1_07_13_turb`)
VAUL_storm2_07_26_turb_FI = FI_diff(VAUL_storm2_07_26_Q, VAUL_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm2_07_26_turb`)
VAUL_storm3_07_29_turb_FI = FI_diff(VAUL_storm3_07_29_Q, VAUL_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm3_07_29_turb`)
VAUL_storm4a_08_02_turb_FI = FI_diff(VAUL_storm4a_08_02_Q, VAUL_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4a_08_02_turb`)
VAUL_storm4b_08_03_turb_FI = FI_diff(VAUL_storm4b_08_03_Q, VAUL_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4b_08_03_turb`)
VAUL_storm4c_08_05_turb_FI = FI_diff(VAUL_storm4c_08_05_Q, VAUL_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm4c_08_05_turb`)
#VAUL_storm5_08_12_SPC_FI = FI_diff(VAUL_storm5_08_12_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm5_08_12_SPC`)# didnt work 
#VAUL_storm6_08_15_SPC_FI = FI_diff(VAUL_storm6_08_15_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm6_08_15_SPC`)# didnt work 
#VAUL_storm7_09_19_SPC_FI = FI_diff(VAUL_storm7_09_19_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm7_09_19_SPC`)# didnt work 
#VAUL_storm8a_09_29_SPC_FI = FI_diff(VAUL_storm8a_09_29_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8a_09_29_SPC`)# didnt work 
#VAUL_storm8b_10_01_SPC_FI = FI_diff(VAUL_storm8b_10_01_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8b_10_01_SPC`) # didnt work 
#VAUL_storm8c_10_04_SPC_FI = FI_diff(VAUL_storm8c_10_04_Q, VAUL_SPC_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//VAUL_storm8c_10_04_SPC`) # didnt work 

# gather results and save ##
FRCH_storm1_05_31_NO3_FI[[2]]
FRCH_storm1_05_31_NO3_test[[2]]
STRT_storm2_07_12_turb_FI[[2]]

FI_results = rbind(
  c("FRCH_storm1_05_31_NO3_FI",FRCH_storm1_05_31_NO3_FI[[2]]),
  c("FRCH_storm2_06_15_NO3_FI",FRCH_storm2_06_15_NO3_FI[[2]]),
  c("FRCH_storm3_06_18_NO3_FI",FRCH_storm3_06_18_NO3_FI[[2]]),
  c("FRCH_storm4_06_20_NO3_FI",FRCH_storm4_06_20_NO3_FI[[2]]),
  c("FRCH_storm5_06_22_NO3_FI",FRCH_storm5_06_22_NO3_FI[[2]]),
  c("FRCH_storm6_07_12_NO3_FI",FRCH_storm6_07_12_NO3_FI[[2]]),
  c("FRCH_storm7_07_25_NO3_FI",FRCH_storm7_07_25_NO3_FI[[2]]),
  c("FRCH_storm8_07_28_NO3_FI",NA, NA, NA),
  c("FRCH_storm9a_07_29_NO3_FI",FRCH_storm9a_07_29_NO3_FI[[2]]),
  c("FRCH_storm9b_07_30_NO3_FI",FRCH_storm9b_07_30_NO3_FI[[2]]),
  c("FRCH_storm10a_08_01_NO3_FI",FRCH_storm10a_08_01_NO3_FI[[2]]),
  c("FRCH_storm10b_08_02_NO3_FI",FRCH_storm10b_08_02_NO3_FI[[2]]),
  c("FRCH_storm11_08_05_NO3_FI",FRCH_storm11_08_05_NO3_FI[[2]]),
  c("FRCH_storm12a_08_12_NO3_FI",FRCH_storm12a_08_12_NO3_FI[[2]]),
  c("FRCH_storm12b_08_14_NO3_FI",FRCH_storm12b_08_14_NO3_FI[[2]]),
  c("FRCH_storm12c_08_15_NO3_FI",FRCH_storm12c_08_15_NO3_FI[[2]]),
  c("FRCH_storm12d_08_21_NO3_FI",FRCH_storm12d_08_21_NO3_FI[[2]]),
  c("FRCH_storm12e_08_23_NO3_FI",FRCH_storm12e_08_23_NO3_FI[[2]]),
  c("FRCH_storm13_09_20_NO3_FI",FRCH_storm13_09_20_NO3_FI[[2]]),
  c("FRCH_storm14_10_01_NO3_FI",FRCH_storm14_10_01_NO3_FI[[2]]),
  
  c("FRCH_storm1_05_31_fDOM_FI",FRCH_storm1_05_31_fDOM_FI[[2]]),
  c("FRCH_storm2_06_15_fDOM_FI",FRCH_storm2_06_15_fDOM_FI[[2]]),
  c("FRCH_storm3_06_18_fDOM_FI",FRCH_storm3_06_18_fDOM_FI[[2]]),
  c("FRCH_storm4_06_20_fDOM_FI",FRCH_storm4_06_20_fDOM_FI[[2]]),
  c("FRCH_storm5_06_22_fDOM_FI",FRCH_storm5_06_22_fDOM_FI[[2]]),
  c("FRCH_storm6_07_12_fDOM_FI", NA, NA, NA),
  c("FRCH_storm7_07_25_fDOM_FI",FRCH_storm7_07_25_fDOM_FI[[2]]),
  c("FRCH_storm8_07_28_fDOM_FI", NA, NA, NA),
  c("FRCH_storm9a_07_29_fDOM_FI",FRCH_storm9a_07_29_fDOM_FI[[2]]),
  c("FRCH_storm9b_07_30_fDOM_FI",FRCH_storm9b_07_30_fDOM_FI[[2]]),
  c("FRCH_storm10a_08_01_fDOM_FI",FRCH_storm10a_08_01_fDOM_FI[[2]]),
  c("FRCH_storm10b_08_02_fDOM_FI",FRCH_storm10b_08_02_fDOM_FI[[2]]),
  c("FRCH_storm11_08_05_fDOM_FI",FRCH_storm11_08_05_fDOM_FI[[2]]),
  c("FRCH_storm12a_08_12_fDOM_FI",FRCH_storm12a_08_12_fDOM_FI[[2]]),
  c("FRCH_storm12b_08_14_fDOM_FI",FRCH_storm12b_08_14_fDOM_FI[[2]]),
  c("FRCH_storm12c_08_15_fDOM_FI",FRCH_storm12c_08_15_fDOM_FI[[2]]),
  c("FRCH_storm12d_08_21_fDOM_FI",FRCH_storm12d_08_21_fDOM_FI[[2]]),
  c("FRCH_storm12e_08_23_fDOM_FI",FRCH_storm12e_08_23_fDOM_FI[[2]]),
  c("FRCH_storm13_09_20_fDOM_FI",NA, NA, NA),
  c("FRCH_storm14_10_01_NO3_FI",FRCH_storm14_10_01_NO3_FI[[2]]),
  
  c("FRCH_storm1_05_31_SPC_FI",FRCH_storm1_05_31_SPC_FI[[2]]),
  c("FRCH_storm2_06_15_SPC_FI",FRCH_storm2_06_15_SPC_FI[[2]]),
  c("FRCH_storm3_06_18_SPC_FI",FRCH_storm3_06_18_SPC_FI[[2]]),
  c("FRCH_storm4_06_20_SPC_FI",FRCH_storm4_06_20_SPC_FI[[2]]),
  c("FRCH_storm5_06_22_SPC_FI",FRCH_storm5_06_22_SPC_FI[[2]]),
  c("FRCH_storm6_07_12_SPC_FI", NA, NA, NA),
  c("FRCH_storm7_07_25_SPC_FI",FRCH_storm7_07_25_SPC_FI[[2]]),
  c("FRCH_storm8_07_28_SPC_FI", NA, NA, NA),
  c("FRCH_storm9a_07_29_SPC_FI",FRCH_storm9a_07_29_SPC_FI[[2]]),
  c("FRCH_storm9b_07_30_SPC_FI",FRCH_storm9b_07_30_SPC_FI[[2]]),
  c("FRCH_storm10a_08_01_SPC_FI",FRCH_storm10a_08_01_SPC_FI[[2]]),
  c("FRCH_storm10b_08_02_SPC_FI",FRCH_storm10b_08_02_SPC_FI[[2]]),
  c("FRCH_storm11_08_05_SPC_FI",FRCH_storm11_08_05_SPC_FI[[2]]),
  c("FRCH_storm12a_08_12_SPC_FI",FRCH_storm12a_08_12_SPC_FI[[2]]),
  c("FRCH_storm12b_08_14_SPC_FI",FRCH_storm12b_08_14_SPC_FI[[2]]),
  c("FRCH_storm12c_08_15_SPC_FI",FRCH_storm12c_08_15_SPC_FI[[2]]),
  c("FRCH_storm12d_08_21_SPC_FI",FRCH_storm12d_08_21_SPC_FI[[2]]),
  c("FRCH_storm12e_08_23_SPC_FI",FRCH_storm12e_08_23_SPC_FI[[2]]),
  c("FRCH_storm13_09_20_SPC_FI",NA, NA, NA),
  c("FRCH_storm14_10_01_NO3_FI",FRCH_storm14_10_01_NO3_FI[[2]]),
  
  c("FRCH_storm1_05_31_turb_FI",FRCH_storm1_05_31_turb_FI[[2]]),
  c("FRCH_storm2_06_15_turb_FI",FRCH_storm2_06_15_turb_FI[[2]]),
  c("FRCH_storm3_06_18_turb_FI",FRCH_storm3_06_18_turb_FI[[2]]),
  c("FRCH_storm4_06_20_turb_FI",FRCH_storm4_06_20_turb_FI[[2]]),
  c("FRCH_storm5_06_22_turb_FI",FRCH_storm5_06_22_turb_FI[[2]]),
  c("FRCH_storm6_07_12_turb_FI", NA, NA, NA),
  c("FRCH_storm7_07_25_turb_FI",FRCH_storm7_07_25_turb_FI[[2]]),
  c("FRCH_storm8_07_28_turb_FI", NA, NA, NA),
  c("FRCH_storm9a_07_29_turb_FI",FRCH_storm9a_07_29_turb_FI[[2]]),
  c("FRCH_storm9b_07_30_turb_FI",FRCH_storm9b_07_30_turb_FI[[2]]),
  c("FRCH_storm10a_08_01_turb_FI",FRCH_storm10a_08_01_turb_FI[[2]]),
  c("FRCH_storm10b_08_02_turb_FI",FRCH_storm10b_08_02_turb_FI[[2]]),
  c("FRCH_storm11_08_05_turb_FI",FRCH_storm11_08_05_turb_FI[[2]]),
  c("FRCH_storm12a_08_12_turb_FI",FRCH_storm12a_08_12_turb_FI[[2]]),
  c("FRCH_storm12b_08_14_turb_FI",FRCH_storm12b_08_14_turb_FI[[2]]),
  c("FRCH_storm12c_08_15_turb_FI",FRCH_storm12c_08_15_turb_FI[[2]]),
  c("FRCH_storm12d_08_21_turb_FI",FRCH_storm12d_08_21_turb_FI[[2]]),
  c("FRCH_storm12e_08_23_turb_FI",FRCH_storm12e_08_23_turb_FI[[2]]),
  c("FRCH_storm13_09_20_turb_FI",NA, NA, NA),
  c("FRCH_storm14_10_01_NO3_FI",FRCH_storm14_10_01_NO3_FI[[2]]),
  
  c("MOOS_storm1_06_01_NO3_FI",MOOS_storm1_06_01_NO3_FI[[2]]),
  c("MOOS_storm3_07_12_NO3_FI",MOOS_storm3_07_12_NO3_FI[[2]]),
  c("MOOS_storm4_07_25_NO3_FI",NA, NA, NA),
  c("MOOS_storm5_07_29_NO3_FI",MOOS_storm5_07_29_NO3_FI[[2]]),
  c("MOOS_storm6a_08_01_NO3_FI",MOOS_storm6a_08_01_NO3_FI[[2]]),
  c("MOOS_storm6b_08_02_NO3_FI",NA, NA, NA),
  c("MOOS_storm6c_08_03_NO3_FI",NA, NA, NA),
  c("MOOS_storm7a_08_13_NO3_FI",MOOS_storm7a_08_13_NO3_FI[[2]]),
  c("MOOS_storm7b_08_14_NO3_FI",MOOS_storm7b_08_14_NO3_FI[[2]]),
  c("MOOS_storm7c_08_15_NO3_FI",MOOS_storm7c_08_15_NO3_FI[[2]]),
  c("MOOS_storm8_09_21_NO3_FI",MOOS_storm8_09_21_NO3_FI[[2]]),
  c("MOOS_storm9_10_02_NO3_FI",MOOS_storm9_10_02_NO3_FI[[2]]),
  
  c("MOOS_storm1_06_01_fDOM_FI",MOOS_storm1_06_01_fDOM_FI[[2]]),
  c("MOOS_storm3_07_12_fDOM_FI",NA, NA, NA),
  c("MOOS_storm4_07_25_fDOM_FI",NA, NA, NA),
  c("MOOS_storm5_07_29_fDOM_FI",NA, NA, NA),
  c("MOOS_storm6a_08_01_fDOM_FI",MOOS_storm6a_08_01_fDOM_FI[[2]]),
  c("MOOS_storm6b_08_02_fDOM_FI",NA, NA, NA),
  c("MOOS_storm6c_08_03_fDOM_FI",MOOS_storm6c_08_03_fDOM_FI[[2]]),
  c("MOOS_storm7a_08_13_fDOM_FI",MOOS_storm7a_08_13_fDOM_FI[[2]]),
  c("MOOS_storm7b_08_14_fDOM_FI",MOOS_storm7b_08_14_fDOM_FI[[2]]),
  c("MOOS_storm7c_08_15_fDOM_FI",MOOS_storm7c_08_15_fDOM_FI[[2]]),
  c("MOOS_storm8_09_21_fDOM_FI",NA, NA, NA),
  c("MOOS_storm9_10_02_fDOM_FI",NA, NA, NA),
  
  c("MOOS_storm1_06_01_SPC_FI",MOOS_storm1_06_01_SPC_FI[[2]]),
  c("MOOS_storm3_07_12_SPC_FI",NA, NA, NA),
  c("MOOS_storm4_07_25_SPC_FI",NA, NA, NA),
  c("MOOS_storm5_07_29_SPC_FI",NA, NA, NA),
  c("MOOS_storm6a_08_01_SPC_FI",MOOS_storm6a_08_01_SPC_FI[[2]]),
  c("MOOS_storm6b_08_02_SPC_FI",NA, NA, NA),
  c("MOOS_storm6c_08_03_SPC_FI",MOOS_storm6c_08_03_SPC_FI[[2]]),
  c("MOOS_storm7a_08_13_SPC_FI",MOOS_storm7a_08_13_SPC_FI[[2]]),
  c("MOOS_storm7b_08_14_SPC_FI",MOOS_storm7b_08_14_SPC_FI[[2]]),
  c("MOOS_storm7c_08_15_SPC_FI",MOOS_storm7c_08_15_SPC_FI[[2]]),
  c("MOOS_storm8_09_21_SPC_FI",NA, NA, NA),
  c("MOOS_storm9_10_02_SPC_FI",NA, NA, NA),
  
  c("MOOS_storm1_06_01_turb_FI",MOOS_storm1_06_01_turb_FI[[2]]),
  c("MOOS_storm3_07_12_turb_FI",NA, NA, NA),
  c("MOOS_storm4_07_25_turb_FI",NA, NA, NA),
  c("MOOS_storm5_07_29_turb_FI",NA, NA, NA),
  c("MOOS_storm6a_08_01_turb_FI",MOOS_storm6a_08_01_turb_FI[[2]]),
  c("MOOS_storm6b_08_02_turb_FI",NA, NA, NA),
  c("MOOS_storm6c_08_03_turb_FI",MOOS_storm6c_08_03_turb_FI[[2]]),
  c("MOOS_storm7a_08_13_turb_FI",MOOS_storm7a_08_13_turb_FI[[2]]),
  c("MOOS_storm7b_08_14_turb_FI",MOOS_storm7b_08_14_turb_FI[[2]]),
  c("MOOS_storm7c_08_15_turb_FI",MOOS_storm7c_08_15_turb_FI[[2]]),
  c("MOOS_storm8_09_21_turb_FI",NA, NA, NA),
  c("MOOS_storm9_10_02_turb_FI",NA, NA, NA),
  
  c("POKE_storm1_06_30_NO3_FI",POKE_storm1_06_30_NO3_FI[[2]]),
  c("POKE_storm2_07_12_NO3_FI",POKE_storm2_07_12_NO3_FI[[2]]),
  c("POKE_storm3_07_26_NO3_FI",0.556451612903223, 0, 0),
  c("POKE_storm4_07_31_NO3_FI",0.609090909090907, 0, 0),
  c("POKE_storm5a_08_02_NO3_FI",0.357142857142857, 0, 0),
  c("POKE_storm5b_08_03_NO3_FI",0.187875574407918, 0 ,0),
  c("POKE_storm5c_08_05_NO3_FI",NA, NA, NA),
  c("POKE_storm5d_08_10_NO3_FI",0.0632183908045973, 0, 0),
  c("POKE_storm6a_08_12_NO3_FI",NA, NA, NA),
  c("POKE_storm6b_08_13_NO3_FI",NA, NA, NA),
  c("POKE_storm7_08_15_NO3_FI",NA, NA, NA),
  c("POKE_storm8_09_29_NO3_FI",POKE_storm8_09_29_NO3_FI[[2]]),
  c("POKE_storm9_10_04_NO3_FI",NA, NA, NA),
  
  c("POKE_storm1_06_30_fDOM_FI",POKE_storm1_06_30_fDOM_FI[[2]]),
  c("POKE_storm2_07_12_fDOM_FI",POKE_storm2_07_12_fDOM_FI[[2]]),
  c("POKE_storm3_07_26_fDOM_FI",POKE_storm3_07_26_fDOM_FI[[2]]),
  c("POKE_storm4_07_31_fDOM_FI",POKE_storm4_07_31_fDOM_FI[[2]]),
  c("POKE_storm5a_08_02_fDOM_FI",POKE_storm5a_08_02_fDOM_FI[[2]]),
  c("POKE_storm5b_08_03_fDOM_FI",POKE_storm5b_08_03_fDOM_FI[[2]]),
  c("POKE_storm5c_08_05_fDOM_FI",POKE_storm5c_08_05_fDOM_FI[[2]]),
  c("POKE_storm5d_08_10_fDOM_FI",POKE_storm5d_08_10_fDOM_FI[[2]]),
  c("POKE_storm6a_08_12_fDOM_FI",POKE_storm6a_08_12_fDOM_FI[[2]]),
  c("POKE_storm6a_08_12_fDOM_FI",NA, NA, NA),
  c("POKE_storm7_08_15_fDOM_FI",POKE_storm7_08_15_fDOM_FI[[2]]),
  c("POKE_storm8_09_29_fDOM_FI",POKE_storm8_09_29_fDOM_FI[[2]]),
  c("POKE_storm9_10_04_fDOM_FI", NA, NA, NA),
  
  c("POKE_storm1_06_30_SPC_FI",POKE_storm1_06_30_SPC_FI[[2]]),
  c("POKE_storm2_07_12_SPC_FI",POKE_storm2_07_12_SPC_FI[[2]]),
  c("POKE_storm3_07_26_SPC_FI",POKE_storm3_07_26_SPC_FI[[2]]),
  c("POKE_storm4_07_31_SPC_FI",POKE_storm4_07_31_SPC_FI[[2]]),
  c("POKE_storm5a_08_02_SPC_FI",POKE_storm5a_08_02_SPC_FI[[2]]),
  c("POKE_storm5b_08_03_SPC_FI",POKE_storm5b_08_03_SPC_FI[[2]]),
  c("POKE_storm5c_08_05_SPC_FI",POKE_storm5c_08_05_SPC_FI[[2]]),
  c("POKE_storm5d_08_10_SPC_FI",POKE_storm5d_08_10_SPC_FI[[2]]),
  c("POKE_storm6a_08_12_SPC_FI",POKE_storm6a_08_12_SPC_FI[[2]]),
  c("POKE_storm6a_08_12_SPC_FI",NA, NA, NA),
  c("POKE_storm7_08_15_SPC_FI",POKE_storm7_08_15_SPC_FI[[2]]),
  c("POKE_storm8_09_29_SPC_FI",POKE_storm8_09_29_SPC_FI[[2]]),
  c("POKE_storm9_10_04_SPC_FI", NA, NA, NA),
  
  c("POKE_storm1_06_30_turb_FI",POKE_storm1_06_30_turb_FI[[2]]),
  c("POKE_storm2_07_12_turb_FI",POKE_storm2_07_12_turb_FI[[2]]),
  c("POKE_storm3_07_26_turb_FI",POKE_storm3_07_26_turb_FI[[2]]),
  c("POKE_storm4_07_31_turb_FI",POKE_storm4_07_31_turb_FI[[2]]),
  c("POKE_storm5a_08_02_turb_FI",POKE_storm5a_08_02_turb_FI[[2]]),
  c("POKE_storm5b_08_03_turb_FI",POKE_storm5b_08_03_turb_FI[[2]]),
  c("POKE_storm5c_08_05_turb_FI",POKE_storm5c_08_05_turb_FI[[2]]),
  c("POKE_storm5d_08_10_turb_FI",POKE_storm5d_08_10_turb_FI[[2]]),
  c("POKE_storm6a_08_12_turb_FI",POKE_storm6a_08_12_turb_FI[[2]]),
  c("POKE_storm6a_08_12_turb_FI",NA, NA, NA),
  c("POKE_storm7_08_15_turb_FI",POKE_storm7_08_15_turb_FI[[2]]),
  c("POKE_storm8_09_29_turb_FI",POKE_storm8_09_29_turb_FI[[2]]),
  c("POKE_storm9_10_04_turb_FI", NA, NA, NA),
  
  c("STRT_storm1_05_31_NO3_FI",STRT_storm1_05_31_NO3_FI[[2]]),
  c("STRT_storm2_07_12_NO3_FI",STRT_storm2_07_12_NO3_FI[[2]]),
  c("STRT_storm3a_07_25_NO3_FI",NA,NA,NA),
  c("STRT_storm3b_08_05_NO3_FI",NA, NA, NA),
  c("STRT_storm3c_08_12_NO3_FI",STRT_storm3c_08_12_NO3_FI[[2]]),
  c("STRT_storm4_08_15_NO3_FI",NA, NA, NA),
  c("STRT_storm5_08_20_NO3_FI",NA, NA, NA),
  c("STRT_storm6_09_20_NO3_FI",STRT_storm6_09_20_NO3_FI[[2]]),
  c("STRT_storm7_10_01_NO3_FI",STRT_storm7_10_01_NO3_FI[[2]]),
  c("STRT_storm7b_10_04_NO3_FI",STRT_storm7b_10_04_NO3_FI[[2]]),
  c("STRT_storm7c_10_09_NO3_FI",STRT_storm7c_10_09_NO3_FI[[2]]),
  
  c("STRT_storm1_05_31_fDOM_FI",NA, NA, NA),
  c("STRT_storm2_07_12_fDOM_FI",STRT_storm2_07_12_fDOM_FI[[2]]),
  c("STRT_storm3a_07_25_fDOM_FI",STRT_storm3a_07_25_fDOM_FI[[2]]),
  c("STRT_storm3b_08_05_fDOM_FI",STRT_storm3b_08_05_fDOM_FI[[2]]),
  c("STRT_storm3c_08_12_fDOM_FI",STRT_storm3c_08_12_fDOM_FI[[2]]),
  c("STRT_storm4_08_15_fDOM_FI",STRT_storm4_08_15_fDOM_FI[[2]]),
  c("STRT_storm5_08_20_fDOM_FI",STRT_storm5_08_20_fDOM_FI[[2]]),
  c("STRT_storm6_09_20_fDOM_FI",NA, NA, NA),
  c("STRT_storm7_10_01_fDOM_FI",NA, NA, NA),
  c("STRT_storm7b_10_04_fDOM_FI",STRT_storm7b_10_04_fDOM_FI[[2]]),
  c("STRT_storm7c_10_09_fDOM_FI",NA, NA, NA),
  
  c("STRT_storm1_05_31_SPC_FI",NA, NA, NA),
  c("STRT_storm2_07_12_SPC_FI",STRT_storm2_07_12_SPC_FI[[2]]),
  c("STRT_storm3a_07_25_SPC_FI",STRT_storm3a_07_25_SPC_FI[[2]]),
  c("STRT_storm3b_08_05_SPC_FI",STRT_storm3b_08_05_SPC_FI[[2]]),
  c("STRT_storm3c_08_12_SPC_FI",STRT_storm3c_08_12_SPC_FI[[2]]),
  c("STRT_storm4_08_15_SPC_FI",STRT_storm4_08_15_SPC_FI[[2]]),
  c("STRT_storm5_08_20_SPC_FI",STRT_storm5_08_20_SPC_FI[[2]]),
  c("STRT_storm6_09_20_SPC_FI",NA, NA, NA),
  c("STRT_storm7_10_01_SPC_FI",NA, NA, NA),
  c("STRT_storm7b_10_04_SPC_FI",STRT_storm7b_10_04_SPC_FI[[2]]),
  c("STRT_storm7c_10_09_SPC_FI",NA, NA, NA),
  
  c("STRT_storm1_05_31_turb_FI",NA, NA, NA),
  c("STRT_storm2_07_12_turb_FI",STRT_storm2_07_12_turb_FI[[2]]),
  c("STRT_storm3a_07_25_turb_FI",STRT_storm3a_07_25_turb_FI[[2]]),
  c("STRT_storm3b_08_05_turb_FI",STRT_storm3b_08_05_turb_FI[[2]]),
  c("STRT_storm3c_08_12_turb_FI",STRT_storm3c_08_12_turb_FI[[2]]),
  c("STRT_storm4_08_15_turb_FI",STRT_storm4_08_15_turb_FI[[2]]),
  c("STRT_storm5_08_20_turb_FI",STRT_storm5_08_20_SPC_FI[[2]]),
  c("STRT_storm6_09_20_turb_FI",NA, NA, NA),
  c("STRT_storm7_10_01_turb_FI",NA, NA, NA),
  c("STRT_storm7b_10_04_turb_FI",STRT_storm7b_10_04_turb_FI[[2]]),
  c("STRT_storm7c_10_09_turb_FI",NA, NA, NA),
  
  c("VAUL_storm1_07_13_NO3_FI",VAUL_storm1_07_13_NO3_FI[[2]]),
  c("VAUL_storm2_07_26_NO3_FI",VAUL_storm2_07_26_NO3_FI[[2]]),
  c("VAUL_storm3_07_29_NO3_FI",VAUL_storm3_07_29_NO3_FI[[2]]),
  c("VAUL_storm4a_08_02_NO3_FI",VAUL_storm4a_08_02_NO3_FI[[2]]),
  c("VAUL_storm4b_08_03_NO3_FI",VAUL_storm4b_08_03_NO3_FI[[2]]),
  c("VAUL_storm4c_08_05_NO3_FI",VAUL_storm4c_08_05_NO3_FI[[2]]),
  c("VAUL_storm5_08_12_NO3_FI",VAUL_storm5_08_12_NO3_FI[[2]]),
  c("VAUL_storm6_08_15_NO3_FI",VAUL_storm6_08_15_NO3_FI[[2]]),
  c("VAUL_storm7_09_19_NO3_FI",VAUL_storm7_09_19_NO3_FI[[2]]),
  c("VAUL_storm8a_09_29_NO3_FI",VAUL_storm8a_09_29_NO3_FI[[2]]),
  c("VAUL_storm8b_10_01_NO3_FI",NA, NA, NA),
  c("VAUL_storm8c_10_04_NO3_FI",NA, NA, NA),
  
  c("VAUL_storm1_07_13_fDOM_FI",VAUL_storm1_07_13_fDOM_FI[[2]]),
  c("VAUL_storm2_07_26_fDOM_FI",VAUL_storm2_07_26_fDOM_FI[[2]]),
  c("VAUL_storm3_07_29_fDOM_FI",VAUL_storm3_07_29_fDOM_FI[[2]]),
  c("VAUL_storm4a_08_02_fDOM_FI",VAUL_storm4a_08_02_fDOM_FI[[2]]),
  c("VAUL_storm4b_08_03_fDOM_FI",VAUL_storm4b_08_03_fDOM_FI[[2]]),
  c("VAUL_storm4c_08_05_fDOM_FI",VAUL_storm4c_08_05_fDOM_FI[[2]]),
  c("VAUL_storm5_08_12_fDOM_FI",NA, NA, NA),
  c("VAUL_storm6_08_15_fDOM_FI",NA, NA, NA),
  c("VAUL_storm7_09_19_fDOM_FI",NA, NA, NA),
  c("VAUL_storm8a_09_29_NO3_FI",NA, NA, NA),
  c("VAUL_storm8b_10_01_NO3_FI",NA, NA, NA),
  c("VAUL_storm8c_10_04_NO3_FI",NA, NA, NA),
  
  c("VAUL_storm1_07_13_SPC_FI",VAUL_storm1_07_13_SPC_FI[[2]]),
  c("VAUL_storm2_07_26_SPC_FI",VAUL_storm2_07_26_SPC_FI[[2]]),
  c("VAUL_storm3_07_29_SPC_FI",VAUL_storm3_07_29_SPC_FI[[2]]),
  c("VAUL_storm4a_08_02_SPC_FI",VAUL_storm4a_08_02_SPC_FI[[2]]),
  c("VAUL_storm4b_08_03_SPC_FI",VAUL_storm4b_08_03_SPC_FI[[2]]),
  c("VAUL_storm4c_08_05_SPC_FI",VAUL_storm4c_08_05_SPC_FI[[2]]),
  c("VAUL_storm5_08_12_SPC_FI",NA, NA, NA),
  c("VAUL_storm6_08_15_SPC_FI",NA, NA, NA),
  c("VAUL_storm7_09_19_SPC_FI",NA, NA, NA),
  c("VAUL_storm8a_09_29_NO3_FI",NA, NA, NA),
  c("VAUL_storm8b_10_01_NO3_FI",NA, NA, NA),
  c("VAUL_storm8c_10_04_NO3_FI",NA, NA, NA),
  
  c("VAUL_storm1_07_13_turb_FI",VAUL_storm1_07_13_turb_FI[[2]]),
  c("VAUL_storm2_07_26_turb_FI",VAUL_storm2_07_26_turb_FI[[2]]),
  c("VAUL_storm3_07_29_turb_FI",VAUL_storm3_07_29_turb_FI[[2]]),
  c("VAUL_storm4a_08_02_turb_FI",VAUL_storm4a_08_02_turb_FI[[2]]),
  c("VAUL_storm4b_08_03_turb_FI",VAUL_storm4b_08_03_turb_FI[[2]]),
  c("VAUL_storm4c_08_05_turb_FI",VAUL_storm4c_08_05_turb_FI[[2]]),
  c("VAUL_storm5_08_12_turb_FI",NA, NA, NA),
  c("VAUL_storm6_08_15_turb_FI",NA, NA, NA),
  c("VAUL_storm7_09_19_turb_FI",NA, NA, NA),
  c("VAUL_storm8a_09_29_NO3_FI",NA, NA, NA),
  c("VAUL_storm8b_10_01_NO3_FI",NA, NA, NA),
  c("VAUL_storm8c_10_04_NO3_FI",NA, NA, NA))
  
FI_results = as.data.frame(FI_results)

names(FI_results) = c("ID", "Flushing_index", "percCI_2.5", "percCI_97.5")

FI_results$ID = unlist(FI_results$ID)
FI_results$Flushing_index = round(as.numeric(as.character(FI_results$Flushing_index)), 4)
FI_results$`percCI_2.5` = round(as.numeric(as.character(FI_results$`percCI_2.5`)), 4)
FI_results$`percCI_97.5` = round(as.numeric(as.character(FI_results$`percCI_97.5`)), 4)

write.csv(FI_results, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/all.FI.diff.results.csv")

# calculate 95% bootstrap around median of Hyst. Indicies for each site and storm #

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

# FRCH #
FRCH.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2019/FRCH/FRCH.HI.df.csv")

storm.list = unique(FRCH.HI.df$storm.ID)
FRCH.HI.boot <- do.call(rbind.data.frame,
                      lapply(storm.list, function(i){
                        dat = subset(FRCH.HI.df, storm.ID == i)
                        median_cl_boot(dat$HI)
                      }))
FRCH.HI.boot$storm.ID = storm.list

# MOOS #
MOOS.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2019/MOOS/MOOS.HI.df.csv")

storm.list = unique(MOOS.HI.df$storm.ID)
MOOS.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(MOOS.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
MOOS.HI.boot$storm.ID = storm.list

# POKE #
POKE.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2019/POKE/POKE.HI.df.csv")

storm.list = unique(POKE.HI.df$storm.ID)
POKE.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(POKE.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
POKE.HI.boot$storm.ID = storm.list

# STRT #
STRT.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2019/STRT/STRT.HI.df.csv")

storm.list = unique(STRT.HI.df$storm.ID)
STRT.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(STRT.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
STRT.HI.boot$storm.ID = storm.list

# VAUL #
VAUL.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2019/VAUL/VAUL.HI.df.csv")

storm.list = unique(VAUL.HI.df$storm.ID)
VAUL.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(VAUL.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
VAUL.HI.boot$storm.ID = storm.list


# join data #

FRCH.HI.boot$site.ID = "FRCH"
MOOS.HI.boot$site.ID = "MOOS"
POKE.HI.boot$site.ID = "POKE"
STRT.HI.boot$site.ID = "STRT"
VAUL.HI.boot$site.ID = "VAUL"

HI = rbind(FRCH.HI.boot, MOOS.HI.boot, POKE.HI.boot, STRT.HI.boot, VAUL.HI.boot)

all.FI.diff.results = read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/all.FI.diff.results.csv", header = T, row.names = 1)

FI = subset(all.FI.diff.results, select=c("Flushing_index", "percCI_2.5", "percCI_97.5", "ID"))
FI$ID = as.character(FI$ID)
FI = separate(FI, ID, into=c("site.ID", "storm.ID", "month", "day", "response_var", NA), sep = "_")
names(FI) = c("Flush_index", "FI_ymin", "FI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI$site.ID=NULL
HI = separate(HI, storm.ID, into=c("site.ID", "storm.ID", "month", "day", "response_var"), sep = "_")
names(HI) = c("Hyst_index", "HI_ymin", "HI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI_FI = left_join(HI, FI, by=c("site.ID", "storm.ID", "response_var"))
write.csv(HI_FI, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results.csv")


# plot #

# NO3 #
HI_FI_NO3 = subset(HI_FI, response_var == "NO3")
HI_FI_NO3$site.ID <- factor(HI_FI_NO3$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_NO3.p = 
  ggplot(HI_FI_NO3, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_NO3.p

# fDOM #
HI_FI_fDOM = subset(HI_FI, response_var == "fDOM")
HI_FI_fDOM$site.ID <- factor(HI_FI_fDOM$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_fDOM.p = 
  ggplot(HI_FI_fDOM, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("b) fDOM")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_fDOM.p

# SPC #
HI_FI_SPC = subset(HI_FI, response_var == "SPC")
HI_FI_SPC$site.ID <- factor(HI_FI_SPC$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_SPC.p = 
  ggplot(HI_FI_SPC, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("c) SPC")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_SPC.p

# turb #
HI_FI_turb = subset(HI_FI, response_var == "turb")
HI_FI_turb$site.ID <- factor(HI_FI_turb$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_turb.p = 
  ggplot(HI_FI_turb, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("d) Turb")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_turb.p


grid.arrange(HI_FI_NO3.p,HI_FI_fDOM.p,HI_FI_SPC.p,HI_FI_turb.p)




##############################################################################################################
####################### Beta Test 1/13/22 #######################################################################
##############################################################################################################
# Step 1) Load in the storm files
# Step 2) normalize dicharge and concentration for each solute
# Step 3) calculate Beta for each solute for each site
# Step 4) plot against HI 
#################################### 2018 ###############################################################
FRCHstorm_file_list <- list.files(path="~/Documents/Storms/Storm_Events/2018/All_Sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE) # reading in individual storms by site 

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 142),
                          rep("storm10", 704),
                          rep("storm11a", 91),
                          rep("storm11b", 264),
                          rep("storm2a", 230),
                          rep("storm2b", 190),
                          rep("storm3", 212),
                          rep("storm4a", 72),
                          rep("storm4b", 383),
                          rep("storm5", 331),
                          rep("storm6", 303),
                          rep("storm7", 119),
                          rep("storm8a", 79),
                          rep("storm8b", 95),
                          rep("storm9", 115)) # naming each storm by the number of storm 


FRCH_2018_test_beta <- read_csv("~/Desktop/FRCH_2018_test_beta.csv")
attributes(FRCH_2018_test_beta$DateTime)$tzone <- 'America/Anchorage'

cols.1 <- c("datavalue.x", "datavalue.y") 
fdom.storm5[cols.1] <- log(fdom.storm5[cols.1])

cols <- c("fDOM.QSU","nitrateuM", "SpCond.uScm", "Turbidity.FNU", "MeanDischarge")
FRCH_2018_test_beta[cols] <- log(FRCH_2018_test_beta[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}


# Try it by all sites per response
FRCH_2018_test_beta.NO3 <- FRCH_2018_test_beta[,-c(5,7,8)] # only have NO3 as the response
FRCH_2018_test_beta.fDOM <- FRCH_2018_test_beta[,-c(6:8)] # only have fdom as the response
FRCH_2018_test_beta.SPC <- FRCH_2018_test_beta[,-c(5,6,8)] # only have fdom as the response
FRCH_2018_test_beta.turb <- FRCH_2018_test_beta[,-c(5:7)] # only have fdom as the response

# NO3
all_NO3_test <- FRCH_2018_test_beta.NO3 %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_NO3_ascending <- filter(all_NO3_test, limb == "ascending")
all_NO3_ascending <-  all_NO3_ascending[!is.na(all_NO3_ascending$nitrateuM), ]

beta.all.no3 <- all_NO3_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(MeanDischarge, nitrateuM)) # this works just like the beta one that is for an individual site

# fDOM
all_fDOM_test <- FRCH_2018_test_beta.fDOM %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_fDOM_ascending <- filter(all_fDOM_test, limb == "ascending")
all_fDOM_ascending <-  all_fDOM_ascending[!is.na(all_fDOM_ascending$fDOM.QSU), ]

beta.all.fDOM <- all_fDOM_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(MeanDischarge, fDOM.QSU)) # this works just like the beta one that is for an individual site

# SPC
all_SPC_test <- FRCH_2018_test_beta.SPC %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_SPC_ascending <- filter(all_SPC_test, limb == "ascending")
all_SPC_ascending <-  all_SPC_ascending[!is.na(all_SPC_ascending$SpCond.uScm), ]

beta.all.SPC <- all_SPC_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(MeanDischarge, SpCond.uScm)) # this works just like the beta one that is for an individual site

# Turb
all_turb_test <- FRCH_2018_test_beta.turb %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(MeanDischarge)], "ascending", "descending"))

all_turb_ascending <- filter(all_turb_test, limb == "ascending")
all_turb_ascending <-  all_turb_ascending[!is.na(all_turb_ascending$Turbidity.FNU), ]

beta.all.turb <- all_turb_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(MeanDischarge, Turbidity.FNU)) # this works just like the beta one that is for an individual site



# Everything south of this has not been done yet as of 1/13/22....
### I need to go through the 03_HI_FI script again with my new discharge data

# merge HI and beta 
HI.all <- read.csv("Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.2020.csv")
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
  ggplot(beta.HI.NO3, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(pf), alpha = doy), size = 4)+
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("NO3")+ 
  xlab("") +
  ylab("HI") +
  theme(legend.position = "none") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20),
        legend.text = element_text(size = 8)) 
HI_FI_NO3.p

HI_FI_fDOM.p = 
  ggplot(beta.HI.fDOM, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(pf), alpha = doy), size = 4)+
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("fDOM")+ 
  ylab("") +
  xlab("") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20),
        legend.text = element_text(size = 10))
HI_FI_fDOM.p

HI_FI_SPC.p = 
  ggplot(beta.HI.SPC, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(pf), alpha = doy), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("SPC")+ 
  xlab("Storm Flushing Index") + 
  theme(legend.position = "none") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 30)) 
HI_FI_SPC.p

HI_FI_turb.p = 
  ggplot(beta.HI.turb, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(pf), alpha = doy), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("Turbidity")+ 
  xlab("Storm Flushing Index") + 
  theme(legend.position = "none") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 30)) +
  labs(
    colour = "Permafrost Extent")
HI_FI_turb.p


ggarrange(HI_FI_NO3.p, HI_FI_fDOM.p,  
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend="bottom",
          font.label = list(size = 100))

plot_grid(HI_FI_NO3.p, HI_FI_fDOM.p,
          cols = 2)

# burn #
HI_FI_NO3.p = 
  ggplot(beta.HI.NO3, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(burn), alpha = doy), size = 4)+
  scale_fill_manual(values = c("brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("NO3")+ 
  xlab("") +
  ylab("HI") +
  theme_classic() +
  labs(color = "PF Extent")
HI_FI_NO3.p

HI_FI_fDOM.p = 
  ggplot(beta.HI.fDOM, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(burn), alpha = doy), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("fDOM")+ 
  ylab("") +
  xlab("") +
  theme(legend.position = "none") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_fDOM.p

HI_FI_SPC.p = 
  ggplot(beta.HI.SPC, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(burn), alpha = doy), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("SPC")+ 
  xlab("Storm Flushing Index") + 
  theme(legend.position = "none") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_SPC.p

HI_FI_turb.p = 
  ggplot(beta.HI.turb, aes(beta, HI)) + geom_point(aes(colour= Site, shape = factor(burn), alpha = doy), size = 4)+
  scale_fill_manual(values = c("#E69F00", "brown", "deepskyblue4")) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+theme_bw()+
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("Turbidity")+ 
  xlab("Storm Flushing Index") + 
  theme(legend.position = "none") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) +
  labs(
    colour = "Permafrost Extent")
HI_FI_turb.p

########################################## 2019 ##########################################################
storm_file_list_beta <- list.files(path="~/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI/", 
                              recursive=F, 
                              pattern=".csv", 
                              full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                   read.csv, 
                                   stringsAsFactors=FALSE, 
                                   header=T, row.names=1))

storm_file_list_beta = sub("~/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//", storm_file_list_beta, replacement = "")
storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

for(i in 1:length(storm_list_beta)){
  storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(storm_list_beta[[i]][["valuedatetime"]],
                                                  "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 


#  organize storm data by site and solute # 5 for each storm 
CARI_storm_list_beta = storm_list_beta[c(1:60)] #60
FRCH_storm_list_beta = storm_list_beta[c(61:165)] #105
MOOS_storm_list_beta = storm_list_beta[c(166:230)] #60
POKE_storm_list_beta = storm_list_beta[c(231:295)]# 65
STRT_storm_list_beta = storm_list_beta[c(296:350)] #55
VAUL_storm_list_beta = storm_list_beta[c(351:410)] #65

CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("Turb", names(CARI_storm_list_beta)))]
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("Turb", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("Turb", names(MOOS_storm_list_beta)))]
MOOS_Q_storm_list_beta = MOOS_storm_list_beta[c(grep("Q", names(MOOS_storm_list_beta)))]

POKE_NO3_storm_list_beta = POKE_storm_list_beta[c(grep("NO3", names(POKE_storm_list_beta)))]
POKE_fDOM_storm_list_beta = POKE_storm_list_beta[c(grep("fDOM", names(POKE_storm_list_beta)))]
POKE_SpCond_storm_list_beta = POKE_storm_list_beta[c(grep("SPC", names(POKE_storm_list_beta)))]
POKE_turb_storm_list_beta = POKE_storm_list_beta[c(grep("Turb", names(POKE_storm_list_beta)))]
POKE_Q_storm_list_beta = POKE_storm_list_beta[c(grep("Q", names(POKE_storm_list_beta)))]

STRT_NO3_storm_list_beta = STRT_storm_list_beta[c(grep("NO3", names(STRT_storm_list_beta)))]
STRT_fDOM_storm_list_beta = STRT_storm_list_beta[c(grep("fDOM", names(STRT_storm_list_beta)))]
STRT_SpCond_storm_list_beta = STRT_storm_list_beta[c(grep("SPC", names(STRT_storm_list_beta)))]
STRT_turb_storm_list_beta = STRT_storm_list_beta[c(grep("Turb", names(STRT_storm_list_beta)))]
STRT_Q_storm_list_beta = STRT_storm_list_beta[c(grep("Q", names(STRT_storm_list_beta)))]

VAUL_NO3_storm_list_beta = VAUL_storm_list_beta[c(grep("NO3", names(VAUL_storm_list_beta)))]
VAUL_fDOM_storm_list_beta = VAUL_storm_list_beta[c(grep("fDOM", names(VAUL_storm_list_beta)))]
VAUL_SpCond_storm_list_beta = VAUL_storm_list_beta[c(grep("SPC", names(VAUL_storm_list_beta)))]
VAUL_turb_storm_list_beta = VAUL_storm_list_beta[c(grep("turb", names(VAUL_storm_list_beta)))]
VAUL_Q_storm_list_beta = VAUL_storm_list_beta[c(grep("Q", names(VAUL_storm_list_beta)))]

# normalize Q data 
# FRCH
for(i in 1:length(FRCH_Q_storm_list_beta)){
  FRCH_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_Q_storm_list_beta[[i]][["datavalue"]]-min(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# MOOS
for(i in 1:length(MOOS_Q_storm_list_beta)){
  MOOS_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_Q_storm_list_beta[[i]][["datavalue"]]-min(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# POKE
for(i in 1:length(POKE_Q_storm_list_beta)){
  POKE_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_Q_storm_list_beta[[i]][["datavalue"]]-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# STRT
for(i in 1:length(STRT_Q_storm_list_beta)){
  STRT_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_Q_storm_list_beta[[i]][["datavalue"]]-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# VAUL
for(i in 1:length(VAUL_Q_storm_list_beta)){
  VAUL_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_Q_storm_list_beta[[i]][["datavalue"]]-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# CARI
for(i in 1:length(CARI_Q_storm_list_beta)){
  CARI_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_Q_storm_list_beta[[i]][["datavalue"]]-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# normalize solute data 
# 
#NO3
for(i in 1:length(FRCH_NO3_storm_list_beta)){
  FRCH_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list_beta[[i]][["datavalue"]]-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_NO3_storm_list_beta)){
  MOOS_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_NO3_storm_list_beta[[i]][["datavalue"]]-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_NO3_storm_list_beta)){
  POKE_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_NO3_storm_list_beta[[i]][["datavalue"]]-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_NO3_storm_list_beta)){
  STRT_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_NO3_storm_list_beta[[i]][["datavalue"]]-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_NO3_storm_list_beta)){
  VAUL_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_NO3_storm_list_beta[[i]][["datavalue"]]-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_NO3_storm_list_beta)){
  CARI_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_NO3_storm_list_beta[[i]][["datavalue"]]-min(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#fDOM
for(i in 1:length(FRCH_fDOM_storm_list_beta)){
  FRCH_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_fDOM_storm_list_beta[[i]][["datavalue"]]-min(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_fDOM_storm_list_beta)){
  MOOS_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_fDOM_storm_list_beta[[i]][["datavalue"]]-min(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_fDOM_storm_list_beta)){
  POKE_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_fDOM_storm_list_beta[[i]][["datavalue"]]-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_fDOM_storm_list_beta)){
  STRT_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_fDOM_storm_list_beta[[i]][["datavalue"]]-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_fDOM_storm_list_beta)){
  VAUL_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_fDOM_storm_list_beta[[i]][["datavalue"]]-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_fDOM_storm_list_beta)){
  CARI_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_fDOM_storm_list_beta[[i]][["datavalue"]]-min(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#SPC
for(i in 1:length(FRCH_SpCond_storm_list_beta)){
  FRCH_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_SpCond_storm_list_beta[[i]][["datavalue"]]-min(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_SpCond_storm_list_beta)){
  MOOS_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_SpCond_storm_list_beta[[i]][["datavalue"]]-min(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_SpCond_storm_list_beta)){
  POKE_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_SpCond_storm_list_beta[[i]][["datavalue"]]-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_SpCond_storm_list_beta)){
  STRT_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_SpCond_storm_list_beta[[i]][["datavalue"]]-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_SpCond_storm_list_beta)){
  VAUL_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_SpCond_storm_list_beta[[i]][["datavalue"]]-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_SpCond_storm_list_beta)){
  CARI_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_SpCond_storm_list_beta[[i]][["datavalue"]]-min(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#Turb
for(i in 1:length(FRCH_turb_storm_list_beta)){
  FRCH_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_turb_storm_list_beta[[i]][["datavalue"]]-min(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_turb_storm_list_beta)){
  MOOS_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_turb_storm_list_beta[[i]][["datavalue"]]-min(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_turb_storm_list_beta)){
  POKE_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_turb_storm_list_beta[[i]][["datavalue"]]-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_turb_storm_list_beta)){
  STRT_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_turb_storm_list_beta[[i]][["datavalue"]]-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_turb_storm_list_beta)){
  VAUL_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_turb_storm_list_beta[[i]][["datavalue"]]-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_turb_storm_list_beta[[i]][["datavalue"]]-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}
###### NO3  #######
FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
POKE_NO3_storm <- map2_df(POKE_Q_storm_list_beta, POKE_NO3_storm_list_beta, inner_join, by = "valuedatetime")
STRT_NO3_storm <- map2_df(STRT_Q_storm_list_beta, STRT_NO3_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_NO3_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 993),
                   rep("storm10a", 121),
                   rep("storm10b", 95),
                   rep("storm10c", 207),
                   rep("storm11", 479),
                   rep("storm12a", 183),
                   rep("storm12b", 67),
                   rep("storm12c", 511),
                   rep("storm12d", 99),
                   rep("storm12e", 127),
                   rep("storm13", 391),
                   rep("storm14", 631),
                   rep("storm2", 165),
                   rep("storm3", 201),
                   rep("storm4", 193),
                   rep("storm5", 229),
                   rep("storm6", 257),
                   rep("storm7", 133),
                   rep("storm8", 105),
                   rep("storm9a", 61),
                   rep("storm9b", 149))

names(FRCH_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
FRCH_NO3_storm$site.ID <- "FRCH"

cols <- c("NO3.norm","Q.norm")
FRCH_NO3_storm[cols] <- log(FRCH_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_NO3_storm <- FRCH_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_NO3_storm_ascending <- filter(FRCH_NO3_storm, limb == "ascending")

FRCH_NO3_storm_ascending <- FRCH_NO3_storm_ascending[is.finite(FRCH_NO3_storm_ascending$Q.norm) & is.finite(FRCH_NO3_storm_ascending$NO3.norm), ]

beta.all.no3 <- FRCH_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
MOOS_NO3_storm$site.ID <- "MOOS"

MOOS_NO3_storm[cols] <- log(MOOS_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_NO3_storm <- MOOS_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_NO3_storm_ascending <- filter(MOOS_NO3_storm, limb == "ascending")

MOOS_NO3_storm_ascending <- MOOS_NO3_storm_ascending[is.finite(MOOS_NO3_storm_ascending$Q.norm) & is.finite(MOOS_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.moos.with.all <- MOOS_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_NO3_storm$storm.num = c(rep("storm1", 103),
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
names(MOOS_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.num")
MOOS_NO3_storm$Site <- "MOOS"

MOOS_NO3_storm[cols] <- log(MOOS_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_NO3_storm <- MOOS_NO3_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_NO3_storm_ascending <- filter(MOOS_NO3_storm, limb == "ascending")

MOOS_NO3_storm_ascending <- MOOS_NO3_storm_ascending[is.finite(MOOS_NO3_storm_ascending$Q.norm) & is.finite(MOOS_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.moos.with.all <- MOOS_NO3_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_NO3_storm$storm.ID = c(rep("storm1", 638),
                             rep("storm2", 274),
                             rep("storm3a", 1035),
                             rep("storm3b", 286),
                             rep("storm3c", 174),
                             rep("storm4", 466),
                             rep("storm5", 98),
                             rep("storm6", 246),
                             rep("storm7", 218),
                             rep("storm7b", 266),
                             rep("storm7c", 258))

names(STRT_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
STRT_NO3_storm$site.ID <- "STRT"

STRT_NO3_storm[cols] <- log(STRT_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_NO3_storm <- STRT_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_NO3_storm_ascending <- filter(STRT_NO3_storm, limb == "ascending")

STRT_NO3_storm_ascending <- STRT_NO3_storm_ascending[is.finite(STRT_NO3_storm_ascending$Q.norm) & is.finite(STRT_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.strt <- STRT_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_NO3_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_NO3_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_NO3_storm_ascending$DateTime <- as.POSIXct(STRT_NO3_storm_ascending$DateTime, 
                                                  "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_NO3_storm_ascending$DateTime <- as.POSIXct(VAUL_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending, 
                       STRT_NO3_storm_ascending, VAUL_NO3_storm_ascending )

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
POKE_fDOM_storm <- map2_df(POKE_Q_storm_list_beta, POKE_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
STRT_fDOM_storm <- map2_df(STRT_Q_storm_list_beta, STRT_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_fDOM_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 993),
                            rep("storm10a", 121),
                            rep("storm10b", 95),
                            rep("storm10c", 207),
                            rep("storm11", 479),
                            rep("storm12a", 183),
                            rep("storm12b", 67),
                            rep("storm12c", 511),
                            rep("storm12d", 99),
                            rep("storm12e", 127),
                            rep("storm13", 391),
                            rep("storm14", 631),
                            rep("storm2", 165),
                            rep("storm3", 201),
                            rep("storm4", 193),
                            rep("storm5", 229),
                            rep("storm6", 257),
                            rep("storm7", 133),
                            rep("storm8", 105),
                            rep("storm9a", 61),
                            rep("storm9b", 149))

names(FRCH_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
FRCH_fDOM_storm$site.ID <- "FRCH"

cols <- c("fDOM.norm","Q.norm")
FRCH_fDOM_storm[cols] <- log(FRCH_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_fDOM_storm <- FRCH_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_fDOM_storm_ascending <- filter(FRCH_fDOM_storm, limb == "ascending")

FRCH_fDOM_storm_ascending <- FRCH_fDOM_storm_ascending[is.finite(FRCH_fDOM_storm_ascending$Q.norm) & is.finite(FRCH_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM <- FRCH_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
MOOS_fDOM_storm$site.ID <- "MOOS"

MOOS_fDOM_storm[cols] <- log(MOOS_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_fDOM_storm <- MOOS_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_fDOM_storm_ascending <- filter(MOOS_fDOM_storm, limb == "ascending")

MOOS_fDOM_storm_ascending <- MOOS_fDOM_storm_ascending[is.finite(MOOS_fDOM_storm_ascending$Q.norm) & is.finite(MOOS_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.moos.with.all <- MOOS_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_fDOM_storm$storm.num = c(rep("storm1", 103),
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
names(POKE_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", ".norm", "storm.num")
POKE_fDOM_storm$Site <- "POKE"

POKE_fDOM_storm[cols] <- log(POKE_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_fDOM_storm <- POKE_fDOM_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_fDOM_storm_ascending <- filter(POKE_fDOM_storm, limb == "ascending")

POKE_fDOM_storm_ascending <- POKE_fDOM_storm_ascending[is.finite(POKE_fDOM_storm_ascending$Q.norm) & is.finite(POKE_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.pok <- POKE_fDOM_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_fDOM_storm$storm.ID = c(rep("storm1", 638),
                            rep("storm2", 274),
                            rep("storm3a", 1035),
                            rep("storm3b", 286),
                            rep("storm3c", 174),
                            rep("storm4", 466),
                            rep("storm5", 98),
                            rep("storm6", 246),
                            rep("storm7", 218),
                            rep("storm7b", 266),
                            rep("storm7c", 258))

names(STRT_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
STRT_fDOM_storm$site.ID <- "STRT"

STRT_fDOM_storm[cols] <- log(STRT_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_fDOM_storm <- STRT_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_fDOM_storm_ascending <- filter(STRT_fDOM_storm, limb == "ascending")

STRT_fDOM_storm_ascending <- STRT_fDOM_storm_ascending[is.finite(STRT_fDOM_storm_ascending$Q.norm) & is.finite(STRT_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.strt <- STRT_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_fDOM_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
VAUL_fDOM_storm$site.ID <- "VAUL"

VAUL_fDOM_storm[cols] <- log(VAUL_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_fDOM_storm <- VAUL_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_fDOM_storm_ascending <- filter(VAUL_fDOM_storm, limb == "ascending")

VAUL_fDOM_storm_ascending <- VAUL_fDOM_storm_ascending[is.finite(VAUL_fDOM_storm_ascending$Q.norm) & is.finite(VAUL_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.vaul <- VAUL_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_fDOM_storm_ascending$DateTime <- as.POSIXct(STRT_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_fDOM_storm_ascending$DateTime <- as.POSIXct(VAUL_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending, 
                       STRT_fDOM_storm_ascending, VAUL_fDOM_storm_ascending )

beta.all.fdom <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fdom$response_var <- "fDOM"

##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
POKE_SPC_storm <- map2_df(POKE_Q_storm_list_beta, POKE_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
STRT_SPC_storm <- map2_df(STRT_Q_storm_list_beta, STRT_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_SPC_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm1", 993),
                             rep("storm10a", 121),
                             rep("storm10b", 95),
                             rep("storm10c", 207),
                             rep("storm11", 479),
                             rep("storm12a", 183),
                             rep("storm12b", 67),
                             rep("storm12c", 511),
                             rep("storm12d", 99),
                             rep("storm12e", 127),
                             rep("storm13", 391),
                             rep("storm14", 631),
                             rep("storm2", 165),
                             rep("storm3", 201),
                             rep("storm4", 193),
                             rep("storm5", 229),
                             rep("storm6", 257),
                             rep("storm7", 133),
                             rep("storm8", 105),
                             rep("storm9a", 61),
                             rep("storm9b", 149))

names(FRCH_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
FRCH_SPC_storm$site.ID <- "FRCH"

cols <- c("SPC.norm","Q.norm")
FRCH_SPC_storm[cols] <- log(FRCH_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_SPC_storm <- FRCH_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_SPC_storm_ascending <- filter(FRCH_SPC_storm, limb == "ascending")

FRCH_SPC_storm_ascending <- FRCH_SPC_storm_ascending[is.finite(FRCH_SPC_storm_ascending$Q.norm) & is.finite(FRCH_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC <- FRCH_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_SPC_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
MOOS_SPC_storm$site.ID <- "MOOS"

MOOS_SPC_storm[cols] <- log(MOOS_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_SPC_storm <- MOOS_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_SPC_storm_ascending <- filter(MOOS_SPC_storm, limb == "ascending")

MOOS_SPC_storm_ascending <- MOOS_SPC_storm_ascending[is.finite(MOOS_SPC_storm_ascending$Q.norm) & is.finite(MOOS_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.moos.with.all <- MOOS_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_SPC_storm$storm.num = c(rep("storm1", 103),
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
names(POKE_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.num")
POKE_SPC_storm$Site <- "POKE"

POKE_SPC_storm[cols] <- log(POKE_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_SPC_storm <- POKE_SPC_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_SPC_storm_ascending <- filter(POKE_SPC_storm, limb == "ascending")

POKE_SPC_storm_ascending <- POKE_SPC_storm_ascending[is.finite(POKE_SPC_storm_ascending$Q.norm) & is.finite(POKE_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.pok <- POKE_SPC_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_SPC_storm$storm.ID = c(rep("storm1", 638),
                             rep("storm2", 274),
                             rep("storm3a", 1035),
                             rep("storm3b", 286),
                             rep("storm3c", 174),
                             rep("storm4", 466),
                             rep("storm5", 98),
                             rep("storm6", 246),
                             rep("storm7", 218),
                             rep("storm7b", 266),
                             rep("storm7c", 258))

names(STRT_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
STRT_SPC_storm$site.ID <- "STRT"

STRT_SPC_storm[cols] <- log(STRT_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_SPC_storm <- STRT_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_SPC_storm_ascending <- filter(STRT_SPC_storm, limb == "ascending")

STRT_SPC_storm_ascending <- STRT_SPC_storm_ascending[is.finite(STRT_SPC_storm_ascending$Q.norm) & is.finite(STRT_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.strt <- STRT_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_SPC_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
VAUL_SPC_storm$site.ID <- "VAUL"

VAUL_SPC_storm[cols] <- log(VAUL_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_SPC_storm <- VAUL_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_SPC_storm_ascending <- filter(VAUL_SPC_storm, limb == "ascending")

VAUL_SPC_storm_ascending <- VAUL_SPC_storm_ascending[is.finite(VAUL_SPC_storm_ascending$Q.norm) & is.finite(VAUL_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.vaul <- VAUL_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_SPC_storm_ascending$DateTime <- as.POSIXct(STRT_SPC_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_SPC_storm_ascending$DateTime <- as.POSIXct(VAUL_SPC_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending, 
                        STRT_SPC_storm_ascending, VAUL_SPC_storm_ascending )

beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")
POKE_turb_storm <- map2_df(POKE_Q_storm_list_beta, POKE_turb_storm_list_beta, inner_join, by = "valuedatetime")
STRT_turb_storm <- map2_df(STRT_Q_storm_list_beta, STRT_turb_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_turb_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm1", 993),
                            rep("storm10a", 121),
                            rep("storm10b", 95),
                            rep("storm10c", 207),
                            rep("storm11", 479),
                            rep("storm12a", 183),
                            rep("storm12b", 67),
                            rep("storm12c", 511),
                            rep("storm12d", 99),
                            rep("storm12e", 127),
                            rep("storm13", 391),
                            rep("storm14", 631),
                            rep("storm2", 165),
                            rep("storm3", 201),
                            rep("storm4", 193),
                            rep("storm5", 229),
                            rep("storm6", 257),
                            rep("storm7", 133),
                            rep("storm8", 105),
                            rep("storm9a", 61),
                            rep("storm9b", 149))

names(FRCH_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
FRCH_turb_storm$site.ID <- "FRCH"

cols <- c("turb.norm","Q.norm")
FRCH_turb_storm[cols] <- log(FRCH_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_turb_storm <- FRCH_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_turb_storm_ascending <- filter(FRCH_turb_storm, limb == "ascending")

FRCH_turb_storm_ascending <- FRCH_turb_storm_ascending[is.finite(FRCH_turb_storm_ascending$Q.norm) & is.finite(FRCH_turb_storm_ascending$turb.norm), ]

beta.all.turb <- FRCH_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_turb_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
MOOS_turb_storm$site.ID <- "MOOS"

MOOS_turb_storm[cols] <- log(MOOS_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_turb_storm <- MOOS_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_turb_storm_ascending <- filter(MOOS_turb_storm, limb == "ascending")

MOOS_turb_storm_ascending <- MOOS_turb_storm_ascending[is.finite(MOOS_turb_storm_ascending$Q.norm) & is.finite(MOOS_turb_storm_ascending$turb.norm), ]

beta.all.turb.moos.with.all <- MOOS_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_turb_storm$storm.num = c(rep("storm1", 103),
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
names(POKE_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.num")
POKE_turb_storm$Site <- "POKE"

POKE_turb_storm[cols] <- log(POKE_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_turb_storm <- POKE_turb_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_turb_storm_ascending <- filter(POKE_turb_storm, limb == "ascending")

POKE_turb_storm_ascending <- POKE_turb_storm_ascending[is.finite(POKE_turb_storm_ascending$Q.norm) & is.finite(POKE_turb_storm_ascending$turb.norm), ]

beta.all.turb.poke <- POKE_turb_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_turb_storm$storm.ID = c(rep("storm1", 638),
                            rep("storm2", 274),
                            rep("storm3a", 1035),
                            rep("storm3b", 286),
                            rep("storm3c", 174),
                            rep("storm4", 466),
                            rep("storm5", 98),
                            rep("storm6", 246),
                            rep("storm7", 218),
                            rep("storm7b", 266),
                            rep("storm7c", 258))

names(STRT_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
STRT_turb_storm$site.ID <- "STRT"

STRT_turb_storm[cols] <- log(STRT_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_turb_storm <- STRT_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_turb_storm_ascending <- filter(STRT_turb_storm, limb == "ascending")

STRT_turb_storm_ascending <- STRT_turb_storm_ascending[is.finite(STRT_turb_storm_ascending$Q.norm) & is.finite(STRT_turb_storm_ascending$turb.norm), ]

beta.all.turb.strt <- STRT_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_turb_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
VAUL_turb_storm$site.ID <- "VAUL"

VAUL_turb_storm[cols] <- log(VAUL_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_turb_storm <- VAUL_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_turb_storm_ascending <- filter(VAUL_turb_storm, limb == "ascending")

VAUL_turb_storm_ascending <- VAUL_turb_storm_ascending[is.finite(VAUL_turb_storm_ascending$Q.norm) & is.finite(VAUL_turb_storm_ascending$turb.norm), ]

beta.all.turb.vaul <- VAUL_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_turb_storm_ascending$DateTime <- as.POSIXct(STRT_turb_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_turb_storm_ascending$DateTime <- as.POSIXct(VAUL_turb_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_turb_storm <- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending, 
                       STRT_turb_storm_ascending, VAUL_turb_storm_ascending )

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

beta.all.2019 <- rbind(beta.all.no3, beta.all.fdom,
                       beta.all.SPC, beta.all.turb)

#### plot ####
HI_FI <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results.csv")

HI_FI_beta = left_join(HI_FI, beta.all.2019, by=c("site.ID", "storm.ID", "response_var"))

write.csv(HI_FI_beta, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI_beta.diff_results.csv")

# NO3 #
HI_FI_NO3 = subset(HI_FI_beta, response_var == "NO3")
HI_FI_NO3$site.ID <- factor(HI_FI_NO3$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_NO3.p = 
  ggplot(HI_FI_NO3, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_NO3.p

# NO3 #
HI_beta_NO3 = subset(HI_FI_beta, response_var == "NO3")
HI_beta_NO3$site.ID <- factor(HI_beta_NO3$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_NO3.p = 
  ggplot(HI_beta_NO3, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_NO3.p

# fDOM #
HI_FI_fDOM = subset(HI_FI_beta, response_var == "fDOM")
HI_FI_fDOM$site.ID <- factor(HI_FI_fDOM$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_fDOM.p = 
  ggplot(HI_FI_fDOM, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("b) fDOM")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_fDOM.p

HI_beta_fDOM = subset(HI_FI_beta, response_var == "fDOM")
HI_beta_fDOM$site.ID <- factor(HI_beta_fDOM$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_fDOM.p = 
  ggplot(HI_beta_fDOM, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("b) fDOM-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_fDOM.p

# SPC#
HI_FI_SPC = subset(HI_FI_beta, response_var == "SPC")
HI_FI_SPC$site.ID <- factor(HI_FI_SPC$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_SPC.p = 
  ggplot(HI_FI_SPC, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("c) SPC")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_SPC.p


HI_beta_SPC = subset(HI_FI_beta, response_var == "SPC")
HI_beta_SPC$site.ID <- factor(HI_beta_SPC$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_SPC.p = 
  ggplot(HI_beta_SPC, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("c) SPC")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_SPC.p

# turb #
HI_FI_turb = subset(HI_FI_beta, response_var == "turb")
HI_FI_turb$site.ID <- factor(HI_FI_turb$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_turb.p = 
  ggplot(HI_FI_turb, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("d) Turb")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_turb.p


HI_beta_turb = subset(HI_FI_beta, response_var == "turb")
HI_beta_turb$site.ID <- factor(HI_beta_turb$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_turb.p = 
  ggplot(HI_beta_turb, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("d) turb")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_turb.p

grid.arrange(HI_beta_NO3.p, HI_beta_fDOM.p, HI_beta_SPC.p, HI_beta_turb.p)

#### Regression between beta and FI #######
FI_beta_comp = 
ggplot(HI_FI_NO3, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp

FI_beta_comp_fDOM = 
  ggplot(HI_FI_fDOM, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp_fDOM

FI_beta_comp_SPC = 
  ggplot(HI_FI_SPC, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp_SPC

FI_beta_comp_turb = 
  ggplot(HI_FI_turb, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp_turb


grid.arrange(FI_beta_comp_NO3, FI_beta_comp_fDOM, FI_beta_comp_SPC, FI_beta_comp_turb)















# Haven't done this yet as of 1/19/22...I need to go through all of the HI script again to update those
# HI values with updated Q plots
########################################## 2020 ##########################################################
storm_file_list_beta <- list.files(path="~/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("~/Documents/Storms/Storm_Events/2019/FRCH_MOOS_VAUL_POKE_STRT_CARI//", storm_file_list_beta, replacement = "")
storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

for(i in 1:length(storm_list_beta)){
  storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(storm_list_beta[[i]][["valuedatetime"]],
                                                       "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 


#  organize storm data by site and solute # 5 for each storm 
CARI_storm_list_beta = storm_list_beta[c(1:60)] #60
FRCH_storm_list_beta = storm_list_beta[c(61:165)] #105
MOOS_storm_list_beta = storm_list_beta[c(166:230)] #60
POKE_storm_list_beta = storm_list_beta[c(231:295)]# 65
STRT_storm_list_beta = storm_list_beta[c(296:350)] #55
VAUL_storm_list_beta = storm_list_beta[c(351:410)] #65

CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("Turb", names(CARI_storm_list_beta)))]
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("Turb", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("Turb", names(MOOS_storm_list_beta)))]
MOOS_Q_storm_list_beta = MOOS_storm_list_beta[c(grep("Q", names(MOOS_storm_list_beta)))]

POKE_NO3_storm_list_beta = POKE_storm_list_beta[c(grep("NO3", names(POKE_storm_list_beta)))]
POKE_fDOM_storm_list_beta = POKE_storm_list_beta[c(grep("fDOM", names(POKE_storm_list_beta)))]
POKE_SpCond_storm_list_beta = POKE_storm_list_beta[c(grep("SPC", names(POKE_storm_list_beta)))]
POKE_turb_storm_list_beta = POKE_storm_list_beta[c(grep("Turb", names(POKE_storm_list_beta)))]
POKE_Q_storm_list_beta = POKE_storm_list_beta[c(grep("Q", names(POKE_storm_list_beta)))]

STRT_NO3_storm_list_beta = STRT_storm_list_beta[c(grep("NO3", names(STRT_storm_list_beta)))]
STRT_fDOM_storm_list_beta = STRT_storm_list_beta[c(grep("fDOM", names(STRT_storm_list_beta)))]
STRT_SpCond_storm_list_beta = STRT_storm_list_beta[c(grep("SPC", names(STRT_storm_list_beta)))]
STRT_turb_storm_list_beta = STRT_storm_list_beta[c(grep("Turb", names(STRT_storm_list_beta)))]
STRT_Q_storm_list_beta = STRT_storm_list_beta[c(grep("Q", names(STRT_storm_list_beta)))]

VAUL_NO3_storm_list_beta = VAUL_storm_list_beta[c(grep("NO3", names(VAUL_storm_list_beta)))]
VAUL_fDOM_storm_list_beta = VAUL_storm_list_beta[c(grep("fDOM", names(VAUL_storm_list_beta)))]
VAUL_SpCond_storm_list_beta = VAUL_storm_list_beta[c(grep("SPC", names(VAUL_storm_list_beta)))]
VAUL_turb_storm_list_beta = VAUL_storm_list_beta[c(grep("turb", names(VAUL_storm_list_beta)))]
VAUL_Q_storm_list_beta = VAUL_storm_list_beta[c(grep("Q", names(VAUL_storm_list_beta)))]

# normalize Q data 
# FRCH
for(i in 1:length(FRCH_Q_storm_list_beta)){
  FRCH_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_Q_storm_list_beta[[i]][["datavalue"]]-min(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# MOOS
for(i in 1:length(MOOS_Q_storm_list_beta)){
  MOOS_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_Q_storm_list_beta[[i]][["datavalue"]]-min(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# POKE
for(i in 1:length(POKE_Q_storm_list_beta)){
  POKE_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_Q_storm_list_beta[[i]][["datavalue"]]-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# STRT
for(i in 1:length(STRT_Q_storm_list_beta)){
  STRT_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_Q_storm_list_beta[[i]][["datavalue"]]-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# VAUL
for(i in 1:length(VAUL_Q_storm_list_beta)){
  VAUL_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_Q_storm_list_beta[[i]][["datavalue"]]-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# CARI
for(i in 1:length(CARI_Q_storm_list_beta)){
  CARI_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_Q_storm_list_beta[[i]][["datavalue"]]-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# normalize solute data 
# 
#NO3
for(i in 1:length(FRCH_NO3_storm_list_beta)){
  FRCH_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list_beta[[i]][["datavalue"]]-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_NO3_storm_list_beta)){
  MOOS_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_NO3_storm_list_beta[[i]][["datavalue"]]-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_NO3_storm_list_beta)){
  POKE_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_NO3_storm_list_beta[[i]][["datavalue"]]-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_NO3_storm_list_beta)){
  STRT_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_NO3_storm_list_beta[[i]][["datavalue"]]-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_NO3_storm_list_beta)){
  VAUL_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_NO3_storm_list_beta[[i]][["datavalue"]]-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_NO3_storm_list_beta)){
  CARI_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_NO3_storm_list_beta[[i]][["datavalue"]]-min(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#fDOM
for(i in 1:length(FRCH_fDOM_storm_list_beta)){
  FRCH_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_fDOM_storm_list_beta[[i]][["datavalue"]]-min(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_fDOM_storm_list_beta)){
  MOOS_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_fDOM_storm_list_beta[[i]][["datavalue"]]-min(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_fDOM_storm_list_beta)){
  POKE_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_fDOM_storm_list_beta[[i]][["datavalue"]]-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_fDOM_storm_list_beta)){
  STRT_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_fDOM_storm_list_beta[[i]][["datavalue"]]-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_fDOM_storm_list_beta)){
  VAUL_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_fDOM_storm_list_beta[[i]][["datavalue"]]-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_fDOM_storm_list_beta)){
  CARI_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_fDOM_storm_list_beta[[i]][["datavalue"]]-min(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#SPC
for(i in 1:length(FRCH_SpCond_storm_list_beta)){
  FRCH_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_SpCond_storm_list_beta[[i]][["datavalue"]]-min(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_SpCond_storm_list_beta)){
  MOOS_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_SpCond_storm_list_beta[[i]][["datavalue"]]-min(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_SpCond_storm_list_beta)){
  POKE_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_SpCond_storm_list_beta[[i]][["datavalue"]]-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_SpCond_storm_list_beta)){
  STRT_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_SpCond_storm_list_beta[[i]][["datavalue"]]-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_SpCond_storm_list_beta)){
  VAUL_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_SpCond_storm_list_beta[[i]][["datavalue"]]-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_SpCond_storm_list_beta)){
  CARI_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_SpCond_storm_list_beta[[i]][["datavalue"]]-min(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#Turb
for(i in 1:length(FRCH_turb_storm_list_beta)){
  FRCH_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_turb_storm_list_beta[[i]][["datavalue"]]-min(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_turb_storm_list_beta)){
  MOOS_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_turb_storm_list_beta[[i]][["datavalue"]]-min(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_turb_storm_list_beta)){
  POKE_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_turb_storm_list_beta[[i]][["datavalue"]]-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_turb_storm_list_beta)){
  STRT_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_turb_storm_list_beta[[i]][["datavalue"]]-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_turb_storm_list_beta)){
  VAUL_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_turb_storm_list_beta[[i]][["datavalue"]]-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_turb_storm_list_beta[[i]][["datavalue"]]-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}
###### NO3  #######
FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
POKE_NO3_storm <- map2_df(POKE_Q_storm_list_beta, POKE_NO3_storm_list_beta, inner_join, by = "valuedatetime")
STRT_NO3_storm <- map2_df(STRT_Q_storm_list_beta, STRT_NO3_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_NO3_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 993),
                            rep("storm10a", 121),
                            rep("storm10b", 95),
                            rep("storm10c", 207),
                            rep("storm11", 479),
                            rep("storm12a", 183),
                            rep("storm12b", 67),
                            rep("storm12c", 511),
                            rep("storm12d", 99),
                            rep("storm12e", 127),
                            rep("storm13", 391),
                            rep("storm14", 631),
                            rep("storm2", 165),
                            rep("storm3", 201),
                            rep("storm4", 193),
                            rep("storm5", 229),
                            rep("storm6", 257),
                            rep("storm7", 133),
                            rep("storm8", 105),
                            rep("storm9a", 61),
                            rep("storm9b", 149))

names(FRCH_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
FRCH_NO3_storm$site.ID <- "FRCH"

cols <- c("NO3.norm","Q.norm")
FRCH_NO3_storm[cols] <- log(FRCH_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_NO3_storm <- FRCH_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_NO3_storm_ascending <- filter(FRCH_NO3_storm, limb == "ascending")

FRCH_NO3_storm_ascending <- FRCH_NO3_storm_ascending[is.finite(FRCH_NO3_storm_ascending$Q.norm) & is.finite(FRCH_NO3_storm_ascending$NO3.norm), ]

beta.all.no3 <- FRCH_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
MOOS_NO3_storm$site.ID <- "MOOS"

MOOS_NO3_storm[cols] <- log(MOOS_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_NO3_storm <- MOOS_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_NO3_storm_ascending <- filter(MOOS_NO3_storm, limb == "ascending")

MOOS_NO3_storm_ascending <- MOOS_NO3_storm_ascending[is.finite(MOOS_NO3_storm_ascending$Q.norm) & is.finite(MOOS_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.moos.with.all <- MOOS_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_NO3_storm$storm.num = c(rep("storm1", 103),
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
names(MOOS_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.num")
MOOS_NO3_storm$Site <- "MOOS"

MOOS_NO3_storm[cols] <- log(MOOS_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_NO3_storm <- MOOS_NO3_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_NO3_storm_ascending <- filter(MOOS_NO3_storm, limb == "ascending")

MOOS_NO3_storm_ascending <- MOOS_NO3_storm_ascending[is.finite(MOOS_NO3_storm_ascending$Q.norm) & is.finite(MOOS_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.moos.with.all <- MOOS_NO3_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_NO3_storm$storm.ID = c(rep("storm1", 638),
                            rep("storm2", 274),
                            rep("storm3a", 1035),
                            rep("storm3b", 286),
                            rep("storm3c", 174),
                            rep("storm4", 466),
                            rep("storm5", 98),
                            rep("storm6", 246),
                            rep("storm7", 218),
                            rep("storm7b", 266),
                            rep("storm7c", 258))

names(STRT_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
STRT_NO3_storm$site.ID <- "STRT"

STRT_NO3_storm[cols] <- log(STRT_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_NO3_storm <- STRT_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_NO3_storm_ascending <- filter(STRT_NO3_storm, limb == "ascending")

STRT_NO3_storm_ascending <- STRT_NO3_storm_ascending[is.finite(STRT_NO3_storm_ascending$Q.norm) & is.finite(STRT_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.strt <- STRT_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_NO3_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_NO3_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_NO3_storm_ascending$DateTime <- as.POSIXct(STRT_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_NO3_storm_ascending$DateTime <- as.POSIXct(VAUL_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending, 
                       STRT_NO3_storm_ascending, VAUL_NO3_storm_ascending )

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
POKE_fDOM_storm <- map2_df(POKE_Q_storm_list_beta, POKE_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
STRT_fDOM_storm <- map2_df(STRT_Q_storm_list_beta, STRT_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_fDOM_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 993),
                             rep("storm10a", 121),
                             rep("storm10b", 95),
                             rep("storm10c", 207),
                             rep("storm11", 479),
                             rep("storm12a", 183),
                             rep("storm12b", 67),
                             rep("storm12c", 511),
                             rep("storm12d", 99),
                             rep("storm12e", 127),
                             rep("storm13", 391),
                             rep("storm14", 631),
                             rep("storm2", 165),
                             rep("storm3", 201),
                             rep("storm4", 193),
                             rep("storm5", 229),
                             rep("storm6", 257),
                             rep("storm7", 133),
                             rep("storm8", 105),
                             rep("storm9a", 61),
                             rep("storm9b", 149))

names(FRCH_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
FRCH_fDOM_storm$site.ID <- "FRCH"

cols <- c("fDOM.norm","Q.norm")
FRCH_fDOM_storm[cols] <- log(FRCH_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_fDOM_storm <- FRCH_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_fDOM_storm_ascending <- filter(FRCH_fDOM_storm, limb == "ascending")

FRCH_fDOM_storm_ascending <- FRCH_fDOM_storm_ascending[is.finite(FRCH_fDOM_storm_ascending$Q.norm) & is.finite(FRCH_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM <- FRCH_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
MOOS_fDOM_storm$site.ID <- "MOOS"

MOOS_fDOM_storm[cols] <- log(MOOS_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_fDOM_storm <- MOOS_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_fDOM_storm_ascending <- filter(MOOS_fDOM_storm, limb == "ascending")

MOOS_fDOM_storm_ascending <- MOOS_fDOM_storm_ascending[is.finite(MOOS_fDOM_storm_ascending$Q.norm) & is.finite(MOOS_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.moos.with.all <- MOOS_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_fDOM_storm$storm.num = c(rep("storm1", 103),
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
names(POKE_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", ".norm", "storm.num")
POKE_fDOM_storm$Site <- "POKE"

POKE_fDOM_storm[cols] <- log(POKE_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_fDOM_storm <- POKE_fDOM_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_fDOM_storm_ascending <- filter(POKE_fDOM_storm, limb == "ascending")

POKE_fDOM_storm_ascending <- POKE_fDOM_storm_ascending[is.finite(POKE_fDOM_storm_ascending$Q.norm) & is.finite(POKE_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.pok <- POKE_fDOM_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_fDOM_storm$storm.ID = c(rep("storm1", 638),
                             rep("storm2", 274),
                             rep("storm3a", 1035),
                             rep("storm3b", 286),
                             rep("storm3c", 174),
                             rep("storm4", 466),
                             rep("storm5", 98),
                             rep("storm6", 246),
                             rep("storm7", 218),
                             rep("storm7b", 266),
                             rep("storm7c", 258))

names(STRT_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
STRT_fDOM_storm$site.ID <- "STRT"

STRT_fDOM_storm[cols] <- log(STRT_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_fDOM_storm <- STRT_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_fDOM_storm_ascending <- filter(STRT_fDOM_storm, limb == "ascending")

STRT_fDOM_storm_ascending <- STRT_fDOM_storm_ascending[is.finite(STRT_fDOM_storm_ascending$Q.norm) & is.finite(STRT_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.strt <- STRT_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_fDOM_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
VAUL_fDOM_storm$site.ID <- "VAUL"

VAUL_fDOM_storm[cols] <- log(VAUL_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_fDOM_storm <- VAUL_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_fDOM_storm_ascending <- filter(VAUL_fDOM_storm, limb == "ascending")

VAUL_fDOM_storm_ascending <- VAUL_fDOM_storm_ascending[is.finite(VAUL_fDOM_storm_ascending$Q.norm) & is.finite(VAUL_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.vaul <- VAUL_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_fDOM_storm_ascending$DateTime <- as.POSIXct(STRT_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_fDOM_storm_ascending$DateTime <- as.POSIXct(VAUL_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending, 
                        STRT_fDOM_storm_ascending, VAUL_fDOM_storm_ascending )

beta.all.fdom <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fdom$response_var <- "fDOM"

##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
POKE_SPC_storm <- map2_df(POKE_Q_storm_list_beta, POKE_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
STRT_SPC_storm <- map2_df(STRT_Q_storm_list_beta, STRT_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_SPC_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm1", 993),
                            rep("storm10a", 121),
                            rep("storm10b", 95),
                            rep("storm10c", 207),
                            rep("storm11", 479),
                            rep("storm12a", 183),
                            rep("storm12b", 67),
                            rep("storm12c", 511),
                            rep("storm12d", 99),
                            rep("storm12e", 127),
                            rep("storm13", 391),
                            rep("storm14", 631),
                            rep("storm2", 165),
                            rep("storm3", 201),
                            rep("storm4", 193),
                            rep("storm5", 229),
                            rep("storm6", 257),
                            rep("storm7", 133),
                            rep("storm8", 105),
                            rep("storm9a", 61),
                            rep("storm9b", 149))

names(FRCH_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
FRCH_SPC_storm$site.ID <- "FRCH"

cols <- c("SPC.norm","Q.norm")
FRCH_SPC_storm[cols] <- log(FRCH_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_SPC_storm <- FRCH_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_SPC_storm_ascending <- filter(FRCH_SPC_storm, limb == "ascending")

FRCH_SPC_storm_ascending <- FRCH_SPC_storm_ascending[is.finite(FRCH_SPC_storm_ascending$Q.norm) & is.finite(FRCH_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC <- FRCH_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_SPC_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
MOOS_SPC_storm$site.ID <- "MOOS"

MOOS_SPC_storm[cols] <- log(MOOS_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_SPC_storm <- MOOS_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_SPC_storm_ascending <- filter(MOOS_SPC_storm, limb == "ascending")

MOOS_SPC_storm_ascending <- MOOS_SPC_storm_ascending[is.finite(MOOS_SPC_storm_ascending$Q.norm) & is.finite(MOOS_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.moos.with.all <- MOOS_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_SPC_storm$storm.num = c(rep("storm1", 103),
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
names(POKE_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.num")
POKE_SPC_storm$Site <- "POKE"

POKE_SPC_storm[cols] <- log(POKE_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_SPC_storm <- POKE_SPC_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_SPC_storm_ascending <- filter(POKE_SPC_storm, limb == "ascending")

POKE_SPC_storm_ascending <- POKE_SPC_storm_ascending[is.finite(POKE_SPC_storm_ascending$Q.norm) & is.finite(POKE_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.pok <- POKE_SPC_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_SPC_storm$storm.ID = c(rep("storm1", 638),
                            rep("storm2", 274),
                            rep("storm3a", 1035),
                            rep("storm3b", 286),
                            rep("storm3c", 174),
                            rep("storm4", 466),
                            rep("storm5", 98),
                            rep("storm6", 246),
                            rep("storm7", 218),
                            rep("storm7b", 266),
                            rep("storm7c", 258))

names(STRT_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
STRT_SPC_storm$site.ID <- "STRT"

STRT_SPC_storm[cols] <- log(STRT_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_SPC_storm <- STRT_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_SPC_storm_ascending <- filter(STRT_SPC_storm, limb == "ascending")

STRT_SPC_storm_ascending <- STRT_SPC_storm_ascending[is.finite(STRT_SPC_storm_ascending$Q.norm) & is.finite(STRT_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.strt <- STRT_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_SPC_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
VAUL_SPC_storm$site.ID <- "VAUL"

VAUL_SPC_storm[cols] <- log(VAUL_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_SPC_storm <- VAUL_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_SPC_storm_ascending <- filter(VAUL_SPC_storm, limb == "ascending")

VAUL_SPC_storm_ascending <- VAUL_SPC_storm_ascending[is.finite(VAUL_SPC_storm_ascending$Q.norm) & is.finite(VAUL_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.vaul <- VAUL_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_SPC_storm_ascending$DateTime <- as.POSIXct(STRT_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_SPC_storm_ascending$DateTime <- as.POSIXct(VAUL_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending, 
                       STRT_SPC_storm_ascending, VAUL_SPC_storm_ascending )

beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")
POKE_turb_storm <- map2_df(POKE_Q_storm_list_beta, POKE_turb_storm_list_beta, inner_join, by = "valuedatetime")
STRT_turb_storm <- map2_df(STRT_Q_storm_list_beta, STRT_turb_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_turb_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm1", 993),
                             rep("storm10a", 121),
                             rep("storm10b", 95),
                             rep("storm10c", 207),
                             rep("storm11", 479),
                             rep("storm12a", 183),
                             rep("storm12b", 67),
                             rep("storm12c", 511),
                             rep("storm12d", 99),
                             rep("storm12e", 127),
                             rep("storm13", 391),
                             rep("storm14", 631),
                             rep("storm2", 165),
                             rep("storm3", 201),
                             rep("storm4", 193),
                             rep("storm5", 229),
                             rep("storm6", 257),
                             rep("storm7", 133),
                             rep("storm8", 105),
                             rep("storm9a", 61),
                             rep("storm9b", 149))

names(FRCH_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
FRCH_turb_storm$site.ID <- "FRCH"

cols <- c("turb.norm","Q.norm")
FRCH_turb_storm[cols] <- log(FRCH_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_turb_storm <- FRCH_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_turb_storm_ascending <- filter(FRCH_turb_storm, limb == "ascending")

FRCH_turb_storm_ascending <- FRCH_turb_storm_ascending[is.finite(FRCH_turb_storm_ascending$Q.norm) & is.finite(FRCH_turb_storm_ascending$turb.norm), ]

beta.all.turb <- FRCH_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_turb_storm$storm.ID = c(rep("storm1", 702),
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
names(MOOS_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
MOOS_turb_storm$site.ID <- "MOOS"

MOOS_turb_storm[cols] <- log(MOOS_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_turb_storm <- MOOS_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_turb_storm_ascending <- filter(MOOS_turb_storm, limb == "ascending")

MOOS_turb_storm_ascending <- MOOS_turb_storm_ascending[is.finite(MOOS_turb_storm_ascending$Q.norm) & is.finite(MOOS_turb_storm_ascending$turb.norm), ]

beta.all.turb.moos.with.all <- MOOS_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_turb_storm$storm.num = c(rep("storm1", 103),
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
names(POKE_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.num")
POKE_turb_storm$Site <- "POKE"

POKE_turb_storm[cols] <- log(POKE_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_turb_storm <- POKE_turb_storm %>% group_by(storm.num) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_turb_storm_ascending <- filter(POKE_turb_storm, limb == "ascending")

POKE_turb_storm_ascending <- POKE_turb_storm_ascending[is.finite(POKE_turb_storm_ascending$Q.norm) & is.finite(POKE_turb_storm_ascending$turb.norm), ]

beta.all.turb.poke <- POKE_turb_storm_ascending %>% group_by(storm.num) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_turb_storm$storm.ID = c(rep("storm1", 638),
                             rep("storm2", 274),
                             rep("storm3a", 1035),
                             rep("storm3b", 286),
                             rep("storm3c", 174),
                             rep("storm4", 466),
                             rep("storm5", 98),
                             rep("storm6", 246),
                             rep("storm7", 218),
                             rep("storm7b", 266),
                             rep("storm7c", 258))

names(STRT_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
STRT_turb_storm$site.ID <- "STRT"

STRT_turb_storm[cols] <- log(STRT_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_turb_storm <- STRT_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_turb_storm_ascending <- filter(STRT_turb_storm, limb == "ascending")

STRT_turb_storm_ascending <- STRT_turb_storm_ascending[is.finite(STRT_turb_storm_ascending$Q.norm) & is.finite(STRT_turb_storm_ascending$turb.norm), ]

beta.all.turb.strt <- STRT_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_turb_storm$storm.ID = c(rep("storm1", 191),
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

names(VAUL_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
VAUL_turb_storm$site.ID <- "VAUL"

VAUL_turb_storm[cols] <- log(VAUL_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_turb_storm <- VAUL_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_turb_storm_ascending <- filter(VAUL_turb_storm, limb == "ascending")

VAUL_turb_storm_ascending <- VAUL_turb_storm_ascending[is.finite(VAUL_turb_storm_ascending$Q.norm) & is.finite(VAUL_turb_storm_ascending$turb.norm), ]

beta.all.turb.vaul <- VAUL_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# ALL # 
STRT_turb_storm_ascending$DateTime <- as.POSIXct(STRT_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_turb_storm_ascending$DateTime <- as.POSIXct(VAUL_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_turb_storm <- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending, 
                        STRT_turb_storm_ascending, VAUL_turb_storm_ascending )

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

beta.all.2019 <- rbind(beta.all.no3, beta.all.fdom,
                       beta.all.SPC, beta.all.turb)

#### plot ####
HI_FI <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results.csv")

HI_FI_beta = left_join(HI_FI, beta.all.2019, by=c("site.ID", "storm.ID", "response_var"))

write.csv(HI_FI_beta, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI_beta.diff_results.csv")

# NO3 #
HI_FI_NO3 = subset(HI_FI_beta, response_var == "NO3")
HI_FI_NO3$site.ID <- factor(HI_FI_NO3$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_NO3.p = 
  ggplot(HI_FI_NO3, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_NO3.p

# NO3 #
HI_beta_NO3 = subset(HI_FI_beta, response_var == "NO3")
HI_beta_NO3$site.ID <- factor(HI_beta_NO3$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_NO3.p = 
  ggplot(HI_beta_NO3, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_NO3.p

# fDOM #
HI_FI_fDOM = subset(HI_FI_beta, response_var == "fDOM")
HI_FI_fDOM$site.ID <- factor(HI_FI_fDOM$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_fDOM.p = 
  ggplot(HI_FI_fDOM, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("b) fDOM")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_fDOM.p

HI_beta_fDOM = subset(HI_FI_beta, response_var == "fDOM")
HI_beta_fDOM$site.ID <- factor(HI_beta_fDOM$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_fDOM.p = 
  ggplot(HI_beta_fDOM, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("b) fDOM-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_fDOM.p

# SPC#
HI_FI_SPC = subset(HI_FI_beta, response_var == "SPC")
HI_FI_SPC$site.ID <- factor(HI_FI_SPC$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_SPC.p = 
  ggplot(HI_FI_SPC, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("c) SPC")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_SPC.p


HI_beta_SPC = subset(HI_FI_beta, response_var == "SPC")
HI_beta_SPC$site.ID <- factor(HI_beta_SPC$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_SPC.p = 
  ggplot(HI_beta_SPC, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("c) SPC")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_SPC.p

# turb #
HI_FI_turb = subset(HI_FI_beta, response_var == "turb")
HI_FI_turb$site.ID <- factor(HI_FI_turb$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_FI_turb.p = 
  ggplot(HI_FI_turb, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("d) Turb")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_turb.p


HI_beta_turb = subset(HI_FI_beta, response_var == "turb")
HI_beta_turb$site.ID <- factor(HI_beta_turb$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))

HI_beta_turb.p = 
  ggplot(HI_beta_turb, aes(beta, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("d) turb")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
HI_beta_turb.p

grid.arrange(HI_beta_NO3.p, HI_beta_fDOM.p, HI_beta_SPC.p, HI_beta_turb.p)

#### Regression between beta and FI #######
FI_beta_comp = 
  ggplot(HI_FI_NO3, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp

FI_beta_comp_fDOM = 
  ggplot(HI_FI_fDOM, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp_fDOM

FI_beta_comp_SPC = 
  ggplot(HI_FI_SPC, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp_SPC

FI_beta_comp_turb = 
  ggplot(HI_FI_turb, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
  geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
  stat_poly_eq(formula = y~x,
               label.y = "top", label.x = "right",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
FI_beta_comp_turb


grid.arrange(FI_beta_comp_NO3, FI_beta_comp_fDOM, FI_beta_comp_SPC, FI_beta_comp_turb)













