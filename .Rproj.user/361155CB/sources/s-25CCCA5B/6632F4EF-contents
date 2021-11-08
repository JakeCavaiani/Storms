# press Command+Option+O to collapse all sections and get an overview of the workflow! #



############################## 2021 #############################################
# Load dataset for nitrate analysis: 
no3.daily = read.csv("~/Documents/DoD_Discharge/Discharge_Chem/2021/no3.2021.daily.csv")
no3.daily$DateTime <- ymd_hms(no3.daily$DateTime, tz = "America/Anchorage")

STRT.st <- read_csv("~/Documents/DoD_2021/RainGauge/STRT.RainGauge.2021.csv")
attributes(STRT.st$DateTime)$tzone <- 'America/Anchorage'

no3.daily$DateTime <- lubridate::round_date(no3.daily$DateTime, "15 minutes") 


no3.daily.1 <- left_join(no3.daily, STRT.st, by = "DateTime")
#### READ ME ####

# The purpose of this script is to explain variation in daily averaged (to remove diel seasonality) DOD NO3 data using covariates in multivariate state space models. fDOM analysis will be in a separate script. This script reduces the number of variables considered from "19_noR_CPCRW_2017" to reduce the "fishing expedition" nature of the analysis, and does not test every combination. It does, however, still test for collineariry. 

### Response Variables: ###
# "nitrate_uM_filled_mean"                  

### Explanatory Variables: ###  
### storms:   **** NEED TO DECIDE BETWEEN THESE TWO ****
# "precip_mm_mean"                   [same for all catchments] 
# "precip_mm_mean" - lagged          [same for all catchments] 
### terrestrial veg:
# "GDD.local" - lagged
### stream biota:
# "PAR_modeled_Wm2_max"  
# "GPP_daily_mean"
# "ER_daily_mean"
### other:
# "step"                             [same for all catchments]


#### libraries ####
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(forecast)
library(zoo)
library(xts)
library(imputeTS)
library(Hmisc)
library(data.table)
library(gridExtra)
library(bbmle)

############################# FRCH-no3 06-08:08-20 NO METAB ############################# 
#### data wrangling - covars ####

## subset daily data by site and date range ##
FRCH = subset(no3.daily.1, Site.x == "FRCH")
## order ##
FRCH = dplyr::arrange(FRCH, DateTime)

# add covs lagged by one day
#C2$Discharge_Lsec_mean_lag = c(NA, C2$Discharge_Lsec_mean[-nrow(C2)])
C2$precip_mm_mean_lag = c(NA, C2$precip_mm_mean[-nrow(C2)])
C2$GDD_lag = c(NA, C2$GDD.local[-nrow(C2)])
# remove first row where NAs were introduced by lagging covars
C2 = C2[-1,]

# select covs
cov <- C2 %>% select("precip_mm_mean", 
                     "precip_mm_mean_lag",
                     "GDD_lag",
                     "PAR_modeled_Wm2_max", 
                     "step")

# check for nas
any(is.na(cov))

## generate all non-collinear cov lists ##
covNames = colnames(cov)
b = sapply(1:length(covNames), function(i) (combn(covNames, i, simplify = F)))
c <- unlist(b,recursive=FALSE)

# choose lists with at least n variables:
to.remove <- sapply(c, function(c) length(c) < 5)
o <- c[!to.remove]

## check lists for collinearity ##
colin = list()
for(i in 1:length(o)){
  cov <- C2 %>% select(o[[i]])
  colin[[i]] = any(abs(cor(cov))>0.5 & abs(cor(cov))<1)
}
#colin

## remove collinear sets
colin = which(unlist(colin))
for(i in colin){
  o[[i]] = NA
}
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
o=na.omit.list(o)

#### data wrangling - no3 ####

## center response var ##
dat = as.vector(C2$nitrate_uM_filled_mean)
dat <- scale(dat, center=TRUE, scale=F)
## check for NAs ##
any(is.na((dat)))
## plot ##
plot(dat, type="b")
## transpose for MARSS ##
yy <- t(matrix(dat))

#### set MARSS model parms  ####

### inputs to process model ###
# i.e., the model that estimates the hidden random walk from the data
BB = "unconstrained" # allow for and estimate mean reversion
UU = "zero" # do NOT allow a drift to term for the random walk to be estimated
## cc ##
#cc <- t(scale(cov)) # de-mean and scale covariates to units of sd. I do this in the for loop for each combination of variables, so no need to do it here
## CC ##
CC <- "unconstrained" # allow for and estimate the covariate effects
## QQ ##
QQ = "unconstrained" # allow for and estimate the covariance matrix of process errors

### inputs to observtion model ###
# i.e., the model that estimates the response variable
## ZZ ##
ZZ='identity' # (number of estimated state processes) = 1
## AA ##
AA="zero"
## DD ##
DD="zero" # matrix of coefficients to be estimated for covariates in an observation model. 
## dd ##
dd="zero" # these is the covariate matrix in the observation model.
## RR ##
RR="zero"

### initial conditions ###
x0 = matrix("x0") # mean of the initial condition - allow MARSS to estimate this
V0="zero" # covariance of the initial condition, setting to zero makes x0 a fixed parameter
tinitx=0  # start model at t0

#### loop: run non-collinear cov lists ####

mod.output <- list()
for(i in 1:length(o)){
  cov <- C2 %>% select(o[[i]])
  cc <- t(scale(cov))
  #MARSS function call, can change number of iterations if desired
  mod <- MARSS(y=yy, 
               model=list(
                 B=BB, 
                 U=UU,
                 C=CC,
                 c=cc,
                 Q=QQ,
                 Z=ZZ, 
                 A=AA, 
                 D=DD,
                 d=DD,
                 R=RR,
                 x0=x0, 
                 V0=V0,
                 tinitx=tinitx), 
               control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
  # put MARSS output into a list
  mod.output[[i]] <- mod 
}

#### loop: ID and remove models with autocorrelation in residuals ####

## ID
nac = list()
for(i in 1:(length(mod.output))){
  resids <- residuals(mod.output[[i]])
  #acf.mod = Acf(resids$model.residuals[1,], main="model residuals",lag.max=10,plot=F) # with R
  acf.state = Acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass,lag.max=10,plot=F)
  #nac[[i]] = any(c((abs(acf.mod$acf[2:10])>0.225), (abs(acf.state$acf[2:10])>0.225))) # with R
  nac[[i]] = any((abs(acf.state$acf[2:10])>0.25)) # no R
}

## remove
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
nac = which(unlist(nac))
mod.output.nac = mod.output
for(i in nac){
  mod.output.nac[[i]] = NA
}
mod.output.nac=na.omit.list(mod.output.nac)

## plot acf if there are concerns
acf.1 = Acf((residuals(mod.output[[1]]))$state.residuals[1,], 
            main="state residuals", lag.max=10, ci.type="white",ci.col = "red")


# the model has significant acf but i am going to keep it for Mark to consider for now
mod.output.nac = mod.output

#### loop: save coef plots and coef estimates of no autocorrelation models ####

est.output.nac <- list()
plot.output.nac <-list()

# check date range. Update plot titles and file paths in for loop with this if necessary.
range(C2$day)

i=1
# estimates
est = MARSSparamCIs(mod.output.nac[[i]], method = "parametric", alpha = 0.05, nboot = 1000, silent=F)
est.output.nac[[i]] <- est 
# plot estimates
CIs = cbind(
  est.output.nac[[i]]$par$U,
  est.output.nac[[i]]$par.lowCI$U,
  est.output.nac[[i]]$par.upCI$U)
CIs = as.data.frame(CIs)
names(CIs) = c("Est.", "Lower", "Upper")
CIs$parm = rownames(CIs)
m.p = ggplot(CIs, aes(parm, Est.)) + 
  geom_point(position=position_dodge(width=0.3), size=2) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.3), width=0.2) + 
  theme_bw(base_size=12)+ 
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_flip()+ ggtitle(paste("C2 06-08:08-20 NO3", i))
plot.output.nac[[i]] <- m.p 
ggsave(m.p, filename = paste("Output from analyses/32_noR_CPCRW_2017_C2.NO3_mods/0608-0820/mod.plot.", i, ".pdf",sep=""), width=6, height=6, units = "in")

## save plot in this R environment ###

C2_NO3_nometab = 
  ggplot(CIs, aes(parm, Est.)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.5), width=0.5) +
  geom_point(position=position_dodge(width=0.3), size=10, color="#CA0020") + 
  theme_bw(base_size=25)+ 
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_flip()+ ggtitle("NO3- in < 4% PF (C2)\n June 08 - Aug 20")+ 
  scale_x_discrete(name ="", 
                   limits=c("(X.Y1,PAR_modeled_Wm2_max)",
                            "(X.Y1,GDD_lag)",
                            "(X.Y1,precip_mm_mean_lag)",
                            "(X.Y1,precip_mm_mean)",
                            "(X.Y1,step)"),
                   labels = c("Max PAR",
                              "GDU lagged",
                              "Rain lagged",
                              "Rain",
                              "Step-change"))+
  ylab("Covariate Effects")
C2_NO3_nometab

#### model comparisons (AIC) ####

### run model with no covs ###
mod.null <- MARSS(y=yy, 
                  model=list(
                    B=BB, 
                    U=UU,
                    Q=QQ,
                    Z=ZZ, 
                    A=AA, 
                    D=DD,
                    d=DD,
                    R=RR,
                    x0=x0, 
                    V0=V0,
                    tinitx=tinitx), 
                  control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
## save null model to mod.output.nac list
mod.output.nac[[(length(mod.output.nac)+1)]] = mod.null
## name models in list
names(mod.output.nac) = c(seq(1:(length(mod.output.nac)-1)), "mod.null")

### identify models that did not converge ###
errors = list()
for(i in 1:length(mod.output.nac)){
  errors[[i]] = mod.output.nac[[i]][["errors"]]
}
errors

#### loop: plot and save all model diagnostics ####

dat = yy
time = C2$day

for(i in 1:(length(mod.output.nac)-1)){
  pdf(paste("Output from analyses/32_noR_CPCRW_2017_C2.NO3_mods/0608-0820/model diagnostics/diagnostics.mod.", i, ".pdf",sep=""), width = 10, height =8)
  
  ### Do resids have temporal autocorrelation? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #acf(resids$model.residuals[1,], main="model residuals")
  acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass, lag.max = 10)
  mtext("Do resids have temporal autocorrelation?", outer = TRUE, cex = 1.5)
  
  ### Fitted values over observations ###
  par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
  ## stocastic model with drift
  kf=print(mod.output.nac[[i]], what="kfs") # Kalman filter and smoother output
  # prediction conditioned on all data.
  plot(as.vector(dat) ~ as.POSIXct(time), type="o", 
       main = "fitted values conditioned on all y")
  lines(as.vector(kf$xtT) ~ as.POSIXct(time), col="blue",lwd=2) 
  lines(as.POSIXct(time),mod.output.nac[[i]]$states-1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  lines(as.POSIXct(time),mod.output.nac[[i]]$states+1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
  
  ### Do resids have temporal trend? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #plot(resids$model.residuals[1,], ylab="model residual", xlab="", main="model residuals")
  abline(h=0)
  plot(resids$state.residuals[1,], ylab="state residual", xlab="", main="state residuals")
  abline(h=0)
  mtext("Do resids have temporal trend?", outer = TRUE, cex = 1.5)
  
  ### Are resids normal? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  # qqnorm(resids$model.residuals[1,], main="model residuals", pch=16, 
  #        xlab=paste("shapiro test: ", shapiro.test(resids$model.residuals[1,])[1]))
  # qqline(resids$model.residuals[1,])
  qqnorm(resids$state.residuals[1,], main="state residuals", pch=16, 
         xlab=paste("shapiro test: ", shapiro.test(resids$state.residuals[1,])[1]))
  qqline(resids$state.residuals[1,])
  mtext("Are resids normal?", outer = TRUE, cex = 1.5)
  
  ### residuals vs fitted ###
  par(mfrow=c(2,1),oma = c(2, 0, 2, 0))
  # resids <- residuals(mod.output.nac[[i]])
  # scatter.smooth(as.vector(kf$xtT) ~ resids$model.residuals[1,],
  #                main = "model resids vs fitted (y conditioned)")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]), col="blue")
  scatter.smooth(as.vector(kf$xtT) ~ resids$state.residuals[1,],
                 main = "state resids vs fitted (y conditioned)")
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]), col="blue")
  mtext("Trends in fitted values vs residuals?", outer = TRUE, cex = 1.5)
  
  ### residuals vs observed ###
  # scatter.smooth(as.vector(dat) ~ resids$model.residuals[1,],
  #                main = "model resids vs observed")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]))
  scatter.smooth(as.vector(dat) ~ resids$state.residuals[1,], 
                 main = "state resids vs observed") 
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]))
  mtext("Trends in observed values vs residuals?", outer = TRUE, cex = 1.5, side=1)
  
  dev.off()
}

#### final model selection ####

AICtab(mod.output.nac[["mod.null"]],
       mod.output.nac[[1]])
# 
# dAIC df
# mod.output.nac[[1]]           0.0 8 
# mod.output.nac[["mod.null"]] 44.4 3 

grid.arrange(plot.output.nac[[1]])




############################# C2-no3 06-08:08-20 WITH METAB ############################# 
#### data wrangling - covars ####

## subset daily data by site and date range ##
C2 = subset(no3.daily, site.ID == "C2")
## order ##
C2 = dplyr::arrange(C2, day)
# remove entries with NAs in GPP and ER 
C2 = C2[!is.na(C2$GPP_daily_mean),]

# add covs lagged by one day
C2$precip_mm_mean_lag = c(NA, C2$precip_mm_mean[-nrow(C2)])
C2$GDD_lag = c(NA, C2$GDD.local[-nrow(C2)])
# remove first row where NAs were introduced by lagging covars
C2 = C2[-1,]

# select covs
cov <- C2 %>% select("precip_mm_mean", 
                     "precip_mm_mean_lag",
                     "GDD_lag",
                     "GPP_daily_mean", 
                     "ER_daily_mean",
                     "step")

# check for nas
any(is.na(cov))

## generate all non-collinear cov lists ##
covNames = colnames(cov)
b = sapply(1:length(covNames), function(i) (combn(covNames, i, simplify = F)))
c <- unlist(b,recursive=FALSE)

# choose lists with at least 7 variables:
to.remove <- sapply(c, function(c) length(c) < 6)
o <- c[!to.remove]

## check lists for collinearity ##
colin = list()
for(i in 1:length(o)){
  cov <- C2 %>% select(o[[i]])
  colin[[i]] = any(abs(cor(cov))>0.5 & abs(cor(cov))<1)
}
#colin

## remove collinear sets
colin = which(unlist(colin))
for(i in colin){
  o[[i]] = NA
}
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
o=na.omit.list(o)

#### data wrangling - no3 ####

## center response var ##
dat = as.vector(C2$nitrate_uM_filled_mean)
dat <- scale(dat, center=TRUE, scale=F)
## check for NAs ##
any(is.na((dat)))
## plot ##
plot(dat, type="b")
## transpose for MARSS ##
yy <- t(matrix(dat))

#### set MARSS model parms  ####

### inputs to process model ###
# i.e., the model that estimates the hidden random walk from the data
BB = "unconstrained" # allow for and estimate mean reversion
UU = "zero" # do NOT allow a drift to term for the random walk to be estimated
## cc ##
#cc <- t(scale(cov)) # de-mean and scale covariates to units of sd. I do this in the for loop for each combination of variables, so no need to do it here
## CC ##
CC <- "unconstrained" # allow for and estimate the covariate effects
## QQ ##
QQ = "unconstrained" # allow for and estimate the covariance matrix of process errors

### inputs to observtion model ###
# i.e., the model that estimates the response variable
## ZZ ##
ZZ='identity' # (number of estimated state processes) = 1
## AA ##
AA="zero"
## DD ##
DD="zero" # matrix of coefficients to be estimated for covariates in an observation model. 
## dd ##
dd="zero" # these is the covariate matrix in the observation model.
## RR ##
RR="zero"

### initial conditions ###
x0 = matrix("x0") # mean of the initial condition - allow MARSS to estimate this
V0="zero" # covariance of the initial condition, setting to zero makes x0 a fixed parameter
tinitx=0  # start model at t0

#### loop: run non-collinear cov lists ####

mod.output <- list()
for(i in 1:length(o)){
  cov <- C2 %>% select(o[[i]])
  cc <- t(scale(cov))
  #MARSS function call, can change number of iterations if desired
  mod <- MARSS(y=yy, 
               model=list(
                 B=BB, 
                 U=UU,
                 C=CC,
                 c=cc,
                 Q=QQ,
                 Z=ZZ, 
                 A=AA, 
                 D=DD,
                 d=DD,
                 R=RR,
                 x0=x0, 
                 V0=V0,
                 tinitx=tinitx), 
               control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
  # put MARSS output into a list
  mod.output[[i]] <- mod 
}

#### loop: ID and remove models with autocorrelation in residuals ####

## ID
nac = list()
for(i in 1:(length(mod.output))){
  resids <- residuals(mod.output[[i]])
  #acf.mod = Acf(resids$model.residuals[1,], main="model residuals",lag.max=10,plot=F) # with R
  acf.state = Acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass,lag.max=10,plot=F)
  #nac[[i]] = any(c((abs(acf.mod$acf[2:10])>0.225), (abs(acf.state$acf[2:10])>0.225))) # with R
  nac[[i]] = any((abs(acf.state$acf[2:10])>0.25)) # no R
}

## remove
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
nac = which(unlist(nac))
mod.output.nac = mod.output
for(i in nac){
  mod.output.nac[[i]] = NA
}
mod.output.nac=na.omit.list(mod.output.nac)

## plot acf if there are concerns
acf.1 = Acf((residuals(mod.output[[1]]))$state.residuals[1,], 
            main="state residuals", lag.max=10, ci.type="white",ci.col = "red")
acf.2 = Acf((residuals(mod.output[[2]]))$state.residuals[1,], 
            main="state residuals", lag.max=10, ci.type="white",ci.col = "red")
## better ACF plot (from NWFSC)
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "red")
  abline(h = 0)
}
plot.acf(acf.1)
plot.acf(acf.2)

# the model has significant acf but i am going to keep it for Mark to consider for now
mod.output.nac = mod.output

#### loop: save coef plots and coef estimates of no autocorrelation models ####

est.output.nac <- list()
plot.output.nac <-list()

# check date range. Update plot titles and file paths in for loop with this if necessary.
range(C2$day)

for(i in 1:length(mod.output.nac)){
  # estimates
  est = MARSSparamCIs(mod.output.nac[[i]], method = "hessian", alpha = 0.05, nboot = 1000)
  est.output.nac[[i]] <- est 
  # plot estimates
  CIs = cbind(
    est.output.nac[[i]]$par$U,
    est.output.nac[[i]]$par.lowCI$U,
    est.output.nac[[i]]$par.upCI$U)
  CIs = as.data.frame(CIs)
  names(CIs) = c("Est.", "Lower", "Upper")
  CIs$parm = rownames(CIs)
  m.p = ggplot(CIs, aes(parm, Est.)) + 
    geom_point(position=position_dodge(width=0.3), size=2) + 
    geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.3), width=0.2) + 
    theme_bw(base_size=12)+ 
    geom_hline(aes(yintercept=0), linetype="dashed")+
    coord_flip()+ ggtitle(paste("C2 06-08:08-20 NO3 metab", i))
  plot.output.nac[[i]] <- m.p 
  ggsave(m.p, filename = paste("Output from analyses/32_noR_CPCRW_2017_C2.NO3_mods/0608-0820_metab/mod.plot.", i, ".pdf",sep=""), width=6, height=6, units = "in")
}
## save plot in this R environment ###
i=1 #then run loop
C2_NO3_metab = 
  ggplot(CIs, aes(parm, Est.)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.5), width=0.5) +
  geom_point(position=position_dodge(width=0.3), size=10, color="#CA0020") + 
  theme_bw(base_size=25)+ 
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_flip()+ 
  ggtitle("NO3- in < 4% PF (C2)\n June 08 - Aug 20")+ 
  scale_x_discrete(name ="", 
                   limits=c("(X.Y1,ER_daily_mean)",
                            "(X.Y1,GPP_daily_mean)",
                            "(X.Y1,GDD_lag)",
                            "(X.Y1,precip_mm_mean_lag)",
                            "(X.Y1,precip_mm_mean)",
                            "(X.Y1,step)"),
                   labels = c("Stream ER",
                              "Stream GPP",
                              "GDU lagged",
                              "Rain lagged",
                              "Rain",
                              "Step-change"))+
  ylab("Covariate Effects")
#C2_NO3_nometab
C2_NO3_metab

#### model comparisons (AIC) ####

### run model with no covs ###
mod.null <- MARSS(y=yy, 
                  model=list(
                    B=BB, 
                    U=UU,
                    Q=QQ,
                    Z=ZZ, 
                    A=AA, 
                    D=DD,
                    d=DD,
                    R=RR,
                    x0=x0, 
                    V0=V0,
                    tinitx=tinitx), 
                  control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
## save null model to mod.output.nac list
mod.output.nac[[(length(mod.output.nac)+1)]] = mod.null
## name models in list
names(mod.output.nac) = c(seq(1:(length(mod.output.nac)-1)), "mod.null")

### identify models that did not converge ###
errors = list()
for(i in 1:length(mod.output.nac)){
  errors[[i]] = mod.output.nac[[i]][["errors"]]
}
errors

#### loop: plot and save all model diagnostics ####

dat = yy
time = C2$day

for(i in 1:(length(mod.output.nac)-1)){
  pdf(paste("Output from analyses/32_noR_CPCRW_2017_C2.NO3_mods/0608-0820_metab/model diagnostics/diagnostics.mod.", i, ".pdf",sep=""), width = 10, height =8)
  
  ### Do resids have temporal autocorrelation? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #acf(resids$model.residuals[1,], main="model residuals")
  Acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass, lag.max = 10)
  mtext("Do resids have temporal autocorrelation?", outer = TRUE, cex = 1.5)
  
  ### Fitted values over observations ###
  par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
  ## stocastic model with drift
  kf=print(mod.output.nac[[i]], what="kfs") # Kalman filter and smoother output
  # prediction conditioned on all data.
  plot(as.vector(dat) ~ as.POSIXct(time), type="o", 
       main = "fitted values conditioned on all y")
  lines(as.vector(kf$xtT) ~ as.POSIXct(time), col="blue",lwd=2) 
  lines(as.POSIXct(time),mod.output.nac[[i]]$states-1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  lines(as.POSIXct(time),mod.output.nac[[i]]$states+1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
  
  ### Do resids have temporal trend? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #plot(resids$model.residuals[1,], ylab="model residual", xlab="", main="model residuals")
  abline(h=0)
  plot(resids$state.residuals[1,], ylab="state residual", xlab="", main="state residuals")
  abline(h=0)
  mtext("Do resids have temporal trend?", outer = TRUE, cex = 1.5)
  
  ### Are resids normal? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  # qqnorm(resids$model.residuals[1,], main="model residuals", pch=16, 
  #        xlab=paste("shapiro test: ", shapiro.test(resids$model.residuals[1,])[1]))
  # qqline(resids$model.residuals[1,])
  qqnorm(resids$state.residuals[1,], main="state residuals", pch=16, 
         xlab=paste("shapiro test: ", shapiro.test(resids$state.residuals[1,])[1]))
  qqline(resids$state.residuals[1,])
  mtext("Are resids normal?", outer = TRUE, cex = 1.5)
  
  ### residuals vs fitted ###
  par(mfrow=c(2,1),oma = c(2, 0, 2, 0))
  # resids <- residuals(mod.output.nac[[i]])
  # scatter.smooth(as.vector(kf$xtT) ~ resids$model.residuals[1,],
  #                main = "model resids vs fitted (y conditioned)")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]), col="blue")
  scatter.smooth(as.vector(kf$xtT) ~ resids$state.residuals[1,],
                 main = "state resids vs fitted (y conditioned)")
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]), col="blue")
  mtext("Trends in fitted values vs residuals?", outer = TRUE, cex = 1.5)
  
  ### residuals vs observed ###
  # scatter.smooth(as.vector(dat) ~ resids$model.residuals[1,],
  #                main = "model resids vs observed")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]))
  scatter.smooth(as.vector(dat) ~ resids$state.residuals[1,], 
                 main = "state resids vs observed") 
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]))
  mtext("Trends in observed values vs residuals?", outer = TRUE, cex = 1.5, side=1)
  
  dev.off()
}

#### final model selection ####

AICtab(mod.output.nac[["mod.null"]],
       mod.output.nac[[1]])
# # 
# dAIC df
# mod.output.nac[[1]]           0   9 
# mod.output.nac[["mod.null"]] 41   3 

grid.arrange(plot.output.nac[[1]])




############################# C3-no3 06-07:08-20 NO METAB ############################# 
#### data wrangling - covars ####

## subset daily data by site and date range ##
C3 = subset(no3.daily, site.ID == "C3")
## order ##
C3 = dplyr::arrange(C3, day)
# remove entries with NAs in GPP and ER 
#C3 = C3[!is.na(C3$GPP_daily_mean),]

# add covs lagged by one day
C3$precip_mm_mean_lag = c(NA, C3$precip_mm_mean[-nrow(C3)])
C3$GDD_lag = c(NA, C3$GDD.local[-nrow(C3)])
# remove first row where NAs were introduced by lagging covars
C3 = C3[-1,]

# select covs
cov <- C3 %>% select("precip_mm_mean", 
                     "precip_mm_mean_lag",
                     "GDD_lag",
                     "PAR_modeled_Wm2_max", 
                     "step")

# check for nas
any(is.na(cov))

## generate all non-collinear cov lists ##
covNames = colnames(cov)
b = sapply(1:length(covNames), function(i) (combn(covNames, i, simplify = F)))
c <- unlist(b,recursive=FALSE)

# choose lists with at least 7 variables:
to.remove <- sapply(c, function(c) length(c) < 6)
o <- c[!to.remove]

## check lists for collinearity ##
colin = list()
for(i in 1:length(o)){
  cov <- C3 %>% select(o[[i]])
  colin[[i]] = any(abs(cor(cov))>0.5 & abs(cor(cov))<1)
}
#colin

## remove collinear sets
colin = which(unlist(colin))
for(i in colin){
  o[[i]] = NA
}
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
o=na.omit.list(o)

#### data wrangling - no3 ####

## center response var ##
dat = as.vector(C3$nitrate_uM_filled_mean)
dat <- scale(dat, center=TRUE, scale=F)
## check for NAs ##
any(is.na((dat)))
## plot ##
plot(dat, type="b")
## transpose for MARSS ##
yy <- t(matrix(dat))

#### set MARSS model parms  ####

### inputs to process model ###
# i.e., the model that estimates the hidden random walk from the data
BB = "unconstrained" # allow for and estimate mean reversion
UU = "zero" # do NOT allow a drift to term for the random walk to be estimated
## cc ##
#cc <- t(scale(cov)) # de-mean and scale covariates to units of sd. I do this in the for loop for each combination of variables, so no need to do it here
## CC ##
CC <- "unconstrained" # allow for and estimate the covariate effects
## QQ ##
QQ = "unconstrained" # allow for and estimate the covariance matrix of process errors

### inputs to observtion model ###
# i.e., the model that estimates the response variable
## ZZ ##
ZZ='identity' # (number of estimated state processes) = 1
## AA ##
AA="zero"
## DD ##
DD="zero" # matrix of coefficients to be estimated for covariates in an observation model. 
## dd ##
dd="zero" # these is the covariate matrix in the observation model.
## RR ##
RR="zero"

### initial conditions ###
x0 = matrix("x0") # mean of the initial condition - allow MARSS to estimate this
V0="zero" # covariance of the initial condition, setting to zero makes x0 a fixed parameter
tinitx=0  # start model at t0

#### loop: run non-collinear cov lists ####

mod.output <- list()
for(i in 1:length(o)){
  cov <- C3 %>% select(o[[i]])
  cc <- t(scale(cov))
  #MARSS function call, can change number of iterations if desired
  mod <- MARSS(y=yy, 
               model=list(
                 B=BB, 
                 U=UU,
                 C=CC,
                 c=cc,
                 Q=QQ,
                 Z=ZZ, 
                 A=AA, 
                 D=DD,
                 d=DD,
                 R=RR,
                 x0=x0, 
                 V0=V0,
                 tinitx=tinitx), 
               control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
  # put MARSS output into a list
  mod.output[[i]] <- mod 
}

#### loop: ID and remove models with autocorrelation in residuals ####

## ID
nac = list()
for(i in 1:(length(mod.output))){
  resids <- residuals(mod.output[[i]])
  #acf.mod = Acf(resids$model.residuals[1,], main="model residuals",lag.max=10,plot=F) # with R
  acf.state = Acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass,lag.max=10,plot=F)
  #nac[[i]] = any(c((abs(acf.mod$acf[2:10])>0.225), (abs(acf.state$acf[2:10])>0.225))) # with R
  nac[[i]] = any((abs(acf.state$acf[2:10])>0.25)) # no R
}

## remove
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
nac = which(unlist(nac))
mod.output.nac = mod.output
for(i in nac){
  mod.output.nac[[i]] = NA
}
mod.output.nac=na.omit.list(mod.output.nac)

## plot acf if there are concerns
acf.1 = Acf((residuals(mod.output.nac[[1]]))$state.residuals[1,], 
            main="state residuals", lag.max=10, ci.type="white",ci.col = "red")

#### loop: save coef plots and coef estimates of no autocorrelation models ####

est.output.nac <- list()
plot.output.nac <-list()

# check date range. Update plot titles and file paths in for loop with this if necessary.
range(C3$day)

for(i in 1:length(mod.output.nac)){
  # estimates
  est = MARSSparamCIs(mod.output.nac[[i]], method = "hessian", alpha = 0.05, nboot = 1000)
  est.output.nac[[i]] <- est 
  # plot estimates
  CIs = cbind(
    est.output.nac[[i]]$par$U,
    est.output.nac[[i]]$par.lowCI$U,
    est.output.nac[[i]]$par.upCI$U)
  CIs = as.data.frame(CIs)
  names(CIs) = c("Est.", "Lower", "Upper")
  CIs$parm = rownames(CIs)
  m.p = ggplot(CIs, aes(parm, Est.)) + 
    geom_point(position=position_dodge(width=0.3), size=2) + 
    geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.3), width=0.2) + 
    theme_bw(base_size=12)+ 
    geom_hline(aes(yintercept=0), linetype="dashed")+
    coord_flip()+ ggtitle(paste("C3 06-07:08-22 NO3", i))
  plot.output.nac[[i]] <- m.p 
  ggsave(m.p, filename = paste("Output from analyses/32_noR_CPCRW_2017_C3.NO3_mods/0607-0822/mod.plot.", i, ".pdf",sep=""), width=6, height=6, units = "in")
}
## save plot in this R environment ###
i=1 #then run loop
C3_NO3_nometab = 
  ggplot(CIs, aes(parm, Est.)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.5), width=0.5) +
  geom_point(position=position_dodge(width=0.3), size=10, color="#0571B0") + 
  theme_bw(base_size=25)+ 
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_flip()+ 
  ggtitle("NO3- in 54% PF (C3)\n June 07 - Aug 22")+ 
  scale_x_discrete(name ="", 
                   limits=c("(X.Y1,PAR_modeled_Wm2_max)",
                            "(X.Y1,GDD_lag)",
                            "(X.Y1,precip_mm_mean_lag)",
                            "(X.Y1,precip_mm_mean)",
                            "(X.Y1,step)"),
                   labels = c("Max PAR",
                              "GDU lagged",
                              "Rain lagged",
                              "Rain",
                              "Step-change"))+
  ylab("Covariate Effects")
C3_NO3_nometab


#### model comparisons (AIC) ####

### run model with no covs ###
mod.null <- MARSS(y=yy, 
                  model=list(
                    B=BB, 
                    U=UU,
                    Q=QQ,
                    Z=ZZ, 
                    A=AA, 
                    D=DD,
                    d=DD,
                    R=RR,
                    x0=x0, 
                    V0=V0,
                    tinitx=tinitx), 
                  control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
## save null model to mod.output.nac list
mod.output.nac[[(length(mod.output.nac)+1)]] = mod.null
## name models in list
names(mod.output.nac) = c(seq(1:(length(mod.output.nac)-1)), "mod.null")

### identify models that did not converge ###
errors = list()
for(i in 1:length(mod.output.nac)){
  errors[[i]] = mod.output.nac[[i]][["errors"]]
}
errors

#### loop: plot and save all model diagnostics ####

dat = yy
time = C3$day

for(i in 1:(length(mod.output.nac)-1)){
  pdf(paste("Output from analyses/32_noR_CPCRW_2017_C3.NO3_mods/0607-0822/model diagnostics/diagnostics.mod.", i, ".pdf",sep=""), width = 10, height =8)
  
  ### Do resids have temporal autocorrelation? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #acf(resids$model.residuals[1,], main="model residuals")
  Acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass)
  mtext("Do resids have temporal autocorrelation?", outer = TRUE, cex = 1.5)
  
  ### Fitted values over observations ###
  par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
  ## stocastic model with drift
  kf=print(mod.output.nac[[i]], what="kfs") # Kalman filter and smoother output
  # prediction conditioned on all data.
  plot(as.vector(dat) ~ as.POSIXct(time), type="o", 
       main = "fitted values conditioned on all y")
  lines(as.vector(kf$xtT) ~ as.POSIXct(time), col="blue",lwd=2) 
  lines(as.POSIXct(time),mod.output.nac[[i]]$states-1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  lines(as.POSIXct(time),mod.output.nac[[i]]$states+1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
  
  ### Do resids have temporal trend? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #plot(resids$model.residuals[1,], ylab="model residual", xlab="", main="model residuals")
  abline(h=0)
  plot(resids$state.residuals[1,], ylab="state residual", xlab="", main="state residuals")
  abline(h=0)
  mtext("Do resids have temporal trend?", outer = TRUE, cex = 1.5)
  
  ### Are resids normal? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  # qqnorm(resids$model.residuals[1,], main="model residuals", pch=16, 
  #        xlab=paste("shapiro test: ", shapiro.test(resids$model.residuals[1,])[1]))
  # qqline(resids$model.residuals[1,])
  qqnorm(resids$state.residuals[1,], main="state residuals", pch=16, 
         xlab=paste("shapiro test: ", shapiro.test(resids$state.residuals[1,])[1]))
  qqline(resids$state.residuals[1,])
  mtext("Are resids normal?", outer = TRUE, cex = 1.5)
  
  ### residuals vs fitted ###
  par(mfrow=c(2,1),oma = c(2, 0, 2, 0))
  # resids <- residuals(mod.output.nac[[i]])
  # scatter.smooth(as.vector(kf$xtT) ~ resids$model.residuals[1,],
  #                main = "model resids vs fitted (y conditioned)")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]), col="blue")
  scatter.smooth(as.vector(kf$xtT) ~ resids$state.residuals[1,],
                 main = "state resids vs fitted (y conditioned)")
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]), col="blue")
  mtext("Trends in fitted values vs residuals?", outer = TRUE, cex = 1.5)
  
  ### residuals vs observed ###
  # scatter.smooth(as.vector(dat) ~ resids$model.residuals[1,],
  #                main = "model resids vs observed")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]))
  scatter.smooth(as.vector(dat) ~ resids$state.residuals[1,], 
                 main = "state resids vs observed") 
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]))
  mtext("Trends in observed values vs residuals?", outer = TRUE, cex = 1.5, side=1)
  
  dev.off()
}

#### final model selection ####

AICtab(mod.output.nac[["mod.null"]],
       mod.output.nac[[1]])

# dAIC df
# mod.output.nac[[1]]           0   8 
# mod.output.nac[["mod.null"]] 19   3 

grid.arrange(plot.output.nac[[1]])





############################# C3-no3 06-07:08-20 WITH METAB ############################# 
#### data wrangling - covars ####

## subset daily data by site and date range ##
C3 = subset(no3.daily, site.ID == "C3")
## order ##
C3 = dplyr::arrange(C3, day)
# remove entries with NAs in GPP and ER 
C3 = C3[!is.na(C3$GPP_daily_mean),]

# add covs lagged by one day
C3$precip_mm_mean_lag = c(NA, C3$precip_mm_mean[-nrow(C3)])
C3$GDD_lag = c(NA, C3$GDD.local[-nrow(C3)])
# remove first row where NAs were introduced by lagging covars
C3 = C3[-1,]

range(C3$day) # no step change for this ts
# select covs
cov <- C3 %>% select("precip_mm_mean", 
                     "precip_mm_mean_lag",
                     "GDD_lag",
                     "GPP_daily_mean",
                     "ER_daily_mean")

# check for nas
any(is.na(cov))

## generate all non-collinear cov lists ##
covNames = colnames(cov)
b = sapply(1:length(covNames), function(i) (combn(covNames, i, simplify = F)))
c <- unlist(b,recursive=FALSE)

# choose lists with at least 7 variables:
to.remove <- sapply(c, function(c) length(c) < 5)
o <- c[!to.remove]

## check lists for collinearity ##
colin = list()
for(i in 1:length(o)){
  cov <- C3 %>% select(o[[i]])
  colin[[i]] = any(abs(cor(cov))>0.5 & abs(cor(cov))<1)
}
#colin

## remove collinear sets
colin = which(unlist(colin))
for(i in colin){
  o[[i]] = NA
}
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
o=na.omit.list(o)

#### data wrangling - no3 ####

## center response var ##
dat = as.vector(C3$nitrate_uM_filled_mean)
dat <- scale(dat, center=TRUE, scale=F)
## check for NAs ##
any(is.na((dat)))
## plot ##
plot(dat, type="b")
## transpose for MARSS ##
yy <- t(matrix(dat))

#### set MARSS model parms  ####

### inputs to process model ###
# i.e., the model that estimates the hidden random walk from the data
BB = "unconstrained" # allow for and estimate mean reversion
UU = "zero" # do NOT allow a drift to term for the random walk to be estimated
## cc ##
#cc <- t(scale(cov)) # de-mean and scale covariates to units of sd. I do this in the for loop for each combination of variables, so no need to do it here
## CC ##
CC <- "unconstrained" # allow for and estimate the covariate effects
## QQ ##
QQ = "unconstrained" # allow for and estimate the covariance matrix of process errors

### inputs to observtion model ###
# i.e., the model that estimates the response variable
## ZZ ##
ZZ='identity' # (number of estimated state processes) = 1
## AA ##
AA="zero"
## DD ##
DD="zero" # matrix of coefficients to be estimated for covariates in an observation model. 
## dd ##
dd="zero" # these is the covariate matrix in the observation model.
## RR ##
RR="zero"

### initial conditions ###
x0 = matrix("x0") # mean of the initial condition - allow MARSS to estimate this
V0="zero" # covariance of the initial condition, setting to zero makes x0 a fixed parameter
tinitx=0  # start model at t0

#### loop: run non-collinear cov lists ####

mod.output <- list()
for(i in 1:length(o)){
  cov <- C3 %>% select(o[[i]])
  cc <- t(scale(cov))
  #MARSS function call, can change number of iterations if desired
  mod <- MARSS(y=yy, 
               model=list(
                 B=BB, 
                 U=UU,
                 C=CC,
                 c=cc,
                 Q=QQ,
                 Z=ZZ, 
                 A=AA, 
                 D=DD,
                 d=DD,
                 R=RR,
                 x0=x0, 
                 V0=V0,
                 tinitx=tinitx), 
               control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
  # put MARSS output into a list
  mod.output[[i]] <- mod 
}

#### loop: ID and remove models with autocorrelation in residuals ####

## ID
nac = list()
for(i in 1:(length(mod.output))){
  resids <- residuals(mod.output[[i]])
  #acf.mod = Acf(resids$model.residuals[1,], main="model residuals",lag.max=10,plot=F) # with R
  acf.state = Acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass,lag.max=10,plot=F)
  #nac[[i]] = any(c((abs(acf.mod$acf[2:10])>0.225), (abs(acf.state$acf[2:10])>0.225))) # with R
  nac[[i]] = any((abs(acf.state$acf[2:10])>0.25)) # no R
}

## remove
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
nac = which(unlist(nac))
mod.output.nac = mod.output
for(i in nac){
  mod.output.nac[[i]] = NA
}
mod.output.nac=na.omit.list(mod.output.nac)

## plot acf if there are concerns
acf.1 = Acf((residuals(mod.output.nac[[1]]))$state.residuals[1,], 
            main="state residuals", lag.max=10, ci.type="white",ci.col = "red")
acf.2 = Acf((residuals(mod.output.nac[[2]]))$state.residuals[1,], 
            main="state residuals", lag.max=10, ci.type="white",ci.col = "red")
## better ACF plot (from NWFSC)
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "red")
  abline(h = 0)
}
plot.acf(acf.1)
plot.acf(acf.2)


#### loop: save coef plots and coef estimates of no autocorrelation models ####

est.output.nac <- list()
plot.output.nac <-list()

# check date range. Update plot titles and file paths in for loop with this if necessary.
range(C3$day)

for(i in 1:length(mod.output.nac)){
  # estimates
  est = MARSSparamCIs(mod.output.nac[[i]], method = "hessian", alpha = 0.05, nboot = 1000)
  est.output.nac[[i]] <- est 
  # plot estimates
  CIs = cbind(
    est.output.nac[[i]]$par$U,
    est.output.nac[[i]]$par.lowCI$U,
    est.output.nac[[i]]$par.upCI$U)
  CIs = as.data.frame(CIs)
  names(CIs) = c("Est.", "Lower", "Upper")
  CIs$parm = rownames(CIs)
  m.p = ggplot(CIs, aes(parm, Est.)) + 
    geom_point(position=position_dodge(width=0.3), size=2) + 
    geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.3), width=0.2) + 
    theme_bw(base_size=12)+ 
    geom_hline(aes(yintercept=0), linetype="dashed")+
    coord_flip()+ ggtitle(paste("C3 06-08:07-25 NO3", i))
  plot.output.nac[[i]] <- m.p 
  ggsave(m.p, filename = paste("Output from analyses/32_noR_CPCRW_2017_C3.NO3_mods/0608-0725_metab/mod.plot.", i, ".pdf",sep=""), width=6, height=6, units = "in")
}
## save plot in this R environment ###
i=1 #then run loop
C3_NO3_metab = 
  ggplot(CIs, aes(parm, Est.)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper),position=position_dodge(width=0.5), width=0.5) +
  geom_point(position=position_dodge(width=0.3), size=10, color="#0571B0") + 
  theme_bw(base_size=25)+ 
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_flip()+ 
  ggtitle("NO3- in 54% PF (C3)\n June 08 - July 25")+ 
  scale_x_discrete(name ="", 
                   limits=c("(X.Y1,ER_daily_mean)",
                            "(X.Y1,GPP_daily_mean)",
                            "(X.Y1,GDD_lag)",
                            "(X.Y1,precip_mm_mean_lag)",
                            "(X.Y1,precip_mm_mean)",
                            "(X.Y1,step)"),
                   labels = c("Stream ER",
                              "Stream GPP",
                              "GDU lagged",
                              "Rain lagged",
                              "Rain",
                              "Step-change"))+
  ylab("Covariate Effects")
C3_NO3_metab


#### model comparisons (AIC) ####

### run model with no covs ###
mod.null <- MARSS(y=yy, 
                  model=list(
                    B=BB, 
                    U=UU,
                    Q=QQ,
                    Z=ZZ, 
                    A=AA, 
                    D=DD,
                    d=DD,
                    R=RR,
                    x0=x0, 
                    V0=V0,
                    tinitx=tinitx), 
                  control=list(maxit= 2000, allow.degen=TRUE, trace=1), fit=TRUE)
## save null model to mod.output.nac list
mod.output.nac[[(length(mod.output.nac)+1)]] = mod.null
## name models in list
names(mod.output.nac) = c(seq(1:(length(mod.output.nac)-1)), "mod.null")

### identify models that did not converge ###
errors = list()
for(i in 1:length(mod.output.nac)){
  errors[[i]] = mod.output.nac[[i]][["errors"]]
}
errors

#### loop: plot and save all model diagnostics ####

dat = yy
time = C3$day

for(i in 1:(length(mod.output.nac)-1)){
  pdf(paste("Output from analyses/32_noR_CPCRW_2017_C3.NO3_mods/0608-0725_metab/model diagnostics/diagnostics.mod.", i, ".pdf",sep=""), width = 10, height =8)
  
  ### Do resids have temporal autocorrelation? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #acf(resids$model.residuals[1,], main="model residuals")
  Acf(resids$state.residuals[1,], main="state residuals", na.action=na.pass)
  mtext("Do resids have temporal autocorrelation?", outer = TRUE, cex = 1.5)
  
  ### Fitted values over observations ###
  par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
  ## stocastic model with drift
  kf=print(mod.output.nac[[i]], what="kfs") # Kalman filter and smoother output
  # prediction conditioned on all data.
  plot(as.vector(dat) ~ as.POSIXct(time), type="o", 
       main = "fitted values conditioned on all y")
  lines(as.vector(kf$xtT) ~ as.POSIXct(time), col="blue",lwd=2) 
  lines(as.POSIXct(time),mod.output.nac[[i]]$states-1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  lines(as.POSIXct(time),mod.output.nac[[i]]$states+1.96*mod.output.nac[[i]]$states.se,type="l",lwd=2,lty=2,col="blue")
  mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
  
  ### Do resids have temporal trend? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  #plot(resids$model.residuals[1,], ylab="model residual", xlab="", main="model residuals")
  abline(h=0)
  plot(resids$state.residuals[1,], ylab="state residual", xlab="", main="state residuals")
  abline(h=0)
  mtext("Do resids have temporal trend?", outer = TRUE, cex = 1.5)
  
  ### Are resids normal? ###
  par(mfrow=c(2,1),oma = c(0, 0, 2, 0))
  resids <- residuals(mod.output.nac[[i]])
  # qqnorm(resids$model.residuals[1,], main="model residuals", pch=16, 
  #        xlab=paste("shapiro test: ", shapiro.test(resids$model.residuals[1,])[1]))
  # qqline(resids$model.residuals[1,])
  qqnorm(resids$state.residuals[1,], main="state residuals", pch=16, 
         xlab=paste("shapiro test: ", shapiro.test(resids$state.residuals[1,])[1]))
  qqline(resids$state.residuals[1,])
  mtext("Are resids normal?", outer = TRUE, cex = 1.5)
  
  ### residuals vs fitted ###
  par(mfrow=c(2,1),oma = c(2, 0, 2, 0))
  # resids <- residuals(mod.output.nac[[i]])
  # scatter.smooth(as.vector(kf$xtT) ~ resids$model.residuals[1,],
  #                main = "model resids vs fitted (y conditioned)")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]), col="blue")
  scatter.smooth(as.vector(kf$xtT) ~ resids$state.residuals[1,],
                 main = "state resids vs fitted (y conditioned)")
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]), col="blue")
  mtext("Trends in fitted values vs residuals?", outer = TRUE, cex = 1.5)
  
  ### residuals vs observed ###
  # scatter.smooth(as.vector(dat) ~ resids$model.residuals[1,],
  #                main = "model resids vs observed")
  # abline(lm(as.vector(kf$xtT) ~ resids$model.residuals[1,]))
  scatter.smooth(as.vector(dat) ~ resids$state.residuals[1,], 
                 main = "state resids vs observed") 
  abline(lm(as.vector(kf$xtT) ~ resids$state.residuals[1,]))
  mtext("Trends in observed values vs residuals?", outer = TRUE, cex = 1.5, side=1)
  
  dev.off()
}

#### final model selection ####

AICtab(mod.output.nac[["mod.null"]],
       mod.output.nac[[1]])

# dAIC df
# mod.output.nac[[1]]          0.0  8 
# mod.output.nac[["mod.null"]] 3.1  3  

grid.arrange(plot.output.nac[[1]])





############################# 
#### make final plot ####

grid.arrange(C3_NO3_nometab, C2_NO3_nometab, ncol=2) # 15x7 portrait
grid.arrange(C3_NO3_metab, C2_NO3_metab, ncol=2) # 15x8 portrait