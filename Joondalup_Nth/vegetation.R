library(Hmsc)
library(stringr)
library(dplyr)
library(knitr)
library(corrplot)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(boral)
library(data.table)
library(plotly)
library(lubridate)

load("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds/Refined_data.RData")

setwd("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds/Joondalup_Nth")

Y <- comm$Joondalup_Nth
wat.obs <- data.ls$'6162572'
ahd <- AHD.data.list$Joondalup.AHD
ahd.p <- Params.data.list$Joondalup.params
WL_group <- "surface"
gw.plot <- Site.GW.plots$joon.p
a.ele <- 17.33
b.ele <- 18.849
c.ele <- 20.275

# 1. Build HMSC object
Y <- Y[ Y$plot != "D", , drop=TRUE] # No plot elevation for plot D
Y2 <- comm_prep(Y)
studyDesign <- Y2[[2]]
bor.study.design <- studyDesign
Y2 <- Y2[[1]]

GW.levs <- subset(ahd.p, group==WL_group)
GW.levs$Year <- format(GW.levs$Date, "%Y")
GW.levs <- aggregate(GW.levs$p3, list(GW.levs$Year), mean)
colnames(GW.levs) <- c("Year", "meanGW")
GW.levs.a <- GW.levs
GW.levs.a$meanGW <- GW.levs.a$meanGW - a.ele
GW.levs.a$Plot <- "A"
GW.levs.b <- GW.levs
GW.levs.b$meanGW <- GW.levs.b$meanGW - b.ele
GW.levs.b$Plot <- "B"
GW.levs.c <- GW.levs
GW.levs.c$meanGW <- GW.levs.c$meanGW - c.ele
GW.levs.c$Plot <- "C"
GW.levs <- rbind(GW.levs.a,GW.levs.b,GW.levs.c)
XData <- subset(GW.levs, Year %in% studyDesign$Year)
XData <- XData[with(XData, order(Year, Plot)),]
rownames(XData) <- NULL
XData <- merge(XData, bor.study.design[,c("Year", "Plot")], by.x=c("Year", "Plot"), by.y=c("Year", "Plot"))

Traits <- determine_traits(Y2)

nS <- ncol(Y2)
n <- nrow(Y2)
studyDesign1 = data.frame(sample = sprintf('sample_%.3d',1:n))
rL1 = HmscRandomLevel(units = studyDesign1$sample)
time <- as.matrix(data.frame(studyDesign$Year))
plot <- as.matrix(data.frame(studyDesign$Plot))
studyDesign$Year <- sprintf('Year_%.3d', studyDesign$Year)
studyDesign$Plot <- sprintf('Plot_%.1s', studyDesign$Plot)
studyDesign$Sample <- NULL
rownames(time) <- studyDesign$Year
rownames(plot) <- studyDesign$Plot
rL2 = HmscRandomLevel(sData = time)
rL2 = setPriors(rL2,nfMin=1,nfMax=1)
rL3 = HmscRandomLevel(units = plot)
studyDesign2 <- data.frame(Year = studyDesign$Year, sample = studyDesign1$sample)
studyDesign3 <- data.frame(Year = studyDesign$Year, sample = studyDesign1$sample, Plot=studyDesign$Plot)

XFormula <- ~ poly(meanGW, degree=2, raw=TRUE)
TrFormula <- ~ Status

hmsc.ob = Hmsc(Y=Y2, XData=XData, XFormula = XFormula,
                     TrData = Traits, TrFormula = TrFormula, distr= "lognormal poisson",
                     studyDesign=studyDesign3, ranLevels=list(sample=rL1, Year=rL2, Plot=rL3))

#mcmc_control <- list(n.burnin = 10000, n.iteration = 60000, 
#                             n.thin = 50, seed=28041948)

bor.study.design <- bor.study.design[,1:2]
bor.study.design$Year <- as.integer(bor.study.design$Year)
bor.study.design$Plot <- as.integer(as.numeric(bor.study.design$Plot))
bor.study.design <- cbind(bor.study.design[[1]], bor.study.design[[2]])
Traits.dummy <- data.frame(model.matrix(~as.matrix(Traits))[,2])
colnames(Traits.dummy)[[1]] <- "Status"
which_traits <- vector("list",2)
for(i in 1:length(which_traits)) 
  which_traits[[i]] <- 1

# Model 1 - model with 2 LV, site effects and no env vovariates (Pure latent variable model)

vegfit.mod1 <- boral(Y2,
                     X=NULL,
                     family="negative.binomial",
                     lv.control = list(num.lv = 2),
                     mcmc.control = mcmc_control,
                     row.ids = cbind(1:nrow(Y2), bor.study.design),
                     save.model=TRUE,
                     model.name=NULL)

# Model 2 - model with env covariates and no LV (Independent response model)
vegfit.mod2 <- boral(Y2,
                     X=XData$meanGW,
                     traits = Traits.dummy,
                     which.traits = which_traits,
                     family="negative.binomial",
                     lv.control = list(num.lv = 0),
                     mcmc.control = mcmc_control,
                     row.ids = cbind(1:nrow(Y2), bor.study.design),
                     save.model=TRUE,
                     model.name=NULL)

# Model 3 - Correlated response models
vegfit.mod3 <- boral(Y2,
                     X=XData$meanGW,
                     traits = Traits.dummy,
                     which.traits = which_traits,
                     family="negative.binomial",
                     lv.control = list(num.lv = 2),
                     mcmc.control = mcmc_control,
                     row.ids = cbind(1:nrow(Y2), bor.study.design),
                     save.model=TRUE,
                     model.name=NULL)

save(vegfit.mod1, vegfit.mod2, vegfit.mod3, file="boral_models.RData")

sapply(colnames(vegfit.mod2$X), coefsplot, vegfit.mod2)
sapply(colnames(vegfit.mod3$X), coefsplot, vegfit.mod3)

lvsplot(vegfit.mod1, biplot=FALSE)
lvsplot(vegfit.mod3, biplot=FALSE)

mod1.ext <- boral.extract(vegfit.mod1, XData)
mod3.ext <- boral.extract(vegfit.mod3, XData)

boral.plots(mod1.ext)
boral.plots(mod3.ext)

setwd("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds")


