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

setwd("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds/EMP_173")

Y <- comm$EPP173
wat.obs <- data.ls$'61613213'
ahd <- AHD.data.list$EMP173.AHD
ahd.p <- Params.data.list$EMP173.params
WL_group <- "ground"
gw.plot <- Site.GW.plots$emp173.p
a.ele <- 51.006
b.ele <- 51.38
c.ele <- 51.54
d.ele <- 51.564

# 1. Build HMSC object
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
GW.levs.d <- GW.levs
GW.levs.d$meanGW <- GW.levs.d$meanGW - d.ele
GW.levs.d$Plot <- "D"
GW.levs <- rbind(GW.levs.a,GW.levs.b,GW.levs.c,GW.levs.d)
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

sapply(colnames(vegfit.mod2$X), coefsplot, vegfit.mod2)
sapply(colnames(vegfit.mod3$X), coefsplot, vegfit.mod3)

mod1.ext <- boral.extract(vegfit.mod1, XData)
mod3.ext <- boral.extract(vegfit.mod3, XData)

veg.ord1 <- boral.plots(mod1.ext)
veg.ord2 <- boral.plots(mod3.ext)

ggsave("Vegetation/vege_ordination.pdf", ggarrange(veg.ord1, veg.ord2, ncol=2), device="pdf")

save(vegfit.mod1, vegfit.mod2, vegfit.mod3, mod1.ext, mod3.ext, veg.ord1, veg.ord2, file="boral_models.RData")

#########################################                          
#########################################
### DATA EXTRACTION FROM boral MODELS ###
#########################################
#########################################

library(boral)
library(abind)
library(matrixStats)
library(ineq)

################################################
### Define species and coefficient names/ids ###
################################################

species.id <- 1:ncol(Y2)  # vector of species ids [for extraction]
species.names <- colnames(Y2) # vector of species names

Env.id <- 1  # vector of covariate ids (no intercept) [for extraction]
Env.names <- c("intercept", "meanGW") # vector of covariate names incl. intercept

model <- "boral"

############################################
### Extract MCMC samples from jags model ###
############################################

mcmc.samp <- as.mcmc(vegfit.mod3$jags.model) # extracts mcmc from boral object

colnames(mcmc.samp[[1]]) # column names from mcmc object

#######################################
### Standardise posteriors post-hoc ###
#######################################

dataset_sd <- Y2 #read.csv("Eucalypts_sd.csv")   # load in original data sd
dataset_sd <- dataset_sd[,2]
# dataset_sd <- c(1, dataset_sd) # add intercept ##NOT REQUIRED FOR BORAL

mcmc.samp_standardised <- mcmc.samp

# for(i in seq(length(full_standardised$trace$B))){   # species  ## EASIER TO DO DURING EXTRACTON FOR BORAL
#   for(j in seq(length(dataset_sd))){                #covariate
#     full_standardised$trace$B[[i]][,j] <- (full_standardised$trace$B[[i]][,j])/dataset_sd[j]
#   }
# }

##############################################
### Extract regression coefficient samples ###
##############################################

# Not standardised

beta.samp <- matrix(nrow = 10)

col.names <- vector()

for(i in species.id){ # species
  
  column.name <- sprintf("lv.coefs[%s,1]", i)  # 3 lines code to get species intercept samples
  col.names <- c(col.names, column.name) 
  command <- sprintf('mcmc.samp[[1]][1:10,"%s"]', column.name)
  tmp <- eval(parse(text = command))
  
  for(j in Env.id){ # variables
    
    column.name2 <- sprintf("X.coefs[%s,%s]", i, j) # 3 lines code to get variables samples (by species)
    col.names <- c(col.names, column.name2)
    command2 <- sprintf('mcmc.samp[[1]][1:10, "%s"]', column.name2)
    tmp2 <- eval(parse(text = command2))
    
    tmp <- cbind(tmp, tmp2) # create entire matrix for one species
    
  }
  
  beta.samp <- cbind(beta.samp, tmp) # bind all species matrcies together 
  
}

beta.samp <- beta.samp[,-(1)] # remove NA column from beginning
colnames(beta.samp) <- col.names # set column names

# Standardised

beta.samp_standardised <- matrix(nrow = 10)

col.names_standardised <- vector()

for(i in species.id){ # species
  
  column.name <- sprintf("lv.coefs[%s,1]", i)  # 3 lines code to get species intercept samples
  col.names_standardised <- c(col.names_standardised, column.name) 
  command <- sprintf('mcmc.samp[[1]][1:10,"%s"]', column.name)
  tmp <- eval(parse(text = command))
  
  for(j in Env.id){ # variables
    
    column.name2 <- sprintf("X.coefs[%s,%s]", i, j) # 3 lines code to get variables samples (by species)
    col.names_standardised <- c(col.names_standardised, column.name2)
    command2 <- sprintf('(mcmc.samp[[1]][1:10, "%s"])/dataset_sd[%s]', column.name2, j) # added sd correction
    tmp2 <- eval(parse(text = command2))
    
    tmp <- cbind(tmp, tmp2) # create entire matrix for one species
    
  }
  
  beta.samp_standardised <- cbind(beta.samp_standardised, tmp) # bind all species matrcies together 
  
}

beta.samp_standardised <- beta.samp_standardised[,-(1)] # remove NA column from beginning
colnames(beta.samp_standardised) <- col.names_standardised # set column names

#####################################################
### Extract Mean/SD/Quantiles and into Data Frame ###
#####################################################

# Not standardised

beta.mean <- colMeans(beta.samp)
beta.mean <- matrix(beta.mean, ncol = length(Env.names), byrow = T)
colnames(beta.mean) <- Env.names
rownames(beta.mean) <- species.names

beta.sd <- colSds(beta.samp)
beta.sd <- matrix(beta.sd, ncol = length(Env.names), byrow = T)
colnames(beta.sd) <- Env.names
rownames(beta.sd) <- species.names

beta.lower <- colQuantiles(beta.samp, probs = 0.025)
beta.lower <- matrix(beta.lower, ncol = length(Env.names), byrow = T)
colnames(beta.lower) <- Env.names
rownames(beta.lower) <- species.names

beta.upper <- colQuantiles(beta.samp, probs = 0.975)
beta.upper <- matrix(beta.upper, ncol = length(Env.names), byrow = T)
colnames(beta.upper) <- Env.names
rownames(beta.upper) <- species.names

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

beta.cv <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- coefVar(beta.samp[,i])
  beta.cv <- c(beta.cv, tmp)
}
beta.cv <- matrix(beta.cv, ncol = length(Env.names), byrow = TRUE)
colnames(beta.cv) <- Env.names
rownames(beta.cv) <- species.names

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- qcd(beta.samp[,i])
  beta.qcd <- c(beta.qcd, tmp)
}
beta.qcd <- matrix(beta.qcd, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd) <- Env.names
rownames(beta.qcd) <- species.names

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd2 <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- qcd2(beta.samp[,i])
  beta.qcd2 <- c(beta.qcd2, tmp)
}
beta.qcd2 <- matrix(beta.qcd2, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd2) <- Env.names
rownames(beta.qcd2) <- species.names

beta.gini <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- ineq(beta.samp[,i], type = "Gini")
  beta.gini <- c(beta.gini, tmp)
}
beta.gini <- matrix(beta.gini, ncol = length(Env.names), byrow = TRUE)
colnames(beta.gini) <- Env.names
rownames(beta.gini) <- species.names

# Standardised

beta.mean_standardised <- colMeans(beta.samp_standardised)
beta.mean_standardised <- matrix(beta.mean_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.mean_standardised) <- Env.names
rownames(beta.mean_standardised) <- species.names

beta.sd_standardised <- colSds(beta.samp_standardised)
beta.sd_standardised <- matrix(beta.sd_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.sd_standardised) <- Env.names
rownames(beta.sd_standardised) <- species.names

beta.lower_standardised <- colQuantiles(beta.samp_standardised, probs = 0.025)
beta.lower_standardised <- matrix(beta.lower_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.lower_standardised) <- Env.names
rownames(beta.lower_standardised) <- species.names

beta.upper_standardised <- colQuantiles(beta.samp_standardised, probs = 0.975)
beta.upper_standardised <- matrix(beta.upper_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.upper_standardised) <- Env.names
rownames(beta.upper_standardised) <- species.names

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

beta.cv_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- coefVar(beta.samp_standardised[,i])
  beta.cv_standardised <- c(beta.cv_standardised, tmp)
}
beta.cv_standardised <- matrix(beta.cv_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.cv_standardised) <- Env.names
rownames(beta.cv_standardised) <- species.names

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- qcd(beta.samp_standardised[,i])
  beta.qcd_standardised <- c(beta.qcd_standardised, tmp)
}
beta.qcd_standardised <- matrix(beta.qcd_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd_standardised) <- Env.names
rownames(beta.qcd_standardised) <- species.names

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd2_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- qcd2(beta.samp_standardised[,i])
  beta.qcd2_standardised <- c(beta.qcd2_standardised, tmp)
}
beta.qcd2_standardised <- matrix(beta.qcd2_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd2_standardised) <- Env.names
rownames(beta.qcd2_standardised) <- species.names

beta.gini_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- ineq(beta.samp_standardised[,i], type = "Gini")
  beta.gini_standardised <- c(beta.gini_standardised, tmp)
}
beta.gini_standardised <- matrix(beta.gini_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.gini_standardised) <- Env.names
rownames(beta.gini_standardised) <- species.names

##############################
### Create blank dataframe ### # Not standardised
##############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                 qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

############################
### Extract to dataframe ### # Not standardised
############################

for(i in 1:ncol(Y2)){
  dfr <- cbind(Env.names, beta.mean[i,], beta.lower[i,], beta.upper[i,], beta.sd[i,],
               beta.cv[i,], beta.qcd[i,], beta.qcd2[i,], beta.gini[i,],
               rep("boral", (length(Env.names))), rep(species.names[i], length(Env.names)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df <- rbind(df, dfr)
}

rownames(df) <- NULL

##############################
### Create blank dataframe ### # Standardised
##############################

df_standardised <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                              upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                              qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

############################
### Extract to dataframe ### # Standardised
############################

for(i in 1:ncol(Y2)){
  dfr <- cbind(Env.names, beta.mean_standardised[i,], beta.lower_standardised[i,],
               beta.upper_standardised[i,], beta.sd_standardised[i,],
               beta.cv_standardised[i,], beta.qcd_standardised[i,], beta.qcd2_standardised[i,],
               beta.gini_standardised[i,], rep("boral", (length(Env.names))),
               rep(species.names[i], length(Env.names)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df_standardised <- rbind(df_standardised, dfr)
}

rownames(df_standardised) <- NULL

df_merge <- df
df_merge[,5:9] <- df_standardised[,5:9]

#############################
### Rho Mean/Sd/Quantiles ###
#############################

#source("get_residual_corr_new_function.R") # implement modified version of get.residual.cor() to provide sd and quantile values

rho.mean <- get.residual.cor.new(vegfit.mod3, est = "mean")$cor
rho.sd <- get.residual.cor.new(vegfit.mod3, est = "sd")$cor
rho.lower <- get.residual.cor.new(vegfit.mod3, est = "q_lower")$cor
rho.upper <- get.residual.cor.new(vegfit.mod3, est = "q_upper")$cor

####################
#### Write CSVs ####
####################

write.csv(df_merge, "Vegetation/Beta/Beta_Veg_boral.csv")
write.csv(rho.mean, "Vegetation/Rho/Rho_mean_Veg_boral.csv")
write.csv(rho.lower, "Vegetation/Rho/Rho_lower_Veg_boral.csv")
write.csv(rho.upper, "Vegetation/Rho/Rho_upper_Veg_boral.csv")
write.csv(rho.sd, "Vegetation/Rho/Rho_sd_Veg_boral.csv")

setwd("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds")


