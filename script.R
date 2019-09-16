source("functions.R")
source("Data_prep_script.R")

pdf("Site GAM models.pdf")
Site.GW.plots
dev.off()

wetlands <- read.csv("wetlands.csv", header=FALSE)[,1]

########################
# WATER DATA - Summary #
########################

# Loop that runs source code in each directory

for (i in 1:length(wetlands)) {
  source(paste0(wetlands[i], "/water_summary.R"))
}

#######################
# Vegetaton analysis  #
#######################
# DONOT RUN - TAKES AGESSSSSSSS
#mcmc_control <- list(n.burnin = 10000, n.iteration = 60000, 
#                     n.thin = 50, seed=28041948)

#mcmc_control <- list(n.burnin = 10, n.iteration = 60, 
#                     n.thin = 5, seed=28041948)

#for (i in 1:length(wetlands)) {
#  if (file.exists(paste0(wetlands[i], "/vegetation.R"))) {
#    source(paste0(wetlands[i], "/vegetation.R"))
#  }
#}

####################
# Vegetaton plots  #
####################

for (i in 1:length(wetlands)) {
    if (file.exists(paste0(wetlands[i], "/Vegetation/Beta/Beta_Veg_boral.csv"))) {
    source("Plot_Figures.R")
  }
}

