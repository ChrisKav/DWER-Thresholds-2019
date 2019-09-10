source("functions.R")
source("Data_prep_script.R")

pdf("Site GAM models.pdf")
Site.GW.plots
dev.off()

source("Goollelal/water_summary.R")
source("Loch_McNess/water_summary.R")
source("Joondalup_Nth/water_summary.R")
source("Yonderup/water_summary.R")

#mcmc_control <- list(n.burnin = 10000, n.iteration = 60000, 
#                     n.thin = 50, seed=28041948)

mcmc_control <- list(n.burnin = 10, n.iteration = 60, 
                     n.thin = 5, seed=28041948)

source("Goollelal/vegetation.R")
source("Loch_McNess/vegetation.R")
source("Joondalup_Nth/vegetation.R")
source("Yonderup/vegetation.R")