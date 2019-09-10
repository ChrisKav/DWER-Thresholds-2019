source("functions.R")
source("Data_prep_script.R")

pdf("Site GAM models.pdf")
Site.GW.plots
dev.off()

source("Goollelal/water_summary.R")
pdf("Goollelal/water_level_plot.pdf")
Site.GW.plots$gool.p
dev.off()

source("Loch_McNess/water_summary.R")
pdf("Loch_McNess/water_level_plot.pdf")
Site.GW.plots$mcness.p
dev.off()

source("Joondalup_Nth/water_summary.R")
pdf("Joondalup_Nth/water_level_plot.pdf")
Site.GW.plots$joon.p
dev.off()

source("Yonderup/water_summary.R")
pdf("Yonderup/water_level_plot.pdf")
Site.GW.plots$yond.p
dev.off()

#mcmc_control <- list(n.burnin = 10000, n.iteration = 60000, 
#                     n.thin = 50, seed=28041948)

mcmc_control <- list(n.burnin = 10, n.iteration = 60, 
                     n.thin = 5, seed=28041948)

source("Goollelal/vegetation.R")
source("Loch_McNess/vegetation.R")
source("Joondalup_Nth/vegetation.R")
source("Yonderup/vegetation.R")