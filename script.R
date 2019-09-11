source("functions.R")
source("Data_prep_script.R")

########################
# WATER DATA - Summary #
########################

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

source("Mariginiup/water_summary.R")
pdf("Mariginiup/water_level_plot.pdf")
Site.GW.plots$marig.p
dev.off()

source("Jandabup/water_summary.R")
pdf("Jandabup/water_level_plot.pdf")
Site.GW.plots$jand.p
dev.off()

source("Nowergup_Nth/water_summary.R")
pdf("Nowergup_Nth/water_level_plot.pdf")
Site.GW.plots$nower.p
dev.off()

source("Wilgarup/water_summary.R")
pdf("Wilgarup/water_level_plot.pdf")
Site.GW.plots$wilg.p
dev.off()

source("Pipidinny/water_summary.R")
pdf("Pipidinny/water_level_plot.pdf")
Site.GW.plots$pipi.p
dev.off()

source("Lexia_186/water_summary.R")
pdf("Lexia_186/water_level_plot.pdf")
Site.GW.plots$lex186.p
dev.off()

source("EMP_173/water_summary.R")
pdf("EMP_173/water_level_plot.pdf")
Site.GW.plots$emp173.p
dev.off()

source("Gwelup/water_summary.R")
pdf("Gwelup/water_level_plot.pdf")
Site.GW.plots$gwel.p
dev.off()

source("Quin_Brook/water_summary.R")
pdf("Quin_Brook/water_level_plot.pdf")
Site.GW.plots$quin.p
dev.off()

#mcmc_control <- list(n.burnin = 10000, n.iteration = 60000, 
#                     n.thin = 50, seed=28041948)

mcmc_control <- list(n.burnin = 10, n.iteration = 60, 
                     n.thin = 5, seed=28041948)

source("Goollelal/vegetation.R")
source("Loch_McNess/vegetation.R")
source("Joondalup_Nth/vegetation.R")
source("Yonderup/vegetation.R")
source("Gwelup/vegetation.R")
source("Jandabup/vegetation.R")
source("Joondalup_Sth/vegetation.R")
source("Lexia_186/vegetation.R")
source("Mariginiup/vegetation.R")
source("Nowergup_Nth/vegetation.R")
source("Nowergup_Sth/vegetation.R")
source("Quin_Brook/vegetation.R")
source("Wilgarup/vegetation.R")
