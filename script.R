source("functions.R")
source("Data_prep_script.R")

df("Site GAM models.pdf")
Site.GW.plots
dev.off()

source("Goollelal/water_summary.R")

kable(sw.sum)
