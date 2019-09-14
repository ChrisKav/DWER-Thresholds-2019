library(ggplot2)
library(RColorBrewer)
library(R.utils)
library(corrplot)
library(openxlsx)

#Beta

boral.df <- read.csv(paste0(wetlands[i], "/Vegetation/Beta/Beta_Veg_boral.csv"))
boral.df <- boral.df[,-c(1,6:10)]
colnames(boral.df) <- c("Coefficient", "Posterior.Mean", "Lower", "Upper", "Model", "Species")
boral.df$Coefficient <- factor(boral.df$Coefficient, levels = c("intercept", "meanGW"))
boral.df2 <- subset(boral.df, Coefficient=="meanGW")
i1 <- (rowSums(boral.df2[,3:4] > 0)) & (rowSums(boral.df2[,3:4] < 0))
boral.df3 <- boral.df2[!i1,]

beta.plot <- ggplot(boral.df3, aes(x = Species, y = Posterior.Mean)) + 
  geom_point() +
  geom_errorbar(aes(ymax=Upper,ymin=Lower), position=position_dodge(width=0.5)) +
  theme_bw() +
  ylab("Posterior Mean") + 
  xlab("Species") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dashed")

file.name <- paste0(wetlands[i], "/Vegetation/Beta_", paste0(wetlands[i]), ".pdf", sep = "")
ggsave(file.name, plot=beta.plot, units = "in", width = 7, height = 7)

#Rho

boral_Rho_Mean <- read.csv(paste0(wetlands[i], "/Vegetation/Rho/Rho_mean_Veg_boral.csv"))
rownames(boral_Rho_Mean) <- boral_Rho_Mean[,1]
boral_Rho_Mean <- boral_Rho_Mean[,-1]
boral_Rho_Mean <- as.matrix(boral_Rho_Mean)
colnames(boral_Rho_Mean) <- rownames(boral_Rho_Mean) <- 1:ncol(boral_Rho_Mean)

boral_Rho_Lower <- read.csv(paste0(wetlands[i], "/Vegetation/Rho/Rho_lower_Veg_boral.csv"))
rownames(boral_Rho_Lower) <- boral_Rho_Lower[,1]
boral_Rho_Lower <- boral_Rho_Lower[,-1]
boral_Rho_Lower <- as.matrix(boral_Rho_Lower)
colnames(boral_Rho_Lower) <- rownames(boral_Rho_Lower) <- 1:ncol(boral_Rho_Lower)

boral_Rho_upper <- read.csv(paste0(wetlands[i], "/Vegetation/Rho/Rho_upper_Veg_boral.csv"))
rownames(boral_Rho_upper) <- boral_Rho_upper[,1]
boral_Rho_upper <- boral_Rho_upper[,-1]
boral_Rho_upper <- as.matrix(boral_Rho_upper)
colnames(boral_Rho_upper) <- rownames(boral_Rho_upper) <- 1:ncol(boral_Rho_upper)

Uncertainty_boral <- boral_Rho_upper - boral_Rho_Lower
colnames(Uncertainty_boral) <- rownames(Uncertainty_boral) <- 1:ncol(Uncertainty_boral)
Uncertainty_boral <- as.matrix(Uncertainty_boral)

pdf(file = paste0(wetlands[i], "/Vegetation/", paste0(wetlands[i], " Correlation Plot Vegetation.pdf")), 
    width = 14, height = 5.5)
par(mfrow = c(1,2), mar = c(4,2,4,2), oma = c(0.5,3,0,3))

corrplot(boral_Rho_Mean,
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")

corrplot(Uncertainty_boral, 
         is.corr = F,
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         cl.lim = c(0,2),
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")
dev.off()


