setwd("C:/Users/Chris/Desktop/beta plots")
library(ggplot2)
library(RColorBrewer)
library(R.utils)

boral.df <- read.csv("Beta_Veg_boral.csv")
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
