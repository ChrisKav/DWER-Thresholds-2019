library(reshape2)

M.rhaphiophyla <- c(0.0006,-2.14, 1.03, -4.49)
M.preissiana <- c(-0.54, -2.62, 1.03, -5.04)
E.rudis <- c(-0.7, -3.26, 1.03, -6.44)
B.littoralis <- c(-0.39, -1.92, 0.43, -3.09)
B.articulata <- c(0.28, -1.22, 0.81, -2.59)
T.orientalis <- c(0.74, -0.95, 1.49, -1.9)
A.fascicularis <- c(-0.35, -2.26, 1.03, -4.6)

EWR.new <- rbind(M.rhaphiophyla, M.preissiana, E.rudis, B.littoralis , B.articulata, T.orientalis, A.fascicularis)
colnames(EWR.new) <- c("mean_max", "mean_min", "absolute_max", "absolute_min")
Species <- data.frame(row.names(EWR.new))
EWR.new <- cbind(Species, EWR.new)
colnames(EWR.new)[1] <- "Species"
row.names(EWR.new) <- NULL

EWR.new.base <- EWR.new[,1:3]
EWR.new.base <- melt(EWR.new.base, id="Species")
EWR.new.abs <- EWR.new[,c(1,4,5)]
EWR.new.abs <- melt(EWR.new.abs, id="Species")

#WM1

WM1.thresh <- 55.7
WM1.prop <- 53.7
WM1.ground <- 60.58

WM1.tresh <- WM1.thresh - WM1.ground
WM1.prop <- WM1.prop - WM1.ground

ggplot(EWR.new) +  
  geom_hline(yintercept=0, color="green") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=5, color="orange") +
  geom_hline(yintercept=WM1.tresh, linetype="dotted") +
  geom_hline(yintercept=WM1.prop, linetype="dashed") +
  theme_bw() +
  labs(y = "Depth (m)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))
