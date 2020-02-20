library(reshape2)
library(ggplot2)

#Dampland 78

Damp78_EWR <- cbind(c(67.71, 68.33, 69.61, 70.15), c(67.74,	67.96,	69.5,	69.5), c(67.59,	67.59,	69.86,	70.59))
Damp78_EWR <- t(Damp78_EWR)
colnames(Damp78_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("A. scoparia", "B. ilicifolia", "M. preissiana")
Damp78_EWR <- cbind.data.frame(Species, Damp78_EWR)
Damp78.thresh <- 65.1
Damp78.prop <- 64.7
EWR_AHD <- c(65.44, NA, 61.97)
Damp78_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Damp78_EWR_plot <- ggplot(Damp78_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Damp78.thresh, linetype="dotted") +
  geom_hline(yintercept=Damp78.prop, linetype="dashed") +
  geom_errorbar(data=Damp78_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
  annotate("text", x = 0, y = Damp78.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Damp78.prop, vjust=+1.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

#EPP173

Epp173_EWR <- cbind(c(50.99,	51.01,	51.57,	51.57), c(50.39,	50.59,	50.84,	50.87), c(51.54,	51.54,	51.57,	51.57),
                    c(50.75,	50.78,	51.56,	51.57), c(51.37,	51.38,	51.75,	51.57), c(51.56,	51.56,	51.57,	51.57))
Epp173_EWR <- t(Epp173_EWR)
colnames(Epp173_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("A.scoparia", "B.articulata", "H. angustifolium", "L. longitudinale",
             "M.preissiana", "P.ellipticum")
Epp173_EWR <- cbind.data.frame(Species, Epp173_EWR)
Epp173.thresh <- 50.2
Epp173.prop <- 48.5
EWR_AHD <- c(48.79, 48.98, 48.02, NA, 48.76, 49.34)
Epp173_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Epp173_EWR_plot <- ggplot(Epp173_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Epp173.thresh, linetype="dotted") +
  geom_hline(yintercept=Epp173.prop, linetype="dashed") +
  geom_errorbar(data=Epp173_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
  annotate("text", x = 0, y =Epp173.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Epp173.prop, vjust=+1.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

#Goollelal

Gooll_EWR <- cbind(c(26.6,	26.6,	26.71,	26.74), c(26.76,	26.77,	27.01,	27.02), c(26.65,	26.65,	26.78,	26.78),
                   c(26.6,	26.6,	27.02,	27.02), c(26.6,	26.6,	26.65,	26.65))
Gooll_EWR <- t(Gooll_EWR)
colnames(Gooll_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("B.articulata", "E.rudis", "L. longitudinale", "M.rhaphiophylla", "T.orientalis")
Gooll_EWR <- cbind.data.frame(Species, Gooll_EWR)
Gooll.thresh <- 26
Gooll.prop <- 26.4
EWR_AHD <- c(25.38, 23.39, NA, 24.46, NA)
Gooll_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Gooll_EWR_plot <- ggplot(Gooll_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Gooll.thresh, linetype="dotted") +
  geom_hline(yintercept=Gooll.prop, linetype="dashed") +
  geom_errorbar(data=Gooll_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
  annotate("text", x = 0, y = Gooll.thresh, vjust=+1.25, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Gooll.prop, vjust=-0.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

# Jandabup
Janda_EWR <- cbind(c(46.5,	46.5,	48.33,	48.53), c(47.9,	48.02,	47.9,	47.9), c(47.9,	48.04,	48.04,	47.9),
                   c(46.5,	46.5,	48.34,	48.55), c(46.5,	46.5,	47.94,	48.02), c(46.77,	46.88,	46.94,	46.98))
Janda_EWR <- t(Janda_EWR)
colnames(Janda_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("A.scoparia", "B.ilicifolia", "E.rudis", "H. angustifolium", "M.preissiana", "P.ellipticum")
Janda_EWR <- cbind.data.frame(Species, Janda_EWR)
Janda.thresh <- 44.3
Janda.prop <- 44.3
EWR_AHD <- c(43.54, NA, NA, 42.57, 43.48, NA)
Janda_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Janda_EWR_plot <- ggplot(Janda_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Janda.thresh, linetype="dotted") +
  geom_errorbar(data=Janda_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
    annotate("text", x = 0, y = Janda.thresh, vjust=-0.75, hjust= -0.25, label = "Current") +
  theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

#Joondalup

Joonda_EWR <- cbind(c(16.7,	17.02,	18.1,	19.1), c(17.24,	17.78,	19, 19.2), c(18.14,	18.45,	18.97,	19.2),
                   c(16.7,	16.84,	19.23, 20.83), c(NA, NA, NA, NA))
Joonda_EWR <- t(Joonda_EWR)
colnames(Joonda_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("B.articulata", "B.juncea", "L. longitudinale", "M.rhaphiophylla", "E. rudis")
Joonda_EWR <- cbind.data.frame(Species, Joonda_EWR)
Joonda.thresh <- 15.8
Joonda.prop <- 16.2
EWR_AHD <- c(15.88, 14.75, NA, 15.86, 14.74)
Joonda_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Joonda_EWR_plot <- ggplot(Joonda_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Joonda.thresh, linetype="dotted") +
  geom_hline(yintercept=Joonda.prop, linetype="dashed") +
  geom_errorbar(data=Joonda_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
  annotate("text", x = 0, y = Joonda.thresh, vjust=+1.5, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Joonda.prop, vjust=-0.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

#Lexia186

Lex186_EWR <- cbind(c(49.92,	49.93,	50.62,	50.64), c(49.93,	49.93,	50.54,	50.58), c(49.92,	49.95,	50.64,	50.64),
                    c(50.09,	50.11,	50.54,	50.54), c(49.95,	49.95,	50.61,	50.62), c(49.92,	49.93,	50.63,	50.64))
Lex186_EWR <- t(Lex186_EWR)
colnames(Lex186_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("A.scoparia", "B.ilicifolia", "H. angustifolium", "L. longitudinale", 
             "M.preissiana", "P.ellipticum")
Lex186_EWR <- cbind.data.frame(Species, Lex186_EWR)
Lex186.thresh <- 47.2
Lex186.prop <- 46.5
EWR_AHD <- c(47.67, NA, 46.57, NA, 47.31, 47.71)
Lex186_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Lex186_EWR_plot <- ggplot(Lex186_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Lex186.thresh, linetype="dotted") +
  geom_hline(yintercept=Lex186.prop, linetype="dashed") +
  geom_errorbar(data=Lex186_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
    annotate("text", x = 0, y = Lex186.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Lex186.prop, vjust=-0.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

# McNess
McNess_EWR <- cbind(c(7.08,	7.09,	7.78,	7.85), c(6.52,	6.46,	7.72,	7.83), c(7.36,	7.57,	7.78,	7.83),
                    c(6.43,	6.43,	7.59,	7.7))
McNess_EWR <- t(McNess_EWR)
colnames(McNess_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("B.juncea", "E.rudis", "L. longitudinale", "M.rhaphiophylla")
McNess_EWR <- cbind.data.frame(Species, McNess_EWR)
McNess.thresh <- 6.95
McNess.prop <- 6.2
EWR_AHD <- c(NA, NA, NA, NA)
McNess_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

McNess_EWR_plot <- ggplot(McNess_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=McNess.thresh, linetype="dotted") +
  geom_hline(yintercept=McNess.prop, linetype="dashed") +
  #geom_errorbar(data=McNess_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
    annotate("text", x = 0, y = McNess.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = McNess.prop, vjust=-0.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

# Mariginiup
Marig_EWR <- cbind(c(41.55,	41.55,	41.83,	41.83), c(41.55,	41.73,	41.84,	41.86), c(NA, NA, NA, NA))
Marig_EWR <- t(Marig_EWR)
colnames(Marig_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("B.articulata", "E.rudis", "T. orientalis")
Marig_EWR <- cbind.data.frame(Species, Marig_EWR)
Marig.thresh <- 41.5
Marig.prop <- 42.1
EWR_AHD <- c(40.28, 40.34, 40.55)
Marig_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Marig_EWR_plot <- ggplot(Marig_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Marig.thresh, linetype="dotted") +
  geom_hline(yintercept=Marig.prop, linetype="dashed") +
  geom_errorbar(data=Marig_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
  annotate("text", x = 0, y = Marig.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Marig.prop, vjust=+1.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

# Nowergup

Nower_EWR <- cbind(c(16.03,	16.39,	17.17,	18), c(16.58,	16.99,	17.22,	17.5), c(15.43,	16.64,	18.53,	19.13),
                    c(16.11,	16.52,	17.52,	18), c(14.77,	16.59,	17.32,	19.31), c(14.5,	14.95,	16.21,	16.97))
Nower_EWR <- t(Nower_EWR)
colnames(Nower_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("B.articulata", "B.juncea", "E.rudis", "L. longitudinale", "M.rhaphiophylla", "T.orientalis")
Nower_EWR <- cbind.data.frame(Species, Nower_EWR)
Nower.thresh <- 16.8
Nower.prop <- 16
EWR_AHD <- c(15.22, NA, 14.64, NA, 14.66, 14.44)
Nower_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Nower_EWR_plot <- ggplot(Nower_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Nower.thresh, linetype="dotted") +
  geom_hline(yintercept=Nower.prop, linetype="dashed") +
  geom_errorbar(data=Nower_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
  annotate("text", x = 0, y = Nower.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Nower.prop, vjust=+1.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

#Wilgarup
Wilga_EWR <- cbind(c(6.03,	6.03,	6.69,	6.69), c(6.72,	6.73,	6.74,	6.75), c(6.69,	6.69,	6.81,	6.81),
                    c(6.07,	6.72,	7.07,	7.14), c(6.03,	6.14,	6.78,	7))
Wilga_EWR <- t(Wilga_EWR)
colnames(Wilga_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("B.articulata", "B.juncea", "B.littoralis", "L. longitudinale", "M.rhaphiophylla")
Wilga_EWR <- cbind.data.frame(Species, Wilga_EWR)
Wilga.thresh <- 4.5
Wilga.prop <- 3.9
EWR_AHD <- c(4.81, 3.9, NA, NA, 3.89)
Wilga_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Wilga_EWR_plot <- ggplot(Wilga_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Wilga.thresh, linetype="dotted") +
  geom_hline(yintercept=Wilga.prop, linetype="dashed") +
  geom_errorbar(data=Wilga_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
    annotate("text", x = 0, y = Wilga.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Wilga.prop, vjust=-0.5, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

# Yonderup
Yonda_EWR <- cbind(c(7.4,	7.53,	7.45,	7.56), c(7.59,	7.58,	7.56,	7.55), c(7.6,	7.6,	7.54,	7.56))
Yonda_EWR <- t(Yonda_EWR)
colnames(Yonda_EWR) <- c("absolute_min", "mean_min", "mean_max", "absolute_max")
Species <- c("B.juncea", "B.littoralis", "M.rhaphiophylla")
Yonda_EWR <- cbind.data.frame(Species, Yonda_EWR)
Yonda.thresh <- 5.9
Yonda.prop <- 5.7
EWR_AHD <- c(NA, NA, NA)
Yonda_FroendEWR <- cbind.data.frame(Species, EWR_AHD)

Yonda_EWR_plot <- ggplot(Yonda_EWR) +
  geom_segment(aes(x=Species, xend=Species, y=mean_max, yend=mean_min), size=8, color="orange") +
  geom_segment(aes(x=Species, xend=Species, y=absolute_max, yend=absolute_min), size=3, color="red") +
  geom_hline(yintercept=Yonda.thresh, linetype="dotted") +
  geom_hline(yintercept=Yonda.prop, linetype="dashed") +
  #geom_errorbar(data=Yonda_FroendEWR , aes(y=NULL, ymax=EWR_AHD, ymin=EWR_AHD, x=Species), width=0.5, color="blue") +
  annotate("text", x = 0, y = Yonda.thresh, vjust=-0.75, hjust= -0.25, label = "Current") + 
  annotate("text", x = 0, y = Yonda.prop, vjust=-0.35, hjust= -0.2, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Elevation (mAHD)") +
  theme(axis.text.x=element_text(angle=90, face="italic"))

