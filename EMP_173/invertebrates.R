library(boral)
library(ggplot2)
library(ggrepel)
library(zoo)

source("functions.R")
load("invertebrate_data.RData")

# ORDINATION

Y.wetlands
Y2 <- Y.wetlands[[8]]
meta <- rownames(Y2)

inv.mod <- boral(Y2,
                 X=NULL,
                 family="negative.binomial",
                 lv.control = list(num.lv = 2),
                 mcmc.control = mcmc_control,
                 row.ids = cbind(meta),
                 save.model=FALSE,
                 model.name=NULL)

XData <- data.frame(meta)
rownames(XData) <- 1:nrow(XData)
colnames(XData) <- c("Year")
XData$Year <- as.factor(XData$Year)
mod1.ext <- boral.extract2(inv.mod, XData)
ord.plot <- boral.plots2(mod1.ext)

# RICHNESS

rich <- data.frame(richness[[1]])
colnames(rich)[[1]] <- "Year"

rich.plot <- ggplot(rich, aes(Year, richness)) + 
  geom_point(position=position_jitter(1,3), pch=21, fill="#FF0000AA") +
  geom_line(aes(y=rollmean(richness, 3, na.pad=TRUE))) +
  geom_hline(yintercept = mean(rich$richness), linetype="dotted") +
  scale_x_continuous(breaks = round(seq(min(rich$Year), 2018, by = 2),0)) +
  scale_y_continuous() +
  ylab("Family richness") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

strat.plot <- stratiplotter2(inv.mod)

save(inv.mod, ord.plot, rich.plot, strat.plot, file="EMP_173/Invert_analysis.RData")
