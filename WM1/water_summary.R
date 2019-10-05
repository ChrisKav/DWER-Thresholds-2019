library(Hmsc)
library(stringr)
library(dplyr)
library(knitr)
library(corrplot)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(boral)
library(data.table)
library(plotly)
library(lubridate)

load("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds/Refined_data.RData")

wat.obs <- data.ls$'61610833'

# Water level summary

sw <- na.omit(wat.obs)

DATE1 <- as.Date("1994-08-01")
DATE2 <- as.Date("1999-07-31")
DATE3 <- as.Date("1999-08-01")
DATE4 <- as.Date("2004-07-31")
DATE5 <- as.Date("2004-08-01")
DATE6 <- as.Date("2009-07-31")
DATE7 <- as.Date("2009-08-01")
DATE8 <- as.Date("2014-07-31")
DATE9 <- as.Date("2014-08-01")
DATE10 <- as.Date("2019-07-31")
sw.l <- list(subsetDate(sw, DATE1,DATE2),
             subsetDate(sw, DATE3,DATE4),
             subsetDate(sw, DATE5,DATE6),
             subsetDate(sw, DATE7,DATE8),
             subsetDate(sw, DATE9,DATE10))
sw.l <- lapply(sw.l, function(x) {
  sapply(seq(min(x$Year), max(x$Year)-1), function(yr) {
    subset(x, (Year == yr & Month_num >= 8) | (Year == yr+1 & Month_num <= 7))
  }, simplify=F)
})
sw.l <- lapply(sw.l, function(x) rbindlist(x, idcol = "index"))
sw.l <- lapply(sw.l, function(x) {
  x$index <- factor(x$index)
  return(x)
}
)

sw.sum <- lapply(sw.l, function(x) water_level_5yr_summary(x))
sw.sum <- do.call(rbind, sw.sum)
sw.sum$Period = c("08/1994 - 07/1999", "08/1999 - 07/2004", "08/2004 - 07/2009",
                  "08/2009 - 07/2014", "08/2014 - 07/2019")
sw.sum <- sw.sum[,c(7,1:6)]
colnames(sw.sum) <- c("Period",
                      "Mean max seasonal level (mAHD)",
                      "Mean min seasonal level (mAHD)",
                      "Mean seasonal change (m)",
                      "Month of maximum",
                      "Month of minimum",
                      "Mean max to min (days)")

write.table(sw.sum, file = "WM1/5_yr_water_summary.txt", sep=",")


WM1.AHD <- list(AHD$'61610833')
WM1.AHD[[1]]$group <- "ground"
WM1.AHD <- rbind(WM1.AHD[[1]])

WM1.params <- list(AHD.params$'61610833')
WM1.params[[1]]$group <- "ground"
WM1.params <- rbind(WM1.params[[1]])

wm1.p <- ggplot(WM1.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(colour=group)) +
  geom_point(WM1.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(WM1.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                           group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(WM1.params, mapping=aes(x=Date, y=p3)) +
  geom_line(WM1.params, mapping=aes(x=Date, y=incr2), color="blue") +
  geom_line(WM1.params, mapping=aes(x=Date, y=decr2), color = "red") +
  labs(x = "Year", y = expression("Water Level" ~ (mAHD))) +
  geom_hline(yintercept = c(55.7,53.7), linetype= c("dotted")) +
  annotate("text", x = as.Date("2018-01-01"), y = 55.7, vjust=-2.2, label = "Current") + 
  annotate("text", x = as.Date("2018-01-01"), y = 53.7, vjust=-1, label = "Proposed") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

save(sw, sw.l, sw.sum, wm1.p, file="WM1/water_level.RData")
