library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)

rain.raw <- read.csv("IDCJAC0009_009021_1800_Data.csv", header = TRUE)
rain <- rain.raw[,3:6]
rain <- na.omit(rain)
rain <- subset(rain, Year >= 1950)
rain <- subset(rain, Year <= 2018)
colnames(rain)[c(2,4)] <- c("nMonth", "Rainfall")
rain <- rain %>%
  group_by(Year, nMonth) %>%
  summarise(Rainfall=(sum(Rainfall)))
rain <- data.frame(rain)
rain$Date <- paste("15", rain$nMonth, rain$Year, sep="-") %>% dmy() %>% as.Date()
rain <- transform(rain, Time = as.numeric(Date)/100)
rain$Month <- lubridate::month(rain$Date, label=TRUE, abbr = TRUE)

rainAnn <- rain %>%
  group_by(Year) %>%
  summarise(Rainfall=(sum(Rainfall)))

rain.plot <- ggplot(rainAnn, aes(Year, Rainfall)) + 
  geom_point(position=position_jitter(1,3), pch=21, fill="#FF0000AA") +
  geom_line(aes(y=rollmean(Rainfall, 5, na.pad=TRUE))) +
  geom_hline(yintercept = mean(rainAnn$Rainfall), linetype="dotted") +
  scale_x_continuous(breaks = round(seq(1950, 2020, by = 10),0)) +
  scale_y_continuous(breaks = round(seq(450, max(rainAnn$Rainfall), by = 50),0)) +
  ylab("Annual Rainfall (mm)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

save(rain.plot, rain.raw, rain, rainAnn, file="rainfall_trends.RData")
