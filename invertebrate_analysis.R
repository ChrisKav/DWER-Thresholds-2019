library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(grid)
library(lubridate)
library(dplyr)
library(vegan)
library(mvabund)
library(cowplot)
library(analogue)
library(gridExtra)
library(gratia)
library(reshape2)
library(janitor)
library(boral)

options(stringsAsFactors=F)

source("functions.R")

#1. Import data with wetlands as different lists
invert.raw <- read_excel_allsheets("GMEMP Spring Macro Data.xlsx")

inv.meta <- invert.raw[[1]] # remove metadata data.frame
invert <- invert.raw[c(2:6,8:10)] # select wetlands only - excluding Gnangara

invert <- lapply(invert, function(x) {
  names(x) <- substring(names(x), 4) # Remove three letter abreviation from columns
  colnames(x)[1] <- "Family" # rename family column
  Family <- x$Family # make new vector of family names
  x$Family <- NULL # remove family names from data
  colnames(x) <- recode(colnames(x), "2"="1996", "4"="1997", "6"="1998", "8"="1999",
                        "10"="2000", "12"="2001", "14"="2002", "16"="2003",
                        "18"="2004", "20"="2005", "22"="2006", "24"="2007",
                        "26"="2008", "28"="2009", "30"="2010", "32"="2011",
                        "34"="2012", "36"="2013", "38"="2014", "40"="2015",
                        "42"="2016", "44"="2017", "46"="2018") # Recode column names to year
  x <- t(x) #transpose to make years rows
  colnames(x) <- Family # make column names Family names
  x <- x[, colSums(x) !=0] # delete all Families not present in wetlands
  return(x)
})

# Build richness ~ time plots

richness <- lapply(invert, function(x) {
  richness <- as.numeric(rowSums(x >0))
  year <- as.numeric(rownames(x))
  x <- cbind(year, richness)
  return(x)
}) 
  
# Wetland specific ordination

Y.wetlands <- lapply(invert, function(x) {
  x <- round(x, 0) # round all data to nearest value and remove taxa with zero
  x <- x[, colSums(x) !=0] # delete all Families not present in wetlands
})

# Entire system ordination

Y2 <- t(invert.raw[[11]]) #Capture all site data and transpose
colnames(Y2) <- Y2[1,] # Make colnames Families
Y2 <- data.frame(Y2[2:nrow(Y2),]) # Delete row of family names
Y2$samples <- rownames(Y2) # make vector of sample names
Y2$wetland <- substr(Y2$samples, 1, 3) # make vector of wetland names
Y2 <- subset(Y2, ! wetland=="GNA") # remove Gnangara
Y2$year <- substr(Y2$samples, 4, 5) # extract round number
Y2$year <- recode(Y2$year, "2"="1996", "4"="1997", "6"="1998", "8"="1999",
                      "10"="2000", "12"="2001", "14"="2002", "16"="2003",
                      "18"="2004", "20"="2005", "22"="2006", "24"="2007",
                      "26"="2008", "28"="2009", "30"="2010", "32"="2011",
                      "34"="2012", "36"="2013", "38"="2014", "40"="2015",
                      "42"="2016", "44"="2017", "46"="2018") # Recode rounds to year
complete.meta <- Y2[,c("samples", "wetland", "year")]
Y2$samples <- NULL
Y2$wetland <- NULL
Y2$year <- NULL
Y2 <- data.frame(apply(Y2, 2, function(x) as.numeric(as.character(x)))) #make data.frame numeric
Y2 <- round(Y2, 0) # Round to nearest value. Not <0.5 will become zero ~ 20 families (singletons essentially)
Y2 <- Y2[, colSums(Y2) !=0] # Delete all families with 0 abundance

inv.mod <- boral(Y2,
                  X=NULL,
                  family="negative.binomial",
                  lv.control = list(num.lv = 2),
                  mcmc.control = mcmc_control,
                  row.ids = cbind(complete.meta[,2:3]),
                  save.model=FALSE,
                  model.name=NULL)

XData <- complete.meta[,2:3]
rownames(XData) <- 1:nrow(XData)
colnames(XData) <- c("Plot", "Year")
XData$Year <- as.factor(XData$Year)
mod1.ext <- boral.extract(inv.mod, XData)
ord.plot <- boral.plots(mod1.ext)

ord.data <- mod1.ext$lv
ord.data$Year <- as.numeric(as.character(ord.data$Year))
d <- data.table(ord.data)
d1 <- d[, min(Year, na.rm=TRUE), by=Plot]
d2 <- d[, max(Year, na.rm=TRUE), by=Plot]
d1 <- data.frame(d1)
d2 <- data.frame(d2)
d1$Year <- d1$V1
d2$Year <- d2$V1
d1$V1 <- NULL
d2$V1 <- NULL
min.yr <- dplyr::inner_join(ord.data, d1)
min.yr$fun <- "min"
max.yr <- dplyr::inner_join(ord.data, d2)
max.yr$fun <- "max"
plot.data <- rbind(min.yr, max.yr)

complete.invert.plot <- plot.data %>% 
  arrange(Year) %>% #sort ascending so that 2018 is plotted last
  ggplot() +
  theme_bw() +
  geom_point(data=ord.data, aes(x=LV1, y=LV2, colour = Plot, group = Plot), size = 1) +
  geom_path(data=plot.data, aes(x=LV1, y=LV2, colour = Plot, group = Plot),
            arrow = arrow(length = unit(0.55, "cm")), show.legend = FALSE, size = 1) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(colour="Wetland")

extc.data <- cbind(Y2, complete.meta)
extc.data <- subset(extc.data, ! wetland==c("MAR"))
extc.data <- subset(extc.data, ! wetland==c("MEL")) # remove Mariginiup and EMP173 due to inadequate sampling years
extc.data$year <- as.numeric(extc.data$year)
extc.data <- extc.data[with(extc.data, !(year %in% 2001:2013)), ]
period1 <- extc.data[with(extc.data, (year %in% 1996:2000)), ]
period2 <- extc.data[with(extc.data, (year %in% 2014:2018)), ]
period1 <- aggregate(period1[,1:74 ], by=list(Wetland=period1$wetland), FUN=sum)
period2 <- aggregate(period2[,1:74 ], by=list(Wetland=period2$wetland), FUN=sum)
period1$richness <- as.numeric(rowSums(period1[,1:74] >0 ))
period2$richness <- as.numeric(rowSums(period2[,1:74] >0 ))
ext.plot.data <- data.frame(cbind(period1$Wetland,period1$richness,period2$richness))
ext.plot.data$X2 <- as.numeric(levels(ext.plot.data$X2))[ext.plot.data$X2]
ext.plot.data$X3 <- as.numeric(levels(ext.plot.data$X3))[ext.plot.data$X3]

extc.rich <- ggplot(data=ext.plot.data, aes(x=X2, y=X3), group=X1) +
  geom_point() +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  scale_x_continuous(name= "1996-2000 Family Richness", limits=c(24,32), breaks=seq(24,32,1)) +
  scale_y_continuous(name= "2014-2018 Family Richness",limits=c(22,32), breaks=seq(22,32,1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  annotate("text", x = 24, y = 28, vjust=+1.25, hjust=-0.25, label = "GOO") + 
  annotate("text", x = 31, y = 25, vjust=+1.25, hjust=+1.25, label = "JAN")  +
  annotate("text", x = 31, y = 30, vjust=+1.25, hjust=+1.25, label = "JOO") + 
  annotate("text", x = 28, y = 32, vjust=+1.25, hjust=+1.25, label = "MCS") +
  annotate("text", x = 32, y = 22, vjust=+1.25, hjust=+1.25, label = "NOW") + 
  annotate("text", x = 30, y = 25, vjust=+1.25, hjust=+1.25, label = "YON")

period1$Wetland <- NULL
period1$richness <- NULL
period2$Wetland <- NULL
period2$richness <- NULL

setdiff(colnames(period1[,colSums(period1) > 0]),
  colnames(period2[,colSums(period2) > 0]))

setdiff(colnames(period2[,colSums(period2) > 0]),
        colnames(period1[,colSums(period1) > 0]))


save(invert, richness, Y, Y2, inv.mod, complete.invert.plot, extc.rich, file="invertebrate_data.RData")
