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

save(invert, richness, Y, Y2, inv.mod, complete.invert.plot, file="invertebrate_data.RData")
