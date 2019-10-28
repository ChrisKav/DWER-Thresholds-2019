library(dplyr)
library(lubridate)
library(ggplot2)
library(boral)
library(data.table)
library(corrplot)

load("Refined_data.RData")

setwd("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds/All_vegetation")

lst <- comm

lst <- list(lst[['Goolelal']] , lst[['Loch_McNess']], lst[['Yonderup']], lst[['Joondalup_Nth']], 
       lst[['Joondalup_Sth']], lst[['Mariginiup']], lst[['Jandabup']])
names(lst) <- c('Goolelal', 'Loch_McNess', 'Yonderup', 'Joondalup_Nth', 'Joondalup_Sth', 'Mariginiup', 'Jandabup')

lst <- lapply(lst, function(x) {
  x$Plot <- x$plot
  return(x)
})

lst[[1]]$plot[lst[[1]]$plot=="A"] <- 26.66
lst[[1]]$plot[lst[[1]]$plot=="B"] <- 26.79
lst[[1]]$plot[lst[[1]]$plot=="C"] <- 26.855
lst[[1]]$plot[lst[[1]]$plot=="D"] <- 26.973

lst[[2]]$plot[lst[[2]]$plot=="A"] <- 7.028
lst[[2]]$plot[lst[[2]]$plot=="B"] <- 7.383
lst[[2]]$plot[lst[[2]]$plot=="C"] <- 7.709
lst[[2]]$plot[lst[[2]]$plot=="D"] <- 7.828
lst[[2]]$plot[lst[[2]]$plot=="Z"] <- 6.6

lst[[3]]$plot[lst[[3]]$plot=="A"] <- 7.59
lst[[3]]$plot[lst[[3]]$plot=="B"] <- 7.511
lst[[3]]$plot[lst[[3]]$plot=="C"] <- 7.413
lst[[3]]$plot[lst[[3]]$plot=="D"] <- 7.465

lst[[4]]$plot[lst[[4]]$plot=="A"] <- 17.33
lst[[4]]$plot[lst[[4]]$plot=="B"] <- 18.849
lst[[4]]$plot[lst[[4]]$plot=="C"] <- 20.275
lst[[4]] <- lst[[4]][lst[[4]]$plot != "D", , drop=TRUE] # No plot elevation for plot D

lst[[5]]$plot[lst[[5]]$plot=="A"] <- 16.9
lst[[5]]$plot[lst[[5]]$plot=="B"] <- 17.2
lst[[5]]$plot[lst[[5]]$plot=="C"] <- 17.767
lst[[5]]$plot[lst[[5]]$plot=="D"] <- 18.794

lst[[6]]$plot[lst[[6]]$plot=="A"] <- 41.63
lst[[6]]$plot[lst[[6]]$plot=="B"] <- 41.812
lst[[6]]$plot[lst[[6]]$plot=="C"] <- 41.864
lst[[6]] <- lst[[6]][lst[[6]]$plot != "E", , drop=TRUE] # No plot elevation for plot E
lst[[6]] <- lst[[6]][lst[[6]]$plot != "D", , drop=TRUE] # No plot elevation for plot D

lst[[7]]$plot[lst[[7]]$plot=="A"] <- 44.97
lst[[7]]$plot[lst[[7]]$plot=="B"] <- 47.00 #median value as there are some low values for this site. mean = 39.55
lst[[7]]$plot[lst[[7]]$plot=="C"] <- 48.565
lst[[7]]$plot[lst[[7]]$plot=="D"] <- 48.255

lst <- lapply(lst, function(x) {
  x$plot <- as.numeric(x$plot)
  names(x)[names(x) == "plot"] <- "Elevation"
  return(x)
})

# 4 add GAM modeled water level fo reach wetland. (Do we just do this with surface water plots?)
AHD.levels <- list(Params.data.list$Goollelal.params, Params.data.list$McNess.params, Params.data.list$Yonderup.params,
     Params.data.list$Joondalup.params, Params.data.list$Joondalup.params, Params.data.list$Mariginup.params,
     Params.data.list$Jandabup.params)
names(AHD.levels) <- c('Goolelal', 'Loch_McNess', 'Yonderup', 'Joondalup_Nth', 'Joondalup_Sth', 'Mariginiup', 'Jandabup')

AHD.levels <- lapply(AHD.levels, function(x) {
  x <- subset(x, group=="surface")
  x$Year <- year(x$Date)
  x <- aggregate(x$p3, list(x$Year), mean)
  colnames(x) <- c("year", "mAHD")
  return(x)
})

AHD.levels <- Map(function(x,y){
  df <-  subset(x, year %in% y$year)
  return(df)
}, AHD.levels, lst)

# 5 subtract plot elevation from water level

lst <- Map(function(x,y) {
  df <- merge(x, y, by = "year", all.x=TRUE, all.y=TRUE)
  df$WL <- df$Elevation - df$mAHD
  return(df)
}, lst, AHD.levels)

# 6 use dplyr::bind_rows to merge dataset into a single dataframe
comm.df <- bind_rows(lst, .id="wetland")
comm.df[is.na(comm.df)] <- 0
summary(comm.df$WL)

b.articulata <- comm.df[,c(1,2, 45:47, 34)]
b.articulata <- b.articulata[apply(b.articulata, 1, function(row) all(row !=0 )),]

b.attenuata <- comm.df[,c(1,2, 45:47, 142)]
b.attenuata <- b.attenuata[apply(b.attenuata, 1, function(row) all(row !=0 )),]

m.rhaphiophyla <- comm.df[,c(1,2, 45:47, 40)]
m.rhaphiophyla <- m.rhaphiophyla[apply(m.rhaphiophyla, 1, function(row) all(row !=0 )),]

e.rudis <- comm.df[,c(1,2, 45:47, 37)]
e.rudis <- e.rudis[apply(e.rudis, 1, function(row) all(row !=0 )),]

b.juncea <- comm.df[,c(1,2, 45:47, 81)]
b.juncea <- b.juncea[apply(b.juncea, 1, function(row) all(row !=0 )),]

t.orientalis <- comm.df[,c(1,2, 45:47, 29)]
t.orientalis <- t.orientalis[apply(t.orientalis, 1, function(row) all(row !=0 )),]

l.longitudinale <- comm.df[,c(1,2, 45:47, 95)]
l.longitudinale <- l.longitudinale[apply(l.longitudinale, 1, function(row) all(row !=0 )),]

m.preissiana <- comm.df[,c(1,2, 45:47, 149)]
m.preissiana <- m.preissiana[apply(m.preissiana, 1, function(row) all(row !=0 )),]

a.scoparia <- comm.df[,c(1,2, 45:47, 244)]
a.scoparia <- a.scoparia[apply(a.scoparia, 1, function(row) all(row !=0 )),]

h.angustifoliuma <- comm.df[,c(1,2, 45:47, 268)]
h.angustifoliuma <- h.angustifoliuma[apply(h.angustifoliuma, 1, function(row) all(row !=0 )),]

b.littoralis <- comm.df[,c(1,2, 45:47, 143)]
b.littoralis <- b.littoralis[apply(b.littoralis, 1, function(row) all(row !=0 )),]

p.ellipticum <- comm.df[,c(1,2, 45:47, 282)]
p.ellipticum <- p.ellipticum[apply(p.ellipticum, 1, function(row) all(row !=0 )),]

b.ilicifolia <- comm.df[,c(1,2, 45:47, 246)]
b.ilicifolia <- b.ilicifolia[apply(b.ilicifolia, 1, function(row) all(row !=0 )),]

b.menziesii <- comm.df[,c(1,2, 45:47, 194)]
b.menziesii <- b.menziesii[apply(b.menziesii, 1, function(row) all(row !=0 )),]

a.saligna <- comm.df[,c(1,2, 45:47, 32)]
a.saligna <- a.saligna[apply(a.saligna, 1, function(row) all(row !=0 )),]

x.preissii <- comm.df[,c(1,2, 45:47, 152)]
x.preissii <- x.preissii[apply(x.preissii, 1, function(row) all(row !=0 )),]

wetland.sp <- list(b.articulata, b.juncea, b.attenuata, m.rhaphiophyla, t.orientalis, l.longitudinale, m.preissiana, a.scoparia, h.angustifoliuma, 
     b.littoralis, p.ellipticum, b.ilicifolia, b.menziesii, a.saligna, x.preissii, e.rudis)
names(wetland.sp) <- c(colnames(wetland.sp[[1]])[6], colnames(wetland.sp[[2]])[6], colnames(wetland.sp[[3]])[6], colnames(wetland.sp[[4]])[6], 
                       colnames(wetland.sp[[5]])[6], colnames(wetland.sp[[6]])[6], colnames(wetland.sp[[7]])[6], colnames(wetland.sp[[8]])[6],
                       colnames(wetland.sp[[9]])[6], colnames(wetland.sp[[10]])[6], colnames(wetland.sp[[11]])[6], colnames(wetland.sp[[12]])[6],
                       colnames(wetland.sp[[13]])[6], colnames(wetland.sp[[14]])[6], colnames(wetland.sp[[15]])[6], colnames(wetland.sp[[16]])[6])
wetland.sp <- lapply(wetland.sp, function(x) {
  colnames(x)[6] <- "abundance"
  return(x)
})
wetland.sp <- bind_rows(wetland.sp, .id="species")
wetland.sp[is.na(wetland.sp)] <- 0
wetland.sp <- filter(wetland.sp, abundance > 0)

min <- min(comm.df$WL)
max <- max(comm.df$WL)

species.names <- c("Acacia.saligna" = "Acacia saligna",
                   "Banksia.attenuata" = "Banksia attenuata",
                   "Banksia.menziesii" = "Banksia menziesii",
                   "Baumea.articulata" = "Baumea articulata",
                   "Baumea.juncea" = "Baumea juncea",
                   "Eucalyptus.rudis" = "Eucalyptus rudis",
                   "Lepidosperma.longitudinale" = "Lepidosperma longitudinale",
                   "Melaleuca.rhaphiophylla" = "Melaleuca rhaphiophylla")

wetland.height.plot <- wetland.sp %>%
  filter(species !="Xanthorrhoea.preissii") %>%
  filter(species !="X.Typha.orientalis") %>%
  filter(species !="Pericalymma.ellipticum")%>%
  filter(species !="Melaleuca.preissiana")%>%
  filter(species !="Hypocalymma.angustifolium")%>%
  filter(species !="Astartea.scoparia")%>%
  filter(species !="Banksia.ilicifolia")%>%
  filter(species !="Banksia.littoralis")%>%
  mutate(wetland=recode(wetland, "Joondalup_Nth" = "Lake Joondalup")) %>%
  mutate(wetland=recode(wetland, "Joondalup_Sth" = "Lake Joondalup")) %>%
  mutate(wetland=recode(wetland, "Loch_McNess" = "Loch McNess")) %>%
  mutate(wetland=recode(wetland, "Goolelal" = "Lake Goollelal")) %>%
  mutate(wetland=recode(wetland, "Yonderup" = "Lake Yonderup")) %>%
  mutate(wetland=recode(wetland, "Mariginiup" = "Lake Mariginiup")) %>%
  mutate(wetland=recode(wetland, "Jandabup" = "Lake Jandabup")) %>%
  ggplot(aes(x=wetland, y=WL, fill=wetland)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Height above surface water (m)") + xlab(NULL) +
  facet_wrap(~ species, ncol = 2, labeller = as_labeller(species.names)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype="dotted") +
  coord_flip()

####################################
# Ordination

###########################################
# Sum all plots together

df <- bind_rows(comm, .id="wetland")
df[is.na(df)] <- 0

df <- rbind(subset(df, wetland=="Goolelal"), subset(df, wetland=="Loch_McNess"), subset(df, wetland=="Yonderup"), subset(df, wetland=="Joondalup_Nth"),
      subset(df, wetland=="Joondalup_Sth"), subset(df, wetland=="Mariginiup"), subset(df, wetland=="Jandabup"), subset(df, wetland=="Nowergup_Sth"),
      subset(df, wetland=="Nowergup_Nth"), subset(df, wetland=="Wilgarup"), subset(df, wetland=="Lexia186"), subset(df, wetland=="EPP173"),
      subset(df, wetland=="Dampland_78"), subset(df, wetland=="Goolelal"), subset(df, wetland=="Gwelup"), subset(df, wetland=="Quin_Brook"))

df <- df[,!grepl("^X",names(df))]
df <- subset(df, !plot=="a")
df <- subset(df, !plot=="E")
df <- subset(df, !plot=="X")
df <- subset(df, !plot=="Y")
df <- subset(df, !plot=="Z")
df <- subset(df, !plot=="H")
df <- subset(df, !plot=="J")
df <- subset(df, !plot=="K")
df <- subset(df, !plot=="M")
df$plot <- NULL
df$wetland <- recode(df$wetland, "Joondalup_Nth" = "Joondalup")
df$wetland <- recode(df$wetland, "Joondalup_Sth" = "Joondalup")
df$wetland <- recode(df$wetland, "Nowergup_Nth" = "Nowergup")
df$wetland <- recode(df$wetland, "Nowergup_Sth" = "Nowergup")
df <- aggregate(.~wetland+year, df, mean)
df <- df[-c(4,13),] #Remove Dampland_78 97 and 98 due to lack of WL data
df.meta <- df[,1:2]
df[,1:2] <- NULL
df <- round(df,0)
df <- df[, colSums(df) > 5]

uncon.mod <- boral(df,
              X=NULL,
              family="negative.binomial",
              lv.control = list(num.lv = 2),
              mcmc.control = mcmc_control,
              row.ids = cbind(1:nrow(df), df.meta),
              save.model=FALSE,
              model.name=NULL)

lvsplot(uncon.mod, ind.spp = 20)

XData <- df.meta
rownames(XData) <- 1:nrow(XData)
colnames(XData) <- c("Plot", "Year")
XData$Year <- as.factor(XData$Year)
mod1.ext <- boral.extract(uncon.mod, XData)
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

ord.data$Plot <- recode(ord.data$Plot, "Dampland_78" = "Melaleuca Park 78")
ord.data$Plot <- recode(ord.data$Plot, "EPP173" = "Melaleuca Park 173")
ord.data$Plot <- recode(ord.data$Plot, "Goolelal" = "Lake Goollelal")
ord.data$Plot <- recode(ord.data$Plot, "Gwelup" = "Lake Gwelup")
ord.data$Plot <- recode(ord.data$Plot, "Jandabup" = "Lake Jandabup")
ord.data$Plot <- recode(ord.data$Plot, "Joondalup" = "Lake Joondalup")
ord.data$Plot <- recode(ord.data$Plot, "Lexia186" = "Lexia 186")
ord.data$Plot <- recode(ord.data$Plot, "Loch_McNess" = "Loch McNess")
ord.data$Plot <- recode(ord.data$Plot, "Mariginiup" = "Lake Mariginiup")
ord.data$Plot <- recode(ord.data$Plot, "Nowergup" = "Lake Nowergup")
ord.data$Plot <- recode(ord.data$Plot, "Quin_Brook" = "Quin Brook")
ord.data$Plot <- recode(ord.data$Plot, "Wilgarup" = "Lake Wilgarup")
ord.data$Plot <- recode(ord.data$Plot, "Yonderup" = "Lake Yonderup")

plot.data$Plot <- recode(plot.data$Plot, "Dampland_78" = "Melaleuca Park 78")
plot.data$Plot <- recode(plot.data$Plot, "EPP173" = "Melaleuca Park 173")
plot.data$Plot <- recode(plot.data$Plot, "Goolelal" = "Lake Goollelal")
plot.data$Plot <- recode(plot.data$Plot, "Gwelup" = "Lake Gwelup")
plot.data$Plot <- recode(plot.data$Plot, "Jandabup" = "Lake Jandabup")
plot.data$Plot <- recode(plot.data$Plot, "Joondalup" = "Lake Joondalup")
plot.data$Plot <- recode(plot.data$Plot, "Lexia186" = "Lexia 186")
plot.data$Plot <- recode(plot.data$Plot, "Loch_McNess" = "Loch McNess")
plot.data$Plot <- recode(plot.data$Plot, "Mariginiup" = "Lake Mariginiup")
plot.data$Plot <- recode(plot.data$Plot, "Nowergup" = "Lake Nowergup")
plot.data$Plot <- recode(plot.data$Plot, "Quin_Brook" = "Quin Brook")
plot.data$Plot <- recode(plot.data$Plot, "Wilgarup" = "Lake Wilgarup")
plot.data$Plot <- recode(plot.data$Plot, "Yonderup" = "Lake Yonderup")

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

############################ ALL water data

AHD.levels <- list(Params.data.list$EMP78.params, Params.data.list$EMP173.params, Params.data.list$Goollelal.params,
                   Params.data.list$Gwelup.params, Params.data.list$Jandabup.params, Params.data.list$Joondalup.params,
                   Params.data.list$Lexia186.params, Params.data.list$McNess.params, Params.data.list$Mariginup.params,
                   Params.data.list$Nowergup.params, Params.data.list$QuinBrook.params, Params.data.list$Wilgarup.params,
                   Params.data.list$Yonderup.params)

names(AHD.levels) <- c('Dampland_78', 'EPP173', 'Goolelal', 'Gwelup', 'Jandabup', 'Joondalup', 'Lexia186', "Loch_McNess",
                       "Mariginiup", "Nowergup", "Quin_Brook", "Wilgarup", "Yonderup")

AHD.levels[[1]] <- subset(AHD.levels[[1]], group=="ground")
AHD.levels[[2]] <- subset(AHD.levels[[2]], group=="ground")
AHD.levels[[3]] <- subset(AHD.levels[[3]], group=="surface")
AHD.levels[[4]] <- subset(AHD.levels[[4]], group=="surface")
AHD.levels[[5]] <- subset(AHD.levels[[5]], group=="surface")
AHD.levels[[6]] <- subset(AHD.levels[[6]], group=="surface")
AHD.levels[[7]] <- subset(AHD.levels[[7]], group=="ground")
AHD.levels[[8]] <- subset(AHD.levels[[8]], group=="surface")
AHD.levels[[9]] <- subset(AHD.levels[[9]], group=="surface")
AHD.levels[[10]] <- subset(AHD.levels[[10]], group=="surface")
AHD.levels[[11]] <- subset(AHD.levels[[11]], group=="ground")
AHD.levels[[12]] <- subset(AHD.levels[[12]], group=="ground")
AHD.levels[[13]] <- subset(AHD.levels[[13]], group=="surface")

AHD.levels <- lapply(AHD.levels, function(x) {
  x$Year <- year(x$Date)
  x <- aggregate(x$p3, list(x$Year), mean)
  colnames(x) <- c("year", "mAHD")
  return(x)
})

all.ahd <- bind_rows(AHD.levels, .id="wetland")
all.ahd[is.na(all.ahd)] <- 0

all.ahd$combine = as.character(interaction(all.ahd$wetland, all.ahd$year))
df.meta$combine = as.character(interaction(df.meta$wetland, df.meta$year))
all.ahd <- merge(all.ahd, df.meta, by = "combine")

colnames(all.ahd) <- c("wetland", "year", "mAHD")
XData <- data.frame(all.ahd$mAHD)
all.ahd <- all.ahd[,c(1:2)]

con.mod <- boral(df,
                   X=XData,
                   family="negative.binomial",
                   lv.control = list(num.lv = 2),
                   mcmc.control = mcmc_control,
                   row.ids = cbind(1:nrow(df), all.ahd),
                   save.model=TRUE,
                   model.name=NULL)

#########################################                          
#########################################
### DATA EXTRACTION FROM boral MODELS ###
#########################################
#########################################

library(boral)
library(abind)
library(matrixStats)
library(ineq)

################################################
### Define species and coefficient names/ids ###
################################################

species.id <- 1:ncol(Y2)  # vector of species ids [for extraction]
species.names <- colnames(Y2) # vector of species names

Env.id <- 1  # vector of covariate ids (no intercept) [for extraction]
Env.names <- c("intercept", "meanGW") # vector of covariate names incl. intercept

model <- "boral"

############################################
### Extract MCMC samples from jags model ###
############################################

mcmc.samp <- as.mcmc(vegfit.mod3$jags.model) # extracts mcmc from boral object

colnames(mcmc.samp[[1]]) # column names from mcmc object

#######################################
### Standardise posteriors post-hoc ###
#######################################

dataset_sd <- Y2 #read.csv("Eucalypts_sd.csv")   # load in original data sd
dataset_sd <- dataset_sd[,2]
# dataset_sd <- c(1, dataset_sd) # add intercept ##NOT REQUIRED FOR BORAL

mcmc.samp_standardised <- mcmc.samp

# for(i in seq(length(full_standardised$trace$B))){   # species  ## EASIER TO DO DURING EXTRACTON FOR BORAL
#   for(j in seq(length(dataset_sd))){                #covariate
#     full_standardised$trace$B[[i]][,j] <- (full_standardised$trace$B[[i]][,j])/dataset_sd[j]
#   }
# }

##############################################
### Extract regression coefficient samples ###
##############################################

# Not standardised

beta.samp <- matrix(nrow = 1000)

col.names <- vector()

for(i in species.id){ # species
  
  column.name <- sprintf("lv.coefs[%s,1]", i)  # 3 lines code to get species intercept samples
  col.names <- c(col.names, column.name) 
  command <- sprintf('mcmc.samp[[1]][1:1000,"%s"]', column.name)
  tmp <- eval(parse(text = command))
  
  for(j in Env.id){ # variables
    
    column.name2 <- sprintf("X.coefs[%s,%s]", i, j) # 3 lines code to get variables samples (by species)
    col.names <- c(col.names, column.name2)
    command2 <- sprintf('mcmc.samp[[1]][1:1000, "%s"]', column.name2)
    tmp2 <- eval(parse(text = command2))
    
    tmp <- cbind(tmp, tmp2) # create entire matrix for one species
    
  }
  
  beta.samp <- cbind(beta.samp, tmp) # bind all species matrcies together 
  
}

beta.samp <- beta.samp[,-(1)] # remove NA column from beginning
colnames(beta.samp) <- col.names # set column names

# Standardised

beta.samp_standardised <- matrix(nrow = 1000)

col.names_standardised <- vector()

for(i in species.id){ # species
  
  column.name <- sprintf("lv.coefs[%s,1]", i)  # 3 lines code to get species intercept samples
  col.names_standardised <- c(col.names_standardised, column.name) 
  command <- sprintf('mcmc.samp[[1]][1:1000,"%s"]', column.name)
  tmp <- eval(parse(text = command))
  
  for(j in Env.id){ # variables
    
    column.name2 <- sprintf("X.coefs[%s,%s]", i, j) # 3 lines code to get variables samples (by species)
    col.names_standardised <- c(col.names_standardised, column.name2)
    command2 <- sprintf('(mcmc.samp[[1]][1:1000, "%s"])/dataset_sd[%s]', column.name2, j) # added sd correction
    tmp2 <- eval(parse(text = command2))
    
    tmp <- cbind(tmp, tmp2) # create entire matrix for one species
    
  }
  
  beta.samp_standardised <- cbind(beta.samp_standardised, tmp) # bind all species matrcies together 
  
}

beta.samp_standardised <- beta.samp_standardised[,-(1)] # remove NA column from beginning
colnames(beta.samp_standardised) <- col.names_standardised # set column names

#####################################################
### Extract Mean/SD/Quantiles and into Data Frame ###
#####################################################

# Not standardised

beta.mean <- colMeans(beta.samp)
beta.mean <- matrix(beta.mean, ncol = length(Env.names), byrow = T)
colnames(beta.mean) <- Env.names
rownames(beta.mean) <- species.names

beta.sd <- colSds(beta.samp)
beta.sd <- matrix(beta.sd, ncol = length(Env.names), byrow = T)
colnames(beta.sd) <- Env.names
rownames(beta.sd) <- species.names

beta.lower <- colQuantiles(beta.samp, probs = 0.025)
beta.lower <- matrix(beta.lower, ncol = length(Env.names), byrow = T)
colnames(beta.lower) <- Env.names
rownames(beta.lower) <- species.names

beta.upper <- colQuantiles(beta.samp, probs = 0.975)
beta.upper <- matrix(beta.upper, ncol = length(Env.names), byrow = T)
colnames(beta.upper) <- Env.names
rownames(beta.upper) <- species.names

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

beta.cv <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- coefVar(beta.samp[,i])
  beta.cv <- c(beta.cv, tmp)
}
beta.cv <- matrix(beta.cv, ncol = length(Env.names), byrow = TRUE)
colnames(beta.cv) <- Env.names
rownames(beta.cv) <- species.names

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- qcd(beta.samp[,i])
  beta.qcd <- c(beta.qcd, tmp)
}
beta.qcd <- matrix(beta.qcd, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd) <- Env.names
rownames(beta.qcd) <- species.names

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd2 <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- qcd2(beta.samp[,i])
  beta.qcd2 <- c(beta.qcd2, tmp)
}
beta.qcd2 <- matrix(beta.qcd2, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd2) <- Env.names
rownames(beta.qcd2) <- species.names

beta.gini <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- ineq(beta.samp[,i], type = "Gini")
  beta.gini <- c(beta.gini, tmp)
}
beta.gini <- matrix(beta.gini, ncol = length(Env.names), byrow = TRUE)
colnames(beta.gini) <- Env.names
rownames(beta.gini) <- species.names

# Standardised

beta.mean_standardised <- colMeans(beta.samp_standardised)
beta.mean_standardised <- matrix(beta.mean_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.mean_standardised) <- Env.names
rownames(beta.mean_standardised) <- species.names

beta.sd_standardised <- colSds(beta.samp_standardised)
beta.sd_standardised <- matrix(beta.sd_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.sd_standardised) <- Env.names
rownames(beta.sd_standardised) <- species.names

beta.lower_standardised <- colQuantiles(beta.samp_standardised, probs = 0.025)
beta.lower_standardised <- matrix(beta.lower_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.lower_standardised) <- Env.names
rownames(beta.lower_standardised) <- species.names

beta.upper_standardised <- colQuantiles(beta.samp_standardised, probs = 0.975)
beta.upper_standardised <- matrix(beta.upper_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.upper_standardised) <- Env.names
rownames(beta.upper_standardised) <- species.names

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

beta.cv_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- coefVar(beta.samp_standardised[,i])
  beta.cv_standardised <- c(beta.cv_standardised, tmp)
}
beta.cv_standardised <- matrix(beta.cv_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.cv_standardised) <- Env.names
rownames(beta.cv_standardised) <- species.names

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- qcd(beta.samp_standardised[,i])
  beta.qcd_standardised <- c(beta.qcd_standardised, tmp)
}
beta.qcd_standardised <- matrix(beta.qcd_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd_standardised) <- Env.names
rownames(beta.qcd_standardised) <- species.names

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd2_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- qcd2(beta.samp_standardised[,i])
  beta.qcd2_standardised <- c(beta.qcd2_standardised, tmp)
}
beta.qcd2_standardised <- matrix(beta.qcd2_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd2_standardised) <- Env.names
rownames(beta.qcd2_standardised) <- species.names

beta.gini_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- ineq(beta.samp_standardised[,i], type = "Gini")
  beta.gini_standardised <- c(beta.gini_standardised, tmp)
}
beta.gini_standardised <- matrix(beta.gini_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.gini_standardised) <- Env.names
rownames(beta.gini_standardised) <- species.names

##############################
### Create blank dataframe ### # Not standardised
##############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                 qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

############################
### Extract to dataframe ### # Not standardised
############################

for(i in 1:ncol(Y2)){
  dfr <- cbind(Env.names, beta.mean[i,], beta.lower[i,], beta.upper[i,], beta.sd[i,],
               beta.cv[i,], beta.qcd[i,], beta.qcd2[i,], beta.gini[i,],
               rep("boral", (length(Env.names))), rep(species.names[i], length(Env.names)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df <- rbind(df, dfr)
}

rownames(df) <- NULL

##############################
### Create blank dataframe ### # Standardised
##############################

df_standardised <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                              upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                              qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

############################
### Extract to dataframe ### # Standardised
############################

for(i in 1:ncol(Y2)){
  dfr <- cbind(Env.names, beta.mean_standardised[i,], beta.lower_standardised[i,],
               beta.upper_standardised[i,], beta.sd_standardised[i,],
               beta.cv_standardised[i,], beta.qcd_standardised[i,], beta.qcd2_standardised[i,],
               beta.gini_standardised[i,], rep("boral", (length(Env.names))),
               rep(species.names[i], length(Env.names)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df_standardised <- rbind(df_standardised, dfr)
}

rownames(df_standardised) <- NULL

df_merge <- df
df_merge[,5:9] <- df_standardised[,5:9]

#############################
### Rho Mean/Sd/Quantiles ###
#############################

#source("get_residual_corr_new_function.R") # implement modified version of get.residual.cor() to provide sd and quantile values

rho.mean <- get.residual.cor.new(vegfit.mod3, est = "mean")$cor
rho.sd <- get.residual.cor.new(vegfit.mod3, est = "sd")$cor
rho.lower <- get.residual.cor.new(vegfit.mod3, est = "q_lower")$cor
rho.upper <- get.residual.cor.new(vegfit.mod3, est = "q_upper")$cor

####################
#### Write CSVs ####
####################

write.csv(df_merge, "Vegetation/Beta/Beta_Veg_boral.csv")
write.csv(rho.mean, "Vegetation/Rho/Rho_mean_Veg_boral.csv")
write.csv(rho.lower, "Vegetation/Rho/Rho_lower_Veg_boral.csv")
write.csv(rho.upper, "Vegetation/Rho/Rho_upper_Veg_boral.csv")
write.csv(rho.sd, "Vegetation/Rho/Rho_sd_Veg_boral.csv")

#Beta

boral.df <- read.csv("Vegetation/Beta/Beta_Veg_boral.csv")
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

file.name <- paste0("Vegetation/Beta_all_plot", ".pdf", sep = "")
ggsave(file.name, plot=beta.plot, units = "in", width = 7, height = 7)
save(beta.plot, file="Vegetation/beta_plot.RData")

#Rho

boral_Rho_Mean <- read.csv("Vegetation/Rho/Rho_mean_Veg_boral.csv")
rownames(boral_Rho_Mean) <- boral_Rho_Mean[,1]
boral_Rho_Mean <- boral_Rho_Mean[,-1]
boral_Rho_Mean <- as.matrix(boral_Rho_Mean)
colnames(boral_Rho_Mean) <- rownames(boral_Rho_Mean) <- 1:ncol(boral_Rho_Mean)

boral_Rho_Lower <- read.csv("Vegetation/Rho/Rho_lower_Veg_boral.csv")
rownames(boral_Rho_Lower) <- boral_Rho_Lower[,1]
boral_Rho_Lower <- boral_Rho_Lower[,-1]
boral_Rho_Lower <- as.matrix(boral_Rho_Lower)
colnames(boral_Rho_Lower) <- rownames(boral_Rho_Lower) <- 1:ncol(boral_Rho_Lower)

boral_Rho_upper <- read.csv("Vegetation/Rho/Rho_upper_Veg_boral.csv")
rownames(boral_Rho_upper) <- boral_Rho_upper[,1]
boral_Rho_upper <- boral_Rho_upper[,-1]
boral_Rho_upper <- as.matrix(boral_Rho_upper)
colnames(boral_Rho_upper) <- rownames(boral_Rho_upper) <- 1:ncol(boral_Rho_upper)

Uncertainty_boral <- boral_Rho_upper - boral_Rho_Lower
colnames(Uncertainty_boral) <- rownames(Uncertainty_boral) <- 1:ncol(Uncertainty_boral)
Uncertainty_boral <- as.matrix(Uncertainty_boral)

pdf(file = "Vegetation/All_veg_Correlation Plot Vegetation.pdf", 
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

save(wetland.height.plot, uncon.mod, complete.invert.plot, con.mod, file = "Complete_veg_analysis.RData")
setwd("/home/barefootbushman/Desktop/DWER Thresholds analysis/DWER_Thresholds")
