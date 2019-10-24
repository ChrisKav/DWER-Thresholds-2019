library(dplyr)
library(lubridate)

load("Refined_data.RData")
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
lst[[7]]$plot[lst[[7]]$plot=="B"] <- 39.554
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

b.articulata <- comm.df[,c(1,2, 45:47, 34)]
b.articulata <- b.articulata[apply(b.articulata, 1, function(row) all(row !=0 )),]
summary(b.articulata)

b.attenuata <- comm.df[,c(1,2, 45:47, 142)]
b.attenuata <- b.attenuata[apply(b.attenuata, 1, function(row) all(row !=0 )),]
summary(b.attenuata)

m.rhaphiophyla <- comm.df[,c(1,2, 45:47, 40)]
m.rhaphiophyla <- m.rhaphiophyla[apply(m.rhaphiophyla, 1, function(row) all(row !=0 )),]
summary(m.rhaphiophyla)

e.rudis <- comm.df[,c(1,2, 45:47, 37)]
e.rudis <- e.rudis[apply(e.rudis, 1, function(row) all(row !=0 )),]
summary(e.rudis)
