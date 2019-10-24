library(dplyr)
library(lubridate)

load("Refined_data.RData")
lst <- comm


lst <- list(lst[['Goolelal']] , lst[['Loch_McNess']], lst[['Yonderup']], lst[['Joondalup_Nth']], 
       lst[['Joondalup_Sth']], lst[['Mariginiup']], lst[['Jandabup']])
names(lst) <- c('Goolelal', 'Loch_McNess', 'Yonderup', 'Joondalup_Nth', 'Joondalup_Sth', 'Mariginiup', 'Jandabup')

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
  colnames(x) <- c("Year", "mAHD")
  return(x)
})

AHD.levels <- Map(function(x,y){
  df <-  subset(x, Year %in% y$year)
  return(df)
}, AHD.levels, lst)



# 5 subtract plot elevation from water level
# 6 use dplyr::bind_rows to merge dataset into a single dataframe
comm.df <- bind_rows(lst, .id="wetland")
comm.df[is.na(comm.df)] <- 0

ahd.df <- bind_rows(AHD.levels, .id="wetland")
