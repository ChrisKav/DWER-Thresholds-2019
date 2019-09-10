subsetDate <- function(site,x,y){site[site$Date >= x & site$Date <= y,]}

comm_prep <- function(Y) {
  colnames(Y) <- str_replace_all(colnames(Y), "[.]", "_")
  Y <- Y[with(Y, order(year, plot)),]
  Y <- Y[, colSums(Y !=0) > 1]
  studyDesign <- data.frame(cbind(Y$year, Y$plot, rownames(Y)))
  Y$year <- NULL
  Y$plot <- NULL
  colnames(studyDesign) <- c("Year", "Plot", "Sample")
  studyDesign$Year <- as.numeric(as.character(studyDesign$Year))
  Y <- as.matrix(Y)
  nS <- ncol(Y)
  rownames(Y) <- NULL
  return(list(Y, studyDesign))
}

determine_traits <- function(Y) {
  Traits <- data.frame(colnames(Y))
  colnames(Traits) <- "Species"
  nat <- Traits %>%
    filter(!str_detect(Species, "X_") == TRUE)
  exo <- Traits %>%
    filter(str_detect(Species, "X_") == TRUE)
  nat$Status <- "Native"
  exo$Status <- "Exotic"
  Traits <- rbind(exo, nat)
  rownames(Traits) <- Traits$Species
  Traits$Species <- NULL
  Traits$Status <- as.factor(Traits$Status)
  return(Traits)
  }

boral.extract <- function(b, d) {
  alpha <- 0.5 
  testcov <- b$lv.median %*% t(b$lv.coefs.median[, 2:3])
  do.svd <- svd(testcov, b$num.lv, b$num.lv)
  choose.lvs <- scale(do.svd$u *matrix(do.svd$d[1:b$num.lv]^alpha,
                                       nrow = b$n, ncol = 2, byrow = T),
                      center = T, scale = F)
  choose.lv.coefs <- scale(do.svd$v * 
                             matrix(do.svd$d[1:b$num.lv]^(1 -alpha),
                                    nrow = b$p, ncol = 2, byrow = T), center = T,
                           scale = F)
  choose.lv <- data.frame(choose.lvs, d$Year, d$Plot)
  colnames(choose.lv)[1:4] <- c("LV1", "LV2", "Year", "Plot")
  rownames(choose.lv.coefs) <- colnames(b$y)
  res <- list(choose.lv, choose.lv.coefs)
  names(res) <- c("lv", "coefs")
  return <- res
}

bor.summary.plot <- function(b, Y, d) {
  BigEffs <- b$ssvs.indcoefs.mean > (0.8)
  GWPostPs <- b$ssvs.indcoefs.mean[BigEffs]
  GWBFs <- GWPostPs/(1-GWPostPs)
  GWSizes <- b$X.coefs.median[BigEffs]
  UseSp <- names(GWPostPs)
  GetProp <- function(dat, lst) tapply(dat, lst, function(v) mean(v>0))
  Props <- t(apply(Y[,UseSp], 2, GetProp, lst=list(d[[1]]$Plot)))
  rownames(Props) <- gsub("\n"," ",rownames(Props))
  Props <- sweep(Props, 1, apply(Props,1,sum), "/")
  return(Props)
}

bor.summary.year <- function(b, Y, d) {
  BigEffs <- b$ssvs.indcoefs.mean > (0.8)
  GWPostPs <- b$ssvs.indcoefs.mean[BigEffs]
  GWBFs <- GWPostPs/(1-GWPostPs)
  GWSizes <- b$X.coefs.median[BigEffs]
  UseSp <- names(GWPostPs)
  GetProp <- function(dat, lst) tapply(dat, lst, function(v) mean(v>0))
  Props <- t(apply(Y[,UseSp], 2, GetProp, lst=list(d[[1]]$Year)))
  rownames(Props) <- gsub("\n"," ",rownames(Props))
  Props <- sweep(Props, 1, apply(Props,1,sum), "/")
  return(Props)
}

boral.plots <- function(b) {
  #sp.coef <- data.frame(subset(b$coefs, rownames(b$coefs) %in% rownames(s)))
  yr.min <- min(as.numeric(levels(b$lv$Year)))
  yr.max <- max(as.numeric(levels(b$lv$Year)))
  sp.coef <- data.frame(b$coefs)
  colnames(sp.coef) <- c("LV1", "LV2")
  sp.coef$Species <- rownames(sp.coef)
  ord <- ggplot(b$lv, aes(x=LV1, y=LV2, color=Plot, group=Plot))+
    theme_bw() +
    geom_point(data=subset(b$lv, Year == yr.min | Year == yr.max), size =3) +
    geom_path(aes(group=Plot), linetype="solid")+
    geom_text_repel(data=subset(b$lv, Year == yr.min | Year == yr.max)
                    , aes(label=Year))# +
  #geom_point(data=sp.coef, aes(x=LV1, y=LV2), inherit.aes = FALSE)# +
  #geom_label_repel(data=sp.coef, aes(x=LV1, y=LV2, label=sp.coef$Species), 
  #box.padding = 0.35,
  #point.padding=0.5,
  #segment.color="grey50",
  #inherit.aes = FALSE)
  return(ord)
}

water_level_5yr_summary <- function(x) {
  max <- mean(tapply(x$AHD, x$index, max))
  min <- mean(tapply(x$AHD, x$index, min))
  range <- mean(unlist(lapply( with(x, tapply(AHD, index, range)), diff)))
  max.mth <- x[which.max(apply(data.frame(x$AHD),MARGIN=1,max)),7]
  min.mth <- x[which.min(apply(data.frame(x$AHD),MARGIN=1,min)),7]
  to.dry <- NULL
  for(i in levels(x$index)){
    tmp <- subset(x, index==i)
    tmp <- tmp[which.min(tmp$AHD),3] - tmp[which.max(tmp$AHD),3]
    if(is.null(to.dry)){to.dry<-tmp} else { to.dry<-rbind(to.dry,tmp)}
  }
  to.dry <- mean(as.numeric(to.dry$Date, units="days"))
  hydro.sum <- cbind(max, min, range, max.mth, min.mth, to.dry)
  return(hydro.sum)
}


