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

options(stringsAsFactors=F)

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",
              tmpf, method = "wget")
source(tmpf)

#1. Import data with wetlands as different lists
data <- read_excel_allsheets("comm_data with A1 plots.xlsx")

# 2. Basic data transformations to ensure data is in correct format
comm <- lapply(data, function(x) {
  x <- data.frame(x)
  colnames(x)[1] <- "Species"
  x <- x[rowSums(as.data.frame(x[,2:ncol(x)]) < 1) < (ncol(x)-2),]
  row.names(x) <- as.character(x$Species)
  x$Species <- NULL
  x <- x[,colSums(x==0)<nrow(x) ]
  colnames(x) <- substr(colnames(x), start=2, stop=5)
  x <- data.frame(t(x))
  x$year <- substr(row.names(x), start=1, stop=2)
  x$year2 <- as.numeric(x$year)
  x$year2 <- replace(x$year2, x$year2<90, 20)
  x$year2 <- replace(x$year2, x$year2>90, 19)
  x$year <- do.call(paste, c(x[c('year2', 'year')], sep=""))
  x$year2 <- NULL
  x$plot <- substr(row.names(x), start=4, stop=4)
  x
  return(x)
})

# Model Water Level data

raw.data <- read_excel_allsheets("WaterLevelsDiscreteForSiteFlatFile.xlsx")
  
raw.data <- data.frame(raw.data, stringsAsFactors = FALSE)
data <- data.frame(raw.data[,c(1,3,4,5,6,25,28)])
data[,1] <- factor(data[,1])
data <- data[data[,6] %in% 	c("Water level (AHD) | m", "Storage level (AHD) | m"), ]
data[,6] <- factor(data[,6])
data <- data[!data[,1] %in% 	"616145", ] # Too few samples
data <- data[!data[,1] %in% 	"6162623", ] # Not converging in GAMM model
data <- data[!data[,1] %in% 	"6162629", ] # Most readings below staff gaugue or dry
data[,3] <- excel_numeric_to_date(data[,3])
data[,3] <- as.Date(data[,3], "%d %b %Y")
data[,7] <- as.numeric(data[,7])
data <- data[complete.cases(data),]
data$Month <- format(data[,3],"%B")
data <- cbind(data[,1], data[,3, drop = FALSE], data[,2], data[,5], data[,7], data[,8],data[,4])
colnames(data) <- c("Site", "Date", "Time", "Year", "AHD", "Month", "Month_num")
data$AHD <- as.numeric(as.character(data$AHD))
data$Year <- as.integer(data$Year)
data$Month <- as.character(data$Month)
data$Month_num <- as.integer(data$Month_num)
data$Site <- as.factor(as.character(data$Site))
data.ls <- split(data, data$Site)

AHD <- lapply(data.ls, function(x) {
  x <- x %>%
    group_by(Year, Month) %>%
    summarise(a_mean=(mean(AHD)))
  x <- data.frame(x)
  x <- transform(x, Date = as.Date(paste(Year, Month, "15", sep="-"), format="%Y-%b-%d"))
  x$Month_num <- as.integer(month(x$Date))
  x <- transform(x, Time=as.numeric(Date)/100)
  colnames(x)[3] <- "AHD"
  x <- x[order(as.Date(x$Date, format="%Y-%b-%d")),]
  x <- x[x$Year %in% names(which(table(x$Year) > 2)), ]
  return(x)
}) # Makes Time dummy variable

#Model with correlated errors
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
AHD.mod <- lapply(AHD, function(x){
  m1 <- gamm(AHD ~ s(Month_num, bs = "cc", k = 12) + 
               s(Time, k = 10),
             data = x, correlation = corARMA(form = ~ 1|Year, p = 1),
             control = ctrl)
  return(m1)
})

#Which periods are changing
AHD.params <- Map(function(x,y) {
  want <- seq(1, nrow(x), length.out = 200)
  pdat <- with(x,
               data.frame(Time = Time[want], Date = Date[want],
                          Month_num = Month_num[want]))
  p2 <- predict(y$gam, newdata = pdat, type = "terms", se.fit = TRUE)
  pdat <- transform(pdat, p2 = p2$fit[,2], se2 = p2$se.fit[,2])
  df.res <- df.residual(y$gam)
  crit.t <- qt(0.025, df.res, lower.tail = FALSE)
  pdat <- transform(pdat,
                    upper = p2 + (crit.t * se2),
                    lower = p2 - (crit.t * se2))
  Term <- "Time"
  d <- Deriv(y)
  dci <- confint(d, term = Term)
  dsig <- signifD(pdat$p2, d = d[[Term]]$deriv,
                  dci[[Term]]$upper, dci[[Term]]$lower)
  x <- cbind(pdat, dsig)
  #names(x) <- c("pdat", "dsig")
  return(x)
}, AHD, AHD.mod)

wet.names <- names(data.ls)

AHD.params <- Map(function(x,y){
  x$mean <- mean(x$AHD)
  x$cent <- x$AHD
  y$p3 <- y$p2 + x$mean[1]
  y$upper2 <- y$upper + x$mean[1]
  y$lower2 <- y$lower + x$mean[1]
  y$incr2 <- y$incr + x$mean[1]
  y$decr2 <- y$decr + x$mean[1]
  return(y)
}, AHD, AHD.params)

AHD.plots <- Map(function(x,y,i){
  x$cent <- x$AHD
  p <- ggplot(x, aes(x=Date, y=cent)) +
    geom_point(x, mapping=aes(x=Date, y=cent), color="lightgrey") +
    geom_ribbon(y, mapping=aes(ymin=lower2, ymax=upper2, x=Date), alpha=0.2,
                inherit.aes=FALSE, fill="black") +
    geom_line(y, mapping=aes(x=Date, y=p3)) +
    geom_line(y, mapping=aes(x=Date, y=incr2), color="blue") +
    geom_line(y, mapping=aes(x=Date, y=decr2), color = "red") +
    labs(x = "Year", y = expression(AHD ~ (m)))
  p <- p + ggtitle(paste(i))
  return(p)
}, AHD, AHD.params,wet.names)

#Goollelal

Goollelal.AHD <- list(AHD$'61611870', AHD$'6162517')
Goollelal.AHD[[1]]$group <- "ground"
Goollelal.AHD[[2]]$group <- "surface"
Goollelal.AHD <- rbind(Goollelal.AHD[[1]], Goollelal.AHD[[2]])

Goollelal.params <- list(AHD.params$'61611870', AHD.params$'6162517')
Goollelal.params[[1]]$group <- "ground"
Goollelal.params[[2]]$group <- "surface"
Goollelal.params <- rbind(Goollelal.params[[1]], Goollelal.params[[2]])

gool.p <- ggplot(Goollelal.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Goollelal.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Goollelal.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                            group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Goollelal.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Goollelal.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Goollelal.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
gool.p <- gool.p + ggtitle("Goollelal")
gool.p

#Loch McNess

McNess.AHD <- list(AHD$'61612104', AHD$'6162564', AHD$'61640108')
McNess.AHD[[1]]$group <- "ground"
McNess.AHD[[2]]$group <- "surface"
McNess.AHD[[3]]$group <- "ground2"
McNess.AHD <- rbind(McNess.AHD[[1]], McNess.AHD[[2]], McNess.AHD[[3]])

McNess.params <- list(AHD.params$'61612104', AHD.params$'6162564', AHD.params$'61640108')
McNess.params[[1]]$group <- "ground"
McNess.params[[2]]$group <- "surface"
McNess.params[[3]]$group <- "ground2"
McNess.params <- rbind(McNess.params[[1]], McNess.params[[2]], McNess.params[[3]])

mcness.p <- ggplot(McNess.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(McNess.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(McNess.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                         group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(McNess.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(McNess.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(McNess.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
mcness.p <- mcness.p + ggtitle("Loch McNess")
mcness.p

#Yonderup

Yonderup.AHD <- list(AHD$'61611840', AHD$'6162565')
Yonderup.AHD[[1]]$group <- "ground"
Yonderup.AHD[[2]]$group <- "surface"
Yonderup.AHD <- rbind(Yonderup.AHD[[1]], Yonderup.AHD[[2]])

Yonderup.params <- list(AHD.params$'61611840', AHD.params$'6162565')
Yonderup.params[[1]]$group <- "ground"
Yonderup.params[[2]]$group <- "surface"
Yonderup.params <- rbind(Yonderup.params[[1]], Yonderup.params[[2]])

yond.p <- ggplot(Yonderup.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Yonderup.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Yonderup.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                           group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Yonderup.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Yonderup.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Yonderup.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
yond.p <- yond.p + ggtitle("Yonderup")
yond.p

#Joondalup

Joondalup.AHD <- list(AHD$'61610661', AHD$'6162572', AHD$'61611423')
Joondalup.AHD[[1]]$group <- "ground"
Joondalup.AHD[[2]]$group <- "surface"
Joondalup.AHD[[3]]$group <- "ground2"
Joondalup.AHD <- rbind(Joondalup.AHD[[1]], Joondalup.AHD[[2]], Joondalup.AHD[[3]])

Joondalup.params <- list(AHD.params$'61610661', AHD.params$'6162572', AHD.params$'61611423')
Joondalup.params[[1]]$group <- "ground"
Joondalup.params[[2]]$group <- "surface"
Joondalup.params[[3]]$group <- "ground2"
Joondalup.params <- rbind(Joondalup.params[[1]], Joondalup.params[[2]], Joondalup.params[[3]])

joon.p <- ggplot(Joondalup.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Joondalup.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Joondalup.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                            group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Joondalup.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Joondalup.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Joondalup.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
joon.p <- joon.p + ggtitle("Joondalup")
joon.p

#Mariginup

Mariginup.AHD <- list(AHD$'61610685', AHD$'6162577')
Mariginup.AHD[[1]]$group <- "ground"
Mariginup.AHD[[2]]$group <- "surface"
Mariginup.AHD <- rbind(Mariginup.AHD[[1]], Mariginup.AHD[[2]])

Mariginup.params <- list(AHD.params$'61610685', AHD.params$'6162577')
Mariginup.params[[1]]$group <- "ground"
Mariginup.params[[2]]$group <- "surface"
Mariginup.params <- rbind(Mariginup.params[[1]], Mariginup.params[[2]])

marig.p <- ggplot(Mariginup.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Mariginup.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Mariginup.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                            group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Mariginup.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Mariginup.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Mariginup.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
marig.p <- marig.p + ggtitle("Mariginup")
marig.p

#Jandabup

Jandabup.AHD <- list(AHD$'61611850', AHD$'6162578')
Jandabup.AHD[[1]]$group <- "ground"
Jandabup.AHD[[2]]$group <- "surface"
Jandabup.AHD <- rbind(Jandabup.AHD[[1]], Jandabup.AHD[[2]])

Jandabup.params <- list(AHD.params$'61611850', AHD.params$'6162578')
Jandabup.params[[1]]$group <- "ground"
Jandabup.params[[2]]$group <- "surface"
Jandabup.params <- rbind(Jandabup.params[[1]], Jandabup.params[[2]])

jand.p <- ggplot(Jandabup.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Jandabup.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Jandabup.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                           group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Jandabup.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Jandabup.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Jandabup.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
jand.p <- jand.p + ggtitle("Jandabup")
jand.p

#Nowergup

Nowergup.AHD <- list(AHD$'61610601', AHD$'6162567', AHD$'61611247')
Nowergup.AHD[[1]]$group <- "ground"
Nowergup.AHD[[2]]$group <- "surface"
Nowergup.AHD[[3]]$group <- "ground2"
Nowergup.AHD <- rbind(Nowergup.AHD[[1]], Nowergup.AHD[[2]], Nowergup.AHD[[3]])

Nowergup.params <- list(AHD.params$'61610601', AHD.params$'6162567', AHD.params$'61611247')
Nowergup.params[[1]]$group <- "ground"
Nowergup.params[[2]]$group <- "surface"
Nowergup.params[[3]]$group <- "ground2"
Nowergup.params <- rbind(Nowergup.params[[1]], Nowergup.params[[2]], Nowergup.params[[3]])

nower.p <- ggplot(Nowergup.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Nowergup.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Nowergup.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                           group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Nowergup.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Nowergup.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Nowergup.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
nower.p <- nower.p + ggtitle("Nowergup")
nower.p

#Wilgarup

Wilgarup.AHD <- list(AHD$'61618500')
Wilgarup.AHD[[1]]$group <- "ground"
Wilgarup.AHD <- rbind(Wilgarup.AHD[[1]])

Wilgarup.params <- list(AHD.params$'61618500')
Wilgarup.params[[1]]$group <- "ground"
Wilgarup.params <- rbind(Wilgarup.params[[1]])

wilg.p <- ggplot(Wilgarup.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Wilgarup.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Wilgarup.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                           group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Wilgarup.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Wilgarup.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Wilgarup.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
wilg.p <- wilg.p + ggtitle("Wilgarup")
wilg.p

#Pipidinny
Pipidinny.AHD <- list(AHD$'61611872', AHD$'6162624')
Pipidinny.AHD[[1]]$group <- "ground"
Pipidinny.AHD[[2]]$group <- "surface"
Pipidinny.AHD <- rbind(Pipidinny.AHD[[1]], Pipidinny.AHD[[2]])

Pipidinny.params <- list(AHD.params$'61611872', AHD.params$'6162624')
Pipidinny.params[[1]]$group <- "ground"
Pipidinny.params[[2]]$group <- "surface"
Pipidinny.params <- rbind(Pipidinny.params[[1]], Pipidinny.params[[2]])

pipi.p <- ggplot(Pipidinny.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Pipidinny.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Pipidinny.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                            group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Pipidinny.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Pipidinny.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Pipidinny.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
pipi.p <- pipi.p + ggtitle("Pipidinny")
pipi.p

#Lexia186

Lexia186.AHD <- list(AHD$'61613214')
Lexia186.AHD[[1]]$group <- "ground"
Lexia186.AHD <- rbind(Lexia186.AHD[[1]])

Lexia186.params <- list(AHD.params$'61613214')
Lexia186.params[[1]]$group <- "ground"
Lexia186.params <- rbind(Lexia186.params[[1]])

lex186.p <- ggplot(Lexia186.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Lexia186.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Lexia186.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                           group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Lexia186.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Lexia186.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Lexia186.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
lex186.p <- lex186.p + ggtitle("Lexia186")
lex186.p

#EMP173

EMP173.AHD <- list(AHD$'61613213', AHD$'6162628')
EMP173.AHD[[1]]$group <- "ground"
EMP173.AHD[[2]]$group <- "surface"
EMP173.AHD <- rbind(EMP173.AHD[[1]], EMP173.AHD[[2]])

EMP173.params <- list(AHD.params$'61613213', AHD.params$'6162628')
EMP173.params[[1]]$group <- "ground"
EMP173.params[[2]]$group <- "surface"
EMP173.params <- rbind(EMP173.params[[1]], EMP173.params[[2]])

emp173.p <- ggplot(EMP173.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(EMP173.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(EMP173.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                         group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(EMP173.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(EMP173.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(EMP173.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
emp173.p <- emp173.p + ggtitle("EMP173")
emp173.p

#EMP78

EMP78.AHD <- list(AHD$'61613231')
EMP78.AHD[[1]]$group <- "ground"
EMP78.AHD <- rbind(EMP78.AHD[[1]])

EMP78.params <- list(AHD.params$'61613231')
EMP78.params[[1]]$group <- "ground"
EMP78.params <- rbind(EMP78.params[[1]])

emp78.p <- ggplot(EMP78.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(EMP78.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(EMP78.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                        group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(EMP78.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(EMP78.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(EMP78.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
emp78.p <- emp78.p + ggtitle("EMP78")
emp78.p

#MM59B

MM59B.AHD <- list(AHD$'61610661')
MM59B.AHD[[1]]$group <- "ground"
MM59B.AHD <- rbind(MM59B.AHD[[1]])

MM59B.params <- list(AHD.params$'61610661')
MM59B.params[[1]]$group <- "ground"
MM59B.params <- rbind(MM59B.params[[1]])

MM59B.p <- ggplot(MM59B.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(MM59B.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(MM59B.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                        group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(MM59B.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(MM59B.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(MM59B.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
MM59B.p <- MM59B.p + ggtitle("MM59B")
MM59B.p

#PM9

PM9.AHD <- list(AHD$'61610804')
PM9.AHD[[1]]$group <- "ground"
PM9.AHD <- rbind(PM9.AHD[[1]])

PM9.params <- list(AHD.params$'61610804')
PM9.params[[1]]$group <- "ground"
PM9.params <- rbind(PM9.params[[1]])

PM9.p <- ggplot(PM9.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(PM9.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(PM9.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                      group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(PM9.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(PM9.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(PM9.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
PM9.p <- PM9.p + ggtitle("PM9")
PM9.p

#WM1

WM1.AHD <- list(AHD$'61610833')
WM1.AHD[[1]]$group <- "ground"
WM1.AHD <- rbind(WM1.AHD[[1]])

WM1.params <- list(AHD.params$'61610833')
WM1.params[[1]]$group <- "ground"
WM1.params <- rbind(WM1.params[[1]])

WM1.p <- ggplot(WM1.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(WM1.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(WM1.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                      group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(WM1.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(WM1.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(WM1.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
WM1.p <- WM1.p + ggtitle("WM1")
WM1.p

#WM2

WM2.AHD <- list(AHD$'61610908')
WM2.AHD[[1]]$group <- "ground"
WM2.AHD <- rbind(WM2.AHD[[1]])

WM2.params <- list(AHD.params$'61610908')
WM2.params[[1]]$group <- "ground"
WM2.params <- rbind(WM2.params[[1]])

WM2.p <- ggplot(WM2.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(WM2.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(WM2.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                      group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(WM2.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(WM2.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(WM2.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
WM2.p <- WM2.p + ggtitle("WM2")
WM2.p

#WM8

WM8.AHD <- list(AHD$'61610983')
WM8.AHD[[1]]$group <- "ground"
WM8.AHD <- rbind(WM8.AHD[[1]])

WM8.params <- list(AHD.params$'61610983')
WM8.params[[1]]$group <- "ground"
WM8.params <- rbind(WM8.params[[1]])

WM8.p <- ggplot(WM8.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(WM8.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(WM8.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                      group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(WM8.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(WM8.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(WM8.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
WM8.p <- WM8.p + ggtitle("WM8")
WM8.p

#Gwelup

Gwelup.AHD <- list(AHD$'61610032', AHD$'6162504')
Gwelup.AHD[[1]]$group <- "ground"
Gwelup.AHD[[2]]$group <- "surface"
Gwelup.AHD <- rbind(Gwelup.AHD[[1]], Gwelup.AHD[[2]])

Gwelup.params <- list(AHD.params$'61610032', AHD.params$'6162504')
Gwelup.params[[1]]$group <- "ground"
Gwelup.params[[2]]$group <- "surface"
Gwelup.params <- rbind(Gwelup.params[[1]], Gwelup.params[[2]])

gwel.p <- ggplot(Gwelup.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Gwelup.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Gwelup.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                         group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Gwelup.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Gwelup.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Gwelup.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
gwel.p <- gwel.p + ggtitle("Gwelup")
gwel.p

#Quin Brook

QuinBrook.AHD <- list(AHD$'61710060')
QuinBrook.AHD[[1]]$group <- "ground"
QuinBrook.AHD <- rbind(QuinBrook.AHD[[1]])

QuinBrook.params <- list(AHD.params$'61710060')
QuinBrook.params[[1]]$group <- "ground"
QuinBrook.params <- rbind(QuinBrook.params[[1]])

quin.p <- ggplot(QuinBrook.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(QuinBrook.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(QuinBrook.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                            group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(QuinBrook.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(QuinBrook.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(QuinBrook.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
quin.p <- quin.p + ggtitle("QuinBrook")
quin.p

#Gingin

Gingin.AHD <- list(AHD$'61710078')
Gingin.AHD[[1]]$group <- "ground"
Gingin.AHD <- rbind(Gingin.AHD[[1]])

Gingin.params <- list(AHD.params$'61710078')
Gingin.params[[1]]$group <- "ground"
Gingin.params <- rbind(Gingin.params[[1]])

gin.p <- ggplot(Gingin.AHD, aes(x=Date, y=AHD, group=group)) +
  theme_bw() +
  geom_line(aes(colour=group)) +
  geom_point(Gingin.AHD, mapping=aes(x=Date, y=AHD, colour=group)) +
  geom_ribbon(Gingin.params, mapping=aes(ymin=lower2, ymax=upper2, x=Date, 
                                         group=group), alpha=0.2,
              inherit.aes=FALSE, fill="black") +
  geom_line(Gingin.params, mapping=aes(x=Date, y=p3, group=group)) +
  geom_line(Gingin.params, mapping=aes(x=Date, y=incr2, group=group), color="blue") +
  geom_line(Gingin.params, mapping=aes(x=Date, y=decr2, group=group), color = "red") +
  labs(x = "Year", y = expression(AHD ~ (m)))
gin.p <- gin.p + ggtitle("Gingin")
gin.p

# Save data

AHD.data.list <- list(Goollelal.AHD=Goollelal.AHD, McNess.AHD=McNess.AHD, Yonderup.AHD=Yonderup.AHD, 
                      Joondalup.AHD=Joondalup.AHD, Mariginup.AHD=Mariginup.AHD, Jandabup.AHD=Jandabup.AHD,
                      Nowergup.AHD=Nowergup.AHD, Wilgarup.AHD=Wilgarup.AHD, Pipidinny.AHD=Pipidinny.AHD, 
                      Lexia186.AHD=Lexia186.AHD, EMP173.AHD=EMP173.AHD, EMP78.AHD=EMP78.AHD, MM59B.AHD=MM59B.AHD, 
                      PM9.AHD=PM9.AHD, WM1.AHD=WM1.AHD, WM2.AHD=WM2.AHD, WM8.AHD=WM8.AHD, Gwelup.AHD=Gwelup.AHD,
                      QuinBrook.AHD=QuinBrook.AHD, Gingin.AHD=Gingin.AHD)
Params.data.list <- list(Goollelal.params=Goollelal.params,McNess.params=McNess.params, Yonderup.params=Yonderup.params,
                         Joondalup.params=Joondalup.params, Mariginup.params=Mariginup.params, Jandabup.params=Jandabup.params, 
                         Nowergup.params=Nowergup.params, Wilgarup.params=Wilgarup.params, Pipidinny.params=Pipidinny.params,
                         Lexia186.params=Lexia186.params, EMP173.params=EMP173.params, EMP78.params=EMP78.params, 
                         MM59B.params=MM59B.params, PM9.params=PM9.params, WM1.params=WM1.params, WM2.params=WM2.params, 
                         WM8.params=WM8.params, Gwelup.params=Gwelup.params, QuinBrook.params=QuinBrook.params, 
                         Gingin.params=Gingin.params)
Site.GW.plots <- list(gool.p=gool.p, mcness.p=mcness.p, yond.p=yond.p, joon.p=joon.p, marig.p=marig.p, jand.p=jand.p, 
                      nower.p=nower.p, wilg.p=wilg.p, pipi.p=pipi.p, lex186.p=lex186.p, emp173.p=emp173.p, 
                      emp78.p=emp78.p, MM59B.p=MM59B.p, PM9.p=PM9.p, WM1.p=WM1.p, WM2.p=WM2.p, WM8.p=WM8.p, 
                      gwel.p=gwel.p, quin.p=quin.p, gin.p=gin.p)

pdf("Site GAM models.pdf")
Site.GW.plots
dev.off()

save(AHD.data.list, Params.data.list, Site.GW.plots, comm, data.ls, file = "Refined_data.RData")
  
