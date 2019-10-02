library(lubridate)
library(mgcv)

rain.raw <- read.csv("IDCJAC0009_009021_1800_Data.csv", header = TRUE)
rain <- rain.raw[,3:6]
rain <- na.omit(rain)
rain <- subset(rain, Year >= 1950)
colnames(rain)[c(2,4)] <- c("nMonth", "Rainfall")
rain <- rain %>%
  group_by(Year, nMonth) %>%
  summarise(Rainfall=(sum(Rainfall)))
rain <- data.frame(rain)
rain$Date <- paste("15", rain$nMonth, rain$Year, sep="-") %>% dmy() %>% as.Date()
rain <- transform(rain, Time = as.numeric(Date)/100)
rain$Month <- month(rain$Date, label=TRUE, abbr = TRUE)

plot(Rainfall ~ Year, data = rain, type = "p", 
     ylab = ylab, main = "Perth Airport monthly rainfall")

m <- gamm(Rainfall ~ s(nMonth, bs = "cc", k = 12) + s(Time), data = rain)
summary(m$gam)

layout(matrix(1:2, ncol = 2))
plot(m$gam, scale = 0)

acf(resid(m$lme), lag.max = 36, main = "ACF")
pacf(resid(m$lme), lag.max = 36, main = "pACF")

ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

## AR(1)
m1 <- gamm(Rainfall ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 100),
                         data = rain, correlation = corARMA(form = ~ 1|Year, p = 1),
                          control = ctrl)

## AR(2)
m2 <- gamm(Rainfall ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 100),
                         data = rain, correlation = corARMA(form = ~ 1|Year, p = 2),
                         control = ctrl)

## AR(3)
m3 <- gamm(Rainfall ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 100),
                          data = rain, correlation = corARMA(form = ~ 1|Year, p = 3),
                          control = ctrl)
anova(m$lme, m1$lme, m2$lme, m3$lme)

plot(m1$gam, scale = 0)
res <- resid(m1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")



want <- seq(1, nrow(rain), length.out = 200)
pdat <- with(rain, data.frame(Time = Time[want], Date = Date[want], nMonth = nMonth[want]))

## predict trend contributions
p  <- predict(m$gam,  newdata = pdat, type = "terms", se.fit = TRUE)
p1 <- predict(m1$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p2 <- predict(m2$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p3 <- predict(m3$gam, newdata = pdat, type = "terms", se.fit = TRUE)

## combine with the predictions data, including fitted and SEs
pdat <- transform(pdat,
                      p  = p$fit[,2],  se  = p$se.fit[,2],
                      p1 = p1$fit[,2], se1 = p1$se.fit[,2],
                      p2 = p2$fit[,2], se2 = p2$se.fit[,2],
                      p3 = p3$fit[,2], se3 = p3$se.fit[,2])

op <- par(mar = c(5,4,2,2) + 0.1)
ylim <- with(pdat, range(p, p1, p2, p3))
ylim[1] <- floor(ylim[1])
ylim[2] <- ceiling(ylim[2])
ylab <- expression(Temperature ~ (degree*C ~ centred))
plot(Rainfall - mean(Rainfall) ~ Date, data = rain, type = "n",
     ylab = ylab, ylim = ylim)
lines(p  ~ Date, data = pdat, col = "black")
lines(p1 ~ Date, data = pdat, col = "red")
lines(p2 ~ Date, data = pdat, col = "blue")
lines(p3 ~ Date, data = pdat, col = "forestgreen", lwd = 1)
legend("topleft",
       legend = c("Uncorrelated Errors", paste0("AR(", 1:3, ") Errors")),
       bty = "n", col = c("black","red","blue","forestgreen"),
       lty = 1, lwd = c(1,1,1))
par(op)

rainAnn <- rain %>%
  group_by(Year) %>%
  summarise(Rainfall=(sum(Rainfall)))
plot(Rainfall~Year, rainAnn, type="l")
