setwd("~/research/coffee/tools")

library(ncdf4)
library(raster)
source("suitability/intake/lib.R")

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")
robusta <- ncvar_get(database, "robusta")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

## Turn into probabilities
areas <- matrix(NA, nrow=4320, ncol=720)
for (ii in 1:720) {
    # 1/12 degrees
    londist <- 2*pi * 6371 * cos(latitude[ii] * pi / 180) / (360 * 12)
    latdist <- 2*pi * 6371 / (360 * 12)
    areas[, ii] <- londist * latdist * 100 # in Ha
}

predicted <- nc_open("suitability/outputs/arabica.nc4")
arabica.predicted <- ncvar_get(predicted, "suitability")

predicted <- nc_open("suitability/outputs/robusta.nc4")
robusta.predicted <- ncvar_get(predicted, "suitability")

quantile(arabica / areas, na.rm=T)
quantile(robusta / areas, na.rm=T)


## Generate loess relationship
obs <- arabica / areas
obs[is.na(obs)] <- 0
valid <- !is.na(arabica.predicted) & is.finite(arabica.predicted) & arabica.predicted > 0 & obs > 0

if (sum(valid) > 60000)
    valid <- valid & rep(c(F, T, F), 1036800)

df <- data.frame(yy=as.vector(obs[valid]), xx=as.vector(log(arabica.predicted[valid])))
mod <- loess(yy ~ xx, data=df, span=1/3)
summary(mod)

sum(predict(mod))

xxpred <- seq(min(df$xx), max(df$xx), length.out=100)
yypred <- predict(mod, data.frame(xx=xxpred))

plot(xxpred, yypred, type='l')

library(ggplot2)

ggplot(df, aes(xx, yy)) +
    geom_smooth(method="loess", span=1/3) +
    theme_bw() + xlab("Log relative probability") + ylab("Area under robusta cultivation (loess)") +
    scale_x_continuous(expand=c(0, 0))
ggsave("suitability/outputs/robusta-transform.pdf", width=6, height=4)

write.csv(data.frame(xxpred, yypred), "suitability/outputs/robusta-transform.csv", row.names=F)

## NA-out only the water areas
elev <- get.elev.map()


currmap <- arabica
currpred <- arabica.predicted

existing <- currmap / areas
existing[is.na(existing)] <- 0
existing[is.na(elev)] <- NA

mean(existing == 0 & currpred == 0, na.rm=T)

errors <- pmax(existing - currpred, 0)
mean(errors, na.rm=T)

errors[existing == 0 & currpred == 0] <- NA
mean(errors == 0, na.rm=T)

errors[errors == 0] <- NA
mean(errors, na.rm=T)

newarea <- pmax(pmin(currpred, 1) - existing, 0)
sum(newarea * areas, na.rm=T) / sum(existing * areas, na.rm=T)
sum((currpred > 0) * areas, na.rm=T) / sum((existing > 0) * areas, na.rm=T)

