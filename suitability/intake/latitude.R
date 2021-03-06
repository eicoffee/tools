setwd("~/projects/coffee/tools")

library(ncdf4)
library(ggplot2)
source("intake/lib.R")

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")
robusta <- ncvar_get(database, "robusta")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

## Extract univariates

data <- data.frame(variety=c(), center=c(), weighted=c())

for (variety.name in c('arabica', 'robusta')) {
    print(variety.name)
    if (variety.name == 'arabica')
        variety <- arabica
    else
        variety <- robusta

    uppers <- seq(-29, 30, by=1)
    center <- uppers - .5

    weighted <- c()

    for (upper in uppers) {
        validlat <- latitude < upper
        weighted <- c(weighted, sum(variety[, validlat], na.rm=T))
    }

    weighted.fractions <- weighted / weighted[length(weighted)]

    weighted <- diff(c(0, weighted.fractions))

    data <- rbind(data, data.frame(variety=variety.name, center, weighted))
}

ggplot(data, aes(x=center, y=weighted, colour=variety)) +
    geom_line() +
    scale_colour_discrete(name="Latitude Distribution:",
                          breaks=c('arabica', 'robusta'), labels=c("Arabica", "Robusta")) +
        theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank()) +
            xlab("Latitude") + ylab("") + ggtitle("Distribution of cultivation by latitude") +
                scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))

write.csv(data, file="data/latdist.csv", row.names=F)

## Calculate correlations with soil, elevation, bioclim

latmap <- latitude
for (ii in 1:4319) {
    print(ii)
    latmap <- rbind(latmap, latitude)
}

elev <- get.elev.map()
data.corrs <- data.frame(corr=calc.corr(elev, latmap))

write.csv(data.corrs, file="data/latelevcorr.csv", row.names=F)

data.corrs <- data.frame(soiltype=c(), component=c(), corr=c())

for (ii in 1:nrow(soil.rows)) {
    print(soil.rows[ii, ])
    soilmap <- get.soil.map(soil.rows[ii,])

    data.corrs <- rbind(data.corrs, data.frame(soiltype=soil.rows$soiltype[ii], component=soil.rows$component[ii], corr=calc.corr(soilmap, latmap)))
}

write.csv(data.corrs, file="data/latsoilcorr.csv", row.names=F)

data.corrs <- data.frame(bioclim=c(), corr=c())

for (bio in 1:19) {
    climmap <- get.clim.map(bio)
    print(bio)

    data.corrs <- rbind(data.corrs, data.frame(bioclim=bio, corr=calc.corr(climmap, latmap)))
}

write.csv(data.corrs, file="data/latclimcorr.csv", row.names=F)
