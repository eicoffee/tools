setwd("~/projects/coffee/tools")

library(ncdf4)
library(raster)
library(ggplot2)
source("intake/lib.R")

do.displays <- F

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")
robusta <- ncvar_get(database, "robusta")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

image(longitude, latitude, arabica)

## Get the elevation
elev <- get.elev.map()

image(longitude, latitude, elev)

## Extract univariates

data <- data.frame(variety=c(), center=c(), unweighted=c(), weighted=c())

for (variety.name in c('arabica', 'robusta')) {
    print(variety.name)
    if (variety.name == 'arabica')
        variety <- arabica
    else
        variety <- robusta

    uppers <- c(seq(-10, 2500, by=10), Inf)
    center <- uppers - 5

    unweighted <- c()
    weighted <- c()

    for (upper in uppers) {
        unweighted <- c(unweighted, sum(elev < upper, na.rm=T))
        weighted <- c(weighted, sum(variety[elev < upper], na.rm=T))
    }

    unweighted.fractions <- unweighted / unweighted[length(unweighted)]
    weighted.fractions <- weighted / weighted[length(weighted)]

    unweighted <- diff(c(0, unweighted.fractions))
    weighted <- diff(c(0, weighted.fractions))

    data <- rbind(data, data.frame(variety=variety.name, center, unweighted, weighted))
}

write.csv(data, file="data/elevdist.csv", row.names=F)

if (do.displays) {
    ggplot(data[data$center < Inf,], aes(x=center, y=weighted)) +
        facet_grid(variety ~ ., scale="free_y") +
            geom_ribbon(aes(ymin=0, ymax=unweighted), fill="#80FF80", alpha=.5, colour="#70C070") + geom_line() +
    scale_colour_discrete(name="Elevation Distribution:",
                          breaks=c(F, T), labels=c("Entire zone", "Coffee-growing")) +
        theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank()) +
            xlab("Elevation") + ylab("") + ggtitle("Distribution of cultivation by elevation") +
                scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
}

## Calculate correlations with all soils

data.corrs <- data.frame(soiltype=c(), component=c(), corr=c())

for (ii in 1:nrow(soil.rows)) {
    print(soil.rows[ii, ])
    soilmap <- get.soil.map(soil.rows[ii,])

    data.corrs <- rbind(data.corrs, data.frame(soiltype=soil.rows$soiltype[ii], component=soil.rows$component[ii], corr=calc.corr(soilmap, elev)))
}

write.csv(data.corrs, file="data/elevsoilcorr.csv", row.names=F)
