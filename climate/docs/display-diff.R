setwd("~/projects/coffee/tools/climate")

library(ncdf4)
library(maps)
library(RColorBrewer)
source("../lib/mapper.R")

do.versb <- T

mains <- c("Mean temperature", "Diurnal range", "Maximum temperature", "Minimum temperature", "Annual Precipitation", "Wettest month precipitation", "Driest month precipitation")
if (do.versb) {
    fileprefixes <- c("bio-1b", "bio-2b", "bio-5b", "bio-6b", "bio-12b", "bio-13b", "bio-14b")
} else {
    fileprefixes <- c("bio-1", "bio-2", "bio-5", "bio-6", "bio-12", "bio-13", "bio-14")
}
isprecips <- c(rep(F, 4), rep(T, 3))

for (ii in 1:length(mains)) {
    fileprefix <- fileprefixes[ii]
    database <- nc_open(paste0("outputs/", fileprefix, ".nc4"))
    map <- ncvar_get(database, "change")
    conf <- ncvar_get(database, "confidence")

    conf[conf > 1] <- 1

    longitude <- seq(-180 + .5 / 12, 180 - .5 / 12, by=1/12)
    latitude <- seq(-30 + .5 / 12, 30 - .5 / 12, by=1/12)

    library(mapdata)

    absmap <- abs(map)
    maxmap <- max(absmap[absmap < Inf], na.rm=T)

    if (isprecips[ii]) {
        maxmap <- .5 * maxmap
        colors <- brewer.pal(11,"RdYlBu") #rev(brewer.pal(11,"RdYlBu")), for precip
    } else {
        colors <- rev(brewer.pal(11,"RdYlBu"))
    }

    breaks <- seq(-maxmap, maxmap, length.out=12)

    png(paste0("outputs/", fileprefix, ".png"), width=1850, height=900)
    par(mar=c(0, 0, 0, 0))
    mapperImage(map, colors, breaks=breaks)
    mapperImage(conf, rgb(1, 1, 1, seq(1, 0, by=-.1)), add=T)
    addMap(border="#00000060")
    addSeams(col="#00000040")

    ##map("worldHires", ylim=c(-30, 30), mar=rep(0,4), bg="#8080FF")
    ##image(longitude, latitude, map, col=colors, breaks=breaks, add=T)
    ##image(longitude, latitude, is.na(map), col="blue", add=T)
    ##image(longitude, latitude, conf, col=rgb(1, 1, 1, seq(1, 0, by=-.1)), add=T)
    legend("bottomleft", legend=round((breaks[-1] + breaks[-12]) / 2, digits=2), fill=colors, cex=1.7)
    text(-3.0, -.3, mains[ii], pos=4, cex=2.5)
    dev.off()
}
