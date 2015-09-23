setwd("~/projects/coffee/tools/climate")

library(ncdf4)
library(maps)
library(RColorBrewer)

mains <- c("Mean temperature", "Diurnal range", "Maximum temperature", "Minimum temperature", "Annual Precipitation", "Wettest month precipitation", "Driest month precipitation")
fileprefixes <- c("bio-1", "bio-2", "bio-5", "bio-6", "bio-12", "bio-13", "bio-14")
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

    png(paste0("outputs/", fileprefix, ".png"), width=4320/2, height=400)
    map("worldHires", ylim=c(-30, 30), mar=rep(0,4))
    image(longitude, latitude, map, col=colors, breaks=breaks, add=T)
    image(longitude, latitude, conf, col=rgb(1, 1, 1, seq(1, 0, by=-.1)), add=T)
    legend("bottomleft", legend=round((breaks[-1] + breaks[-12]) / 2, digits=2), fill=colors, cex=1.5)
    text(-180, 27, mains[ii], pos=4, cex=2)
    dev.off()
}
