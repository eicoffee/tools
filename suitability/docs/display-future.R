setwd("~/research/coffee/tools/suitability")

library(ncdf4)
library(maps)
library(RColorBrewer)
library(raster)
source("intake/lib.R") # now up in coffee/tools

do.rescale <- F
fileprefix <- "robusta-future" #"arabica-future"

database <- nc_open(paste0("suitability/outputs/", fileprefix, ".nc4"))
map <- ncvar_get(database, "suitability")
confs <- ncvar_get(database, "confidence")

confs[confs > 1] <- 1 # too high!

database <- nc_open("../database/harvestarea.nc4")
longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

mask <- matrix(NA, dim(map)[1], dim(map)[2])

## Load urban and protected and grey them out
urban <- read.csv("data/urban.csv")
protected <- read.csv("data/protected.csv")
##managed <- read.csv("data/managed.csv")
mask[(urban$row - 1) * nrow(mask) + urban$col] <- 1 # swap row/col
mask[(protected$row - 1) * nrow(mask) + protected$col] <- 2 # swap row/col
##mask[(managed$row - 1) * nrow(mask) + managed$col] <- 3 # swap row/col

## Elevation to grey out oceans
elev <- get.elev.map()

mask[elev == 0] <- 3
mask[is.na(elev)] <- 3

none = map
none[none != 0] <- NA
none[none == 0] <- 1

library(mapdata)

png(paste0("suitability/outputs/", fileprefix, "-suit.png"), width=4320, height=720)
##map("worldHires", ylim=c(-30, 30), mar=rep(0,4))
par(mar=rep(0, 4))
plot(0, 0, ylim=c(-30, 30), xlim=c(-180, 180), xaxs = "i", yaxs = "i")
image(longitude, latitude, map, col=brewer.pal(11,"RdYlGn"), add=T)
image(longitude, latitude, none, col="#FDFDFD", add=T)
map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), col="#000000A0", add=T)
dev.off()

png(paste0("suitability/outputs/", fileprefix, "-conf.png"), width=4320, height=720)
##map("worldHires", ylim=c(-30, 30), mar=rep(0,4))
par(mar=rep(0, 4))
plot(0, 0, ylim=c(-30, 30), xlim=c(-180, 180), xaxs = "i", yaxs = "i")
image(longitude, latitude, confs, col=brewer.pal(9, "Blues"), add=T)
map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), col="#000000A0", add=T)
dev.off()


png(paste0("suitability/outputs/", fileprefix, ".png"), width=4320, height=720)
##map("worldHires", ylim=c(-30, 30), mar=rep(0,4))
par(mar=rep(0, 4))
plot(0, 0, ylim=c(-30, 30), xlim=c(-180, 180), xaxs = "i", yaxs = "i")
image(longitude, latitude, map, col=brewer.pal(11,"RdYlGn"), add=T)
image(longitude, latitude, confs, col=rgb(1, 1, 1, seq(1, 0, by=-.1)), add=T)
image(longitude, latitude, none, col="#FDFDFD", add=T)
image(longitude, latitude, mask, col=c("#FF00FF", "#00FFFF", "#0000FF"), add=T) # "#FFFFFF80",
map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), col="#000000A0", add=T)
dev.off()
