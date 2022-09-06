setwd("~/research/coffee/tools/suitability")

library(ncdf4)
library(maps)
library(RColorBrewer)
library(raster)
source("intake/lib.R") # now up in coffee/tools

elev <- get.elev.map()

variety <- "arabica" #"robusta"

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
harvested <- ncvar_get(database, variety)

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

map <- harvested / areas

mask <- matrix(NA, dim(map)[1], dim(map)[2])

urban <- read.csv("data/urban.csv")
protected <- read.csv("data/protected.csv")

mask[(urban$row - 1) * nrow(mask) + urban$col] <- 1 # swap row/col
mask[(protected$row - 1) * nrow(mask) + protected$col] <- 2 # swap row/col

mask[elev == 0] <- 3
mask[is.na(elev)] <- 3

none = map
none[none != 0] <- NA
none[is.na(none) || none == 0] <- 1

library(mapdata)

limit <- quantile(map, probs=.975, na.rm=T)
map[map > limit] <- limit

png(paste0("suitability/outputs/", variety, "-database.png"), width=4320, height=720)
par(mar=rep(0, 4))
plot(0, 0, ylim=c(-30, 30), xlim=c(-180, 180), xaxs = "i", yaxs = "i")
image(longitude, latitude, none, col="#FDFDFD", add=T) # Need off-white for image stretching
image(longitude, latitude, map, col=brewer.pal(7,"RdYlGn"), breaks=c(0, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1), add=T)
image(longitude, latitude, mask, col=c("#FF00FF", "#00FFFF", "#0000FF"), add=T) # "#FFFFFF80",
map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), col="#000000A0", add=T)
if (variety == "arabica") {
    map("worldHires", "Colombia", col="#8F00F8", lwd=5, add=T)
    map("worldHires", "El Salvador", col="#8F00F8", lwd=5, add=T)
    map("worldHires", "Ethiopia", col="#8F00F8", lwd=5, add=T)
    map("worldHires", "Mexico", col="#8F00F8", lwd=5, add=T)
    map("worldHires", "Nicaragua", col="#8F00F8", lwd=5, add=T)
}
map("worldHires", "Guatemala", col="#8F00F8", lwd=5, add=T)
map("worldHires", "Indonesia", col="#8F00F8", lwd=5, add=T)
map("worldHires", "Tanzania", col="#8F00F8", lwd=5, add=T)
dev.off()
