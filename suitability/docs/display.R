setwd("~/research/coffee/tools/suitability")

library(ncdf4)
library(maps)
library(RColorBrewer)
library(raster)
source("intake/lib.R") # now up in coffee/tools

elev <- get.elev.map()

database <- nc_open("../database/harvestarea.nc4")
longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

fileprefix <- c("arabica", "robusta", "arabica-product", "robusta-product")
do.rescale <- c(T, T, F, F)

for (ii in 1:length(fileprefix)) {

database <- nc_open(paste0("suitability/outputs/", fileprefix[ii], ".nc4"))
map <- ncvar_get(database, "suitability")

if (do.rescale[ii]) {
    quantile(log(map[map > 0 & map < Inf]) / 50)
    map2 <- log(map) / 50
    map2[map < .05] <- -Inf # Hide beyond 95% CI
    map2[map == Inf] <- max(map2[map2 < Inf]) # Make sure Inf shows up
} else {
    map2 <- map
}

mask <- matrix(NA, dim(map2)[1], dim(map2)[2])

## Load urban and protected and grey them out
## Don't show managed: too confusing
urban <- read.csv("data/urban.csv")
protected <- read.csv("data/protected.csv")
##managed <- read.csv("data/managed.csv")
mask[(urban$row - 1) * nrow(mask) + urban$col] <- 1 # swap row/col
mask[(protected$row - 1) * nrow(mask) + protected$col] <- 2 # swap row/col
##mask[(managed$row - 1) * nrow(mask) + managed$col] <- 3 # swap row/col

## Elevation to grey out oceans
mask[elev == 0] <- 3
mask[is.na(elev)] <- 3

none = map2
none[none != 0] <- NA
none[none == 0] <- 1

library(mapdata)

##pdf("suitability.pdf", width=10, height=3)
png(paste0("suitability/outputs/", fileprefix[ii], ".png"), width=4320, height=720)
##map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), mar=rep(0,4))
par(mar=rep(0, 4))
plot(0, 0, ylim=c(-30, 30), xlim=c(-180, 180), xaxs = "i", yaxs = "i")
image(longitude, latitude, map2, col=c("#f6e6e9", brewer.pal(11,"RdYlGn")), add=T)
image(longitude, latitude, none, col="#FDFDFD", add=T) # Need off-white for image stretching
image(longitude, latitude, mask, col=c("#FF00FF", "#00FFFF", "#0000FF"), add=T) # "#00000080",
map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), col="#000000A0", add=T)
dev.off()

}
