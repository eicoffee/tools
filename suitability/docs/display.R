setwd("~/projects/coffee/tools/suitability")

library(ncdf4)
library(maps)
library(RColorBrewer)
library(raster)
source("intake/lib.R") # now up in coffee/tools

do.rescale <- F #T
fileprefix <- "robusta-product" #"robusta"

database <- nc_open(paste0("suitability/outputs/", fileprefix, ".nc4"))
map <- ncvar_get(database, "suitability")

if (do.rescale) {
    quantile(log(map[map > 0 & map < Inf]) / 50)
    map2 <- log(map) / 50
    map2[map < .05] <- -Inf # Hide beyond 95% CI
    map2[map == Inf] <- max(map2[map2 < Inf]) # Make sure Inf shows up
} else {
    map2 <- map
}

database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

mask <- matrix(NA, dim(map2)[1], dim(map2)[2])

## Load urban and protected and grey them out
urban <- read.csv("data/urban.csv")
protected <- read.csv("data/protected.csv")
managed <- read.csv("data/managed.csv")
mask[(urban$row - 1) * nrow(mask) + urban$col] <- 1 # swap row/col
mask[(protected$row - 1) * nrow(mask) + protected$col] <- 2 # swap row/col
mask[(managed$row - 1) * nrow(mask) + managed$col] <- 3 # swap row/col

## Elevation to grey out oceans
elev <- get.elev.map()

mask[elev == 0] <- 4
mask[is.na(elev)] <- 4

none = map2
none[none != 0] <- NA
none[none == 0] <- 1

library(mapdata)

##pdf("suitability.pdf", width=10, height=3)
png(paste0("suitability/outputs/", fileprefix, ".png"), width=4320/2, height=400)
map("worldHires", ylim=c(-30, 30), mar=rep(0,4))
image(longitude, latitude, map2, col=brewer.pal(11,"RdYlGn"), add=T)
image(longitude, latitude, none, col="black", add=T)
image(longitude, latitude, mask, col=c("#FF00FF", "#00FFFF", "#FFFFFF80", "#0000FF"), add=T)
dev.off()
