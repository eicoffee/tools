setwd("~/research/coffee/tools")

library(raster)
library(maps)
library(mapdata)
library(PBSmapping)

filenames <- c("data/sources/alt_5m_bil/alt.bil", "data/sources/bio_5m_bil/bio1.bil", "data/sources/bio_5m_bil/bio12.bil")
outputs <- c("elevmap.png", "tempmap.png", "precipmap.png")
rev.heat.colors <- function(n) {
    rev(heat.colors(n))
}
rev.cm.colors <- function(n) {
    rev(cm.colors(n))
}
colormap <- c(terrain.colors, rev.heat.colors, rev.cm.colors)

for (ii in 1:length(filenames)) {
    print(filenames[ii])
    map <- raster(filenames[ii])
    map <- as.matrix(map[,,1])
    maplats <- 89.9583333333333333 - 0.08333333333333333 * (1:1800 - 1)
    maplons <- (-180 + 0.08333333333333333 / 2) + 0.08333333333333333 * (1:4320 - 1)
    dim(map) <- c(4320, 1800)

    if (ii == 3)
        map[map > 2000] <- 2000

    png(paste0("suitability/docs/", outputs[ii]), width=1000, height=400)
    map("worldHires", xlim=c(-180, 180), ylim=c(-59, 86), mar=rep(0, 4))
    image(maplons, rev(maplats), map[,1800:1], col=colormap[[ii]](20), add=T)
    map("worldHires", col="#00000080", add=T)
    dev.off()
}

## Land use

shape <- importShapefile("data/sources/ne_10m_urban_areas/ne_10m_urban_areas.shp")
proj.abbr <- attr(shape, "projection")

png("suitability/docs/urbanmap.png", width=1000, height=400)
map("worldHires", xlim=c(-180, 180), ylim=c(-59, 86), mar=rep(0, 4))
addPolys(shape, col="#800080", border="#800080")
dev.off()

## Travel time

source("suitability/intake/lib.R")

x <- new("GDALReadOnlyDataset", "~/research/coffee/tools/data/sources/acc_50k/w001001.adf")
xx <- asSGDF_GROD(x)
r <- raster(xx)

redr <- aggregate(r, fact=10)
redr2 <- t(redr[2160:1,,1])
dim(redr2) <- c(4320, 2160)

maplats <- 89.9583333333333333 - 0.08333333333333333 * (1:2160 - 1)
maplons <- (-180 + 0.08333333333333333 / 2) + 0.08333333333333333 * (1:4320 - 1)

png("suitability/docs/travelmap.png", width=1000, height=400)
map("worldHires", xlim=c(-180, 180), ylim=c(-59, 86), mar=rep(0, 4))
image(maplons, rev(maplats), log(redr2), col=topo.colors(20), add=T)
map("worldHires", col="#00000080", add=T)
dev.off()

## DIDN'T USE BELOW

shape <- importShapefile("extdata/WDPA_July2015-shapefile/WDPA_July2015-shapefile-polygons.shp")
proj.abbr <- attr(shape, "projection")
polydata <- attr(shape, "PolyData")

addPolys(shape, col=factor(paste(polydata$IUCN_CAT, polydata$STATUS)))
