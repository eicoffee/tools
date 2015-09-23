setwd("~/projects/coffee/tools")

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

## DIDN'T USE BELOW

shape <- importShapefile("extdata/WDPA_July2015-shapefile/WDPA_July2015-shapefile-polygons.shp")
proj.abbr <- attr(shape, "projection")
polydata <- attr(shape, "PolyData")

addPolys(shape, col=factor(paste(polydata$IUCN_CAT, polydata$STATUS)))
