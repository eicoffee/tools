setwd("~/research/coffee/tools/suitability")

library(ncdf4)
library(maps)
library(RColorBrewer)
library(raster)
source("intake/lib.R") # now up in coffee/tools

elev <- get.elev.map()

variety <- "robusta" #"arabica"

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
harvested <- ncvar_get(database, variety)

## Turn into probabilities
areas <- matrix(NA, nrow=4320, ncol=720)
for (ii in 1:720) {
    # 1/12 degrees
    londist <- 2*pi * 6371 * cos(latitude[ii] * pi / 180) / (360 * 12)
    latdist <- 2*pi * 6371 / (360 * 12)
    areas[, ii] <- londist * latdist * 100 # in Ha
}

harvested <- harvested / areas

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

predicted <- nc_open(paste0("suitability/outputs/", variety, "-product.nc4"))
predicted <- ncvar_get(predicted, "suitability")

map <- predicted - harvested

mask <- matrix(NA, dim(map)[1], dim(map)[2])

urban <- read.csv("data/urban.csv")
protected <- read.csv("data/protected.csv")

mask[(urban$row - 1) * nrow(mask) + urban$col] <- 1 # swap row/col
mask[(protected$row - 1) * nrow(mask) + protected$col] <- 2 # swap row/col

mask[elev == 0] <- 3
mask[is.na(elev)] <- 3

none = map
none[none != 0] <- NA
none[none == 0] <- 1

## Note: Harvested is NA where 0, and almost exactly respects water:
## sum(is.na(elev) & !is.na(harvested))
sum(mask == 1 & predicted > 0)
stats <- data.frame(zeros.both=sum(!is.na(elev) & predicted == 0 & is.na(harvested), na.rm=T),
                    zeros.unallow=sum(!is.na(elev) & !is.na(mask) & harvested > 0, na.rm=T),
                    less=sum(!is.na(elev) & is.na(mask) & predicted < harvested, na.rm=T),
                    greater=sum(!is.na(elev) & ((!is.na(harvested) & predicted > harvested) | (is.na(harvested) & predicted > 0))))
stats / sum(stats)
stats$lessdiff <- mean(-map[!is.na(elev) & is.na(mask) & predicted < harvested], na.rm=T)

library(mapdata)

map[1, 1] <- -1
limit <- max(abs(quantile(map, probs=c(.025, .975), na.rm=T)))
map[map < -limit] <- -limit
map[map > limit] <- limit

png(paste0("suitability/outputs/", variety, "-difference.png"), width=4320, height=720)
par(mar=rep(0, 4))
plot(0, 0, ylim=c(-30, 30), xlim=c(-180, 180), xaxs = "i", yaxs = "i")
image(longitude, latitude, map, col=brewer.pal(11,"PiYG"), add=T)
image(longitude, latitude, none, col="#FDFDFD", add=T) # Need off-white for image stretching
image(longitude, latitude, mask, col=c("#808080", "#808080", "#0000FF"), add=T) # "#00000080",
map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), col="#000000A0", add=T)
dev.off()
