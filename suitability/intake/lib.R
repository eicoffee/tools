setwd("~/research/coffee/tools")

library(rgdal)

calc.corr <- function(map1, map2) {
    cor(c(map1), c(map2), method="spearman", use="complete.obs")
}

soil.rows <- data.frame(soiltype=c(rep("topsoil", 6), rep("botsoil", 6)),
                        component=rep(1:6, 2))

get.soil.map <- function(soil.row) {
    database <- nc_open(paste0("data/sources/", soil.row$soiltype, ".nc4"))
    soil <- ncvar_get(database, "texture")

    noinfo <- is.na(soil[1,,]) | is.na(soil[2,,]) | is.na(soil[3,,]) | (soil[1,,] == 0 & soil[2,,] == 0 & soil[3,,] == 0)

    comp <- soil[soil.row$component,,]
    comp[noinfo] <- NA

    comp
}

get.elev.map <- function() {
    elev <- raster("data/sources/alt_5m_bil/alt.bil")
    elevlats <- 89.9583333333333333 - 0.08333333333333333 * (1:1800 - 1)
    elev <- elev[rev(which(elevlats <= 30 & elevlats >= -30)),,1]
    dim(elev) <- c(4320, 720)

    elev
}

get.clim.map <- function(bio) {
    bioclim <- raster(paste0("data/sources/bio_5m_bil/bio", bio, ".bil"))
    bioclimlats <- 89.9583333333333333 - 0.08333333333333333 * (1:1800 - 1)
    bioclim <- bioclim[rev(which(bioclimlats <= 30 & bioclimlats >= -30)),,1]
    dim(bioclim) <- c(4320, 720)

    bioclim
}

get.travel.map <- function() {
    require(rgdal)
    require(raster)
    x <- new("GDALReadOnlyDataset", "~/research/coffee/tools/data/sources/acc_50k/w001001.adf")
    xx <- asSGDF_GROD(x)
    r <- raster(xx)

    redr <- aggregate(r, fact=10)
    crop.travel <<- crop(redr, extent(-180, 180, -30, 30))

    lats <- 89.9583333333333333 - 0.08333333333333333 * (1:2160 - 1)
    redr2 <- redr[rev(which(lats <= 30 & lats >= -30)),,1]
    dim(redr2) <- c(4320, 720)

    redr2
}

get.gaez.map <- function(variety) {
    library(raster)
    data = raster(paste0("~/research/coffee/tools/data/sources/gaez/constraints-", variety, "-irrig-high-baseline/data.asc"))
    cropped <- crop(data, extent(-180, 180, -30, 30))
    cropped[cropped < 0] <- NA

    rbind(t(as.matrix(cropped)), matrix(NA, 1, 720))
}
