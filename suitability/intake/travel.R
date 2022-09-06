setwd("~/research/coffee/tools")

library(ncdf4)
library(ggplot2)
source("suitability/intake/lib.R")

pdf.method <- 'kernel'
do.displays <- F

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")
robusta <- ncvar_get(database, "robusta")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

## Get the variable from the source and write it back out
travel <- get.travel.map()

writeRaster(crop.travel, "data/sources/travel.bil", "BIL") # crop.travel put into global env by get.travel.map

## Extract univariates
if (pdf.method == 'bins') {
    quantile(travel, probs=c(.05, .95), na.rm=T)
    uppers <- c(seq(24, by=24, length.out=99), Inf)
    center <- seq(12, by=24, length.out=100)

    unweighted <- c()
    arabicaed <- c()
    robustaed <- c()

    for (upper in uppers) {
        unweighted <- c(unweighted, sum(travel < upper, na.rm=T))
        arabicaed <- c(arabicaed, sum(arabica[travel < upper], na.rm=T))
        robustaed <- c(robustaed, sum(robusta[travel < upper], na.rm=T))
    }

    unweighted.fractions <- unweighted / unweighted[length(unweighted)]
    arabicaed.fractions <- arabicaed / arabicaed[length(arabicaed)]
    robustaed.fractions <- robustaed / robustaed[length(robustaed)]

    unweighted <- diff(c(0, unweighted.fractions))
    arabicaed <- diff(c(0, arabicaed.fractions))
    robustaed <- diff(c(0, robustaed.fractions))

    data <- data.frame(center, unweighted, arabicaed, robustaed)
} else {
    limits <- quantile(travel, probs=c(.05, .95), na.rm=T)
    data <- kernel.method(travel, arabica, robusta)
}

write.csv(data, file="data/travdist.csv", row.names=F)

if (do.displays) {
    ggplot(subset(data, center < Inf), aes(x=center)) +
        geom_ribbon(aes(ymin=0, ymax=unweighted), fill="#80FF80", alpha=.5, colour="#70C070") +
        geom_line(aes(y=arabicaed), colour=1) + geom_line(aes(y=robustaed), colour=2) +
            scale_colour_discrete(name="Distribution:",
                                  breaks=c(F, T), labels=c("Entire zone", "Coffee-growing")) +
                theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank()) +
                    xlab("") + ylab("") + ggtitle("Travel to nearest >50,000 person city") +
                        scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
    ggsave('suitability/intake/figures/travel.pdf')
}

## Calculate correlations with all soils

data.corrs <- data.frame(soiltype=c(), component=c(), corr=c())

for (ii in 1:nrow(soil.rows)) {
    print(soil.rows[ii, ])
    soilmap <- get.soil.map(soil.rows[ii,])

    data.corrs <- rbind(data.corrs, data.frame(soiltype=soil.rows$soiltype[ii], component=soil.rows$component[ii], corr=calc.corr(soilmap, travel)))
}

write.csv(data.corrs, file="data/travsoilcorr.csv", row.names=F)

elev <- get.elev.map()
data.corrs <- data.frame(corr=calc.corr(travel, elev))

write.csv(data.corrs, file="data/travelevcorr.csv", row.names=F)

data.corrs <- data.frame(bio=c(), corr=c())

for (bio in 1:19) {
    climmap <- get.clim.map(bio)

    data.corrs <- rbind(data.corrs, data.frame(bio, corr=calc.corr(travel, climmap)))
}

write.csv(data.corrs, file="data/travclimcorr.csv", row.names=F)

latmap <- latitude
for (ii in 1:4319) {
    latmap <- rbind(latmap, latitude)
}

data.corrs <- data.frame(corr=calc.corr(travel, latmap))

write.csv(data.corrs, file="data/travlatcorr.csv", row.names=F)
