setwd("~/research/coffee/tools/suitability")

library(ncdf4)
library(raster)
library(ggplot2)
library(zoo)
source("intake/lib.R")

pdf.method <- 'kernel' # bins
do.displays <- F

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")
robusta <- ncvar_get(database, "robusta")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

data <- data.frame(bioclim=c(), center=c(), unweighted=c(), arabicaed=c(), robustaed=c())

## Get each variable
for (bio in 1:19) {
    print(bio)
    bioclim <- get.clim.map(bio)

    ## Extract univariates
    if (pdf.method == 'bins') {
        limits <- quantile(bioclim, probs=c(.05, .95), na.rm=T)
        center <- seq(limits[1], limits[2], length.out=100)
        uppers <- c((center[-length(center)] + center[-1]) / 2, Inf)

        unweighted <- c()
        arabicaed <- c()
        robustaed <- c()

        for (upper in uppers) {
            unweighted <- c(unweighted, sum(bioclim < upper, na.rm=T))
            arabicaed <- c(arabicaed, sum(arabica[bioclim < upper], na.rm=T))
            robustaed <- c(robustaed, sum(robusta[bioclim < upper], na.rm=T))
        }

        unweighted.fractions <- unweighted / unweighted[length(unweighted)]
        arabicaed.fractions <- arabicaed / arabicaed[length(arabicaed)]
        robustaed.fractions <- robustaed / robustaed[length(robustaed)]

        unweighted <- diff(c(0, unweighted.fractions))
        arabicaed <- diff(c(0, arabicaed.fractions))
        robustaed <- diff(c(0, robustaed.fractions))
    } else {
        limits <- quantile(bioclim, probs=c(.05, .95), na.rm=T)
        unweighted <- density(bioclim, from=limits[1], to=limits[2], na.rm=T)

        valid.arabica <- !is.na(bioclim) & !is.na(arabica)
        arabicaed <- density(bioclim[valid.arabica], weights=arabica[valid.arabica] / sum(arabica[valid.arabica]), from=limits[1], to=limits[2])$y
        valid.robusta <- !is.na(bioclim) & !is.na(robusta)
        robustaed <- density(bioclim[valid.robusta], weights=robusta[valid.robusta] / sum(robusta[valid.robusta]), from=limits[1], to=limits[2])$y
        center <- unweighted$x
        unweighted <- unweighted$y
    }

    data <- rbind(data, data.frame(bioclim=bio, center, unweighted, arabicaed, robustaed))
}

write.csv(data, file="data/climdist.csv", row.names=F)

if (do.displays) {
    names <- c("Annual Mean Temperature", "Mean Diurnal Range (Mean of monthly (max temp - min temp))", "Isothermality (BIO2/BIO7) (* 100)", "Temperature Seasonality (standard deviation *100)", "Max Temperature of Warmest Month", "Min Temperature of Coldest Month", "Temperature Annual Range (BIO5-BIO6)", "Mean Temperature of Wettest Quarter", "Mean Temperature of Driest Quarter", "Mean Temperature of Warmest Quarter", "Mean Temperature of Coldest Quarter", "Annual Precipitation", "Precipitation of Wettest Month", "Precipitation of Driest Month", "Precipitation Seasonality (Coefficient of Variation)", "Precipitation of Wettest Quarter", "Precipitation of Driest Quarter", "Precipitation of Warmest Quarter", "Precipitation of Coldest Quarter")

    for (bio in 1:19) {
        if (pdf.method == 'bins') {
            ## Apply rollmean of 2
            ggplot(subset(data, center < Inf & bioclim == bio), aes(x=c(NA, rollmean(center, 2)))) +
                geom_ribbon(aes(ymin=0, ymax=c(NA, rollmean(unweighted, 2))), fill="#80FF80", alpha=.5, colour="#70C070") +
                geom_line(aes(y=c(NA, rollmean(arabicaed, 2))), colour=1) + geom_line(aes(y=c(NA, rollmean(robustaed, 2))), colour=2) +
                scale_colour_discrete(name="Distribution:",
                                      breaks=c(F, T), labels=c("Entire zone", "Coffee-growing")) +
                theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank()) +
                xlab("") + ylab("") + ggtitle(names[bio]) +
                scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
        } else {
            ggplot(subset(data, center < Inf & bioclim == bio), aes(x=center)) +
                geom_ribbon(aes(ymin=0, ymax=unweighted), fill="#80FF80", alpha=.5, colour="#70C070") +
                geom_line(aes(y=arabicaed), colour=1) + geom_line(aes(y=robustaed), colour=2) +
                scale_colour_discrete(name="Distribution:",
                                      breaks=c(F, T), labels=c("Entire zone", "Coffee-growing")) +
                theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank()) +
                xlab("") + ylab("") + ggtitle(names[bio]) +
                scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
        }
        ggsave(paste0('suitability/intake/figures/bioclim', bio, '.pdf'))
    }
}

## Calculate correlations with all soils

data.corrs <- data.frame(bioclim=c(), soiltype=c(), component=c(), corr=c())

for (bio in 1:19) {
    climmap <- get.clim.map(bio)
    for (ii in 1:nrow(soil.rows)) {
        print(cbind(data.frame(bio), soil.rows[ii, ]))
        soilmap <- get.soil.map(soil.rows[ii,])

        data.corrs <- rbind(data.corrs, data.frame(bioclim=bio, soiltype=soil.rows$soiltype[ii], component=soil.rows$component[ii], corr=calc.corr(soilmap, climmap)))
    }
}

write.csv(data.corrs, file="data/climsoilcorr.csv", row.names=F)

data.corrs <- data.frame(bioclim=c(), corr=c())

for (bio in 1:19) {
    print(bio)
    climmap <- get.clim.map(bio)
    elev <- get.elev.map()
    data.corrs <- rbind(data.corrs, data.frame(bioclim=bio, corr=calc.corr(climmap, elev)))
}

write.csv(data.corrs, file="data/climelevcorr.csv", row.names=F)

data.corrs <- data.frame(bioclim1=c(), bioclim2=c(), corr=c())

for (bio1 in 1:19) {
    climmap1 <- get.clim.map(bio1)
    for (bio2 in (bio1 + 1):19) {
        print(c(bio1, bio2))
        climmap2 <- get.clim.map(bio2)

        data.corrs <- rbind(data.corrs, data.frame(bioclim1=bio1, bioclim2=bio2, corr=calc.corr(climmap1, climmap2)))
    }
}

write.csv(data.corrs, file="data/climcorr.csv", row.names=F)
