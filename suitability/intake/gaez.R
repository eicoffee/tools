setwd("~/research/coffee/tools")

library(ncdf4)
library(ggplot2)
source("suitability/intake/lib.R")

do.displays <- F

## Get harvest area
database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")
robusta <- ncvar_get(database, "robusta")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

## Get the variable from the source and write it back out
gaez.arabica <- get.gaez.map('arabica')
gaez.robusta <- get.gaez.map('robusta')

## Extract univariates
quantile(c(as.vector(gaez.arabica), as.vector(gaez.robusta)), na.rm=T)
uppers <- seq(0, by=1, length.out=101)
center <- c(0, seq(.5, by=1, length.out=100))

df <- data.frame(variety=c(), center=c(), unweighted=c(), weighted=c())

## Arabica
unweighted <- c()
weighted <- c()

for (upper in uppers) {
    unweighted <- c(unweighted, sum(gaez.arabica <= upper, na.rm=T))
    weighted <- c(weighted, sum(arabica[gaez.arabica <= upper], na.rm=T))
}

unweighted.fractions <- unweighted / unweighted[length(unweighted)]
weighted.fractions <- weighted / weighted[length(weighted)]

unweighted <- diff(c(0, unweighted.fractions))
weighted <- diff(c(0, weighted.fractions))

df <- rbind(df, data.frame(variety='arabica', center, unweighted, weighted))

## Robusta
unweighted <- c()
weighted <- c()

for (upper in uppers) {
    unweighted <- c(unweighted, sum(gaez.robusta <= upper, na.rm=T))
    weighted <- c(weighted, sum(robusta[gaez.robusta <= upper], na.rm=T))
}

unweighted.fractions <- unweighted / unweighted[length(unweighted)]
weighted.fractions <- weighted / weighted[length(weighted)]

unweighted <- diff(c(0, unweighted.fractions))
weighted <- diff(c(0, weighted.fractions))

df <- rbind(df, data.frame(variety='robusta', center, unweighted, weighted))

write.csv(df, file="data/gaezdist.csv", row.names=F)

if (do.displays) {
    df$colour <- 1
    df$colour[df$variety == 'robusta'] <- 2
    ggplot(subset(df, center != 0 & center != 99.5), aes(x=center)) +
        geom_ribbon(aes(ymin=0, ymax=unweighted), fill="#80FF80", alpha=.5, colour="#70C070") +
    geom_line(aes(y=weighted), colour=df$colour[df$center != 0 & df$center != 99.5]) +
        facet_grid(variety ~ ., scales="free") +
        scale_colour_discrete(name="Distribution:",
                              breaks=c(F, T), labels=c("Entire zone", "Coffee-growing")) +
        theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank()) +
        xlab("") + ylab("") + ggtitle("Gaez suitability constraints") +
        scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
    ggsave('suitability/intake/figures/gaez.pdf')
}

## Calculate correlations with all soils

data.corrs <- data.frame(soiltype=c(), component=c(), variety=c(), corr=c())

for (ii in 1:nrow(soil.rows)) {
    print(soil.rows[ii, ])
    soilmap <- get.soil.map(soil.rows[ii,])

    data.corrs <- rbind(data.corrs, data.frame(soiltype=soil.rows$soiltype[ii], component=soil.rows$component[ii], variety='arabica', corr=calc.corr(soilmap, gaez.arabica)))
    data.corrs <- rbind(data.corrs, data.frame(soiltype=soil.rows$soiltype[ii], component=soil.rows$component[ii], variety='robusta', corr=calc.corr(soilmap, gaez.robusta)))
}

write.csv(data.corrs, file="data/gaezsoilcorr.csv", row.names=F)

elev <- get.elev.map()
data.corrs <- data.frame(variety=c('arabica', 'robusta'), corr=c(calc.corr(gaez.arabica, elev), calc.corr(gaez.robusta, elev)))

write.csv(data.corrs, file="data/gaezelevcorr.csv", row.names=F)

data.corrs <- data.frame(bio=c(), variety=c(), corr=c())

for (bio in 1:19) {
    climmap <- get.clim.map(bio)

    data.corrs <- rbind(data.corrs, data.frame(bio, variety=c('arabica', 'robusta'), corr=c(calc.corr(gaez.arabica, climmap), calc.corr(gaez.robusta, climmap))))
}

write.csv(data.corrs, file="data/gaezclimcorr.csv", row.names=F)

latmap <- latitude
for (ii in 1:4319) {
    latmap <- rbind(latmap, latitude)
}

data.corrs <- data.frame(variety=c('arabica', 'robusta'), corr=c(calc.corr(gaez.arabica, latmap), calc.corr(gaez.robusta, latmap)))

write.csv(data.corrs, file="data/gaezlatcorr.csv", row.names=F)

travel <- get.travel.map()
data.corrs <- data.frame(variety=c('arabica', 'robusta'), corr=c(calc.corr(gaez.arabica, travel), calc.corr(gaez.robusta, travel)))

write.csv(data.corrs, file="data/gaeztravcorr.csv", row.names=F)
