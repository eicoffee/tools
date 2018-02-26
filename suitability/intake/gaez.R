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

## Estimate area at each latitude
percell <- t(matrix(diff(latitude)[1] * 110.567 * diff(longitude)[1] * 111.321 * cos(latitude * pi / 180), length(latitude), length(longitude))) # cell area in km^2

df <- data.frame(variety=c(), center=c(), unweighted=c(), weighted=c())

## Arabica
unweighted <- c()
weighted <- c()

for (upper in uppers) {
    unweighted <- c(unweighted, sum((gaez.arabica <= upper) * percell, na.rm=T))
    weighted <- c(weighted, sum(arabica[gaez.arabica <= upper], na.rm=T) * .01)
}

##unweighted <- unweighted + (148.94e6 - tail(unweighted, 1)) ## Adjust upper = 0 to downweight that rest of earth is 0

unweighted.fractions <- unweighted / unweighted[length(unweighted)]
weighted.fractions <- weighted / weighted[length(weighted)]

unweighted <- diff(c(0, unweighted.fractions))
weighted <- diff(c(0, weighted.fractions))

weighted[1] <- 0
weighted[weighted == 0] <- min(weighted[weighted > 0]) / 2

unweighted0 <- unweighted[1]
unweighted[1] <- 0
unweighted[unweighted == 0] <- unweighted0 / sum(unweighted == 0)

unweighted <- unweighted / sum(unweighted)
weighted <- weighted / sum(weighted)

df <- rbind(df, data.frame(variety='arabica', center, unweighted, weighted))

## Robusta
unweighted <- c()
weighted <- c()

for (upper in uppers) {
    unweighted <- c(unweighted, sum((gaez.robusta <= upper) * percell, na.rm=T))
    weighted <- c(weighted, sum(robusta[gaez.robusta <= upper], na.rm=T) * .01)
}

##unweighted <- unweighted + (148.94e6 - tail(unweighted, 1)) ## Adjust upper = 0 to downweight that rest of earth is 0

unweighted.fractions <- unweighted / unweighted[length(unweighted)]
weighted.fractions <- weighted / weighted[length(weighted)]

unweighted <- diff(c(0, unweighted.fractions))
weighted <- diff(c(0, weighted.fractions))

weighted[1] <- 0
weighted[weighted == 0] <- min(weighted[weighted > 0]) / 2

unweighted0 <- unweighted[1]
unweighted[1] <- 0
unweighted[unweighted == 0] <- unweighted0 / sum(unweighted == 0)

unweighted <- unweighted / sum(unweighted)
weighted <- weighted / sum(weighted)

df <- rbind(df, data.frame(variety='robusta', center, unweighted, weighted))

write.csv(df, file="data/gaezdist.csv", row.names=F)

if (do.displays) {
    df$colour <- 1
    df$colour[df$variety == 'robusta'] <- 2
                                        #ggplot(subset(df, center != 0 & center != 99.5), aes(x=center)) +
                                        #ggplot(subset(df, center != 0), aes(x=center)) +
    ggplot(df, aes(x=center)) +
        geom_ribbon(aes(ymin=0, ymax=unweighted), fill="#80FF80", alpha=.5, colour="#70C070") +
                                        #geom_line(aes(y=weighted), colour=df$colour[df$center != 0 & df$center != 99.5]) +
                                        #geom_line(aes(y=weighted), colour=df$colour[df$center != 0]) +
    geom_line(aes(y=weighted), colour=df$colour) +
        facet_grid(variety ~ ., scales="free") +
        scale_colour_discrete(name="Distribution:",
                              breaks=c(F, T), labels=c("Entire zone", "Coffee-growing")) +
        theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank()) +
        xlab("") + ylab("") + ggtitle("Gaez suitability constraints") +
        scale_x_continuous(expand=c(0, 0)) + scale_y_log10(expand=c(0, 0))
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
