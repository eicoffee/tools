setwd("~/projects/coffee/tools")

library(ncdf4)
library(ggplot2)

database <- nc_open("../database/harvestarea.nc4")
arabica <- ncvar_get(database, "arabica")
robusta <- ncvar_get(database, "robusta")

longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

data <- data.frame(soiltype=c(), component=c(), center=c(), unweighted=c(), arabicaed=c(), robustaed=c())

for (soiltype in c("topsoil", "botsoil")) {
    database <- nc_open(paste0("data/sources/", soiltype, ".nc4"))
    soil <- ncvar_get(database, "texture")

    noinfo <- is.na(soil[1,,]) | is.na(soil[2,,]) | is.na(soil[3,,]) | (soil[1,,] == 0 & soil[2,,] == 0 & soil[3,,] == 0)

    uppers.first3 <- 2 * 1:50
    center.first3 <- uppers.first3 - 1
    uppers.second3 <- 100 * exp(2 * 1:50 / 10) / exp(10)
    center.second3 <- 100 * exp((2 * 1:50 - 1) / 10) / exp(10)

    for (component in 1:6) {
        print(component)

        comp <- soil[component,,]
        comp[noinfo] <- NA
        maxcomp <- max(comp, na.rm=T) + 1

        unweighted <- c()
        arabicaed <- c()
        robustaed <- c()

        if (component <= 3) {
            uppers.this <- uppers.first3
            center.this <- center.first3
        } else {
            uppers.this <- uppers.second3
            center.this <- center.second3
        }

        for (uppers in uppers.this[uppers.this <= maxcomp]) {
            unweighted <- c(unweighted, sum(comp < uppers, na.rm=T))
            arabicaed <- c(arabicaed, sum(arabica[comp < uppers], na.rm=T))
            robustaed <- c(robustaed, sum(robusta[comp < uppers], na.rm=T))
        }
        center <- center.this[uppers.this <= maxcomp]
        unweighted.fractions <- unweighted / unweighted[length(unweighted)]
        arabicaed.fractions <- arabicaed / arabicaed[length(arabicaed)]
        robustaed.fractions <- robustaed / robustaed[length(robustaed)]

        unweighted <- diff(c(0, unweighted.fractions))
        arabicaed <- diff(c(0, arabicaed.fractions))
        robustaed <- diff(c(0, robustaed.fractions))

        data <- rbind(data, data.frame(soiltype, component, center, unweighted, arabicaed, robustaed))
    }
}

data$component <- factor(data$component)
levels(data$component) <- c("sand", "silt", "clay", "carb", "calc", "gyps")

write.csv(data, file="data/soildist.csv", row.names=F)

data.corrs <- data.frame(soiltype1=c(), soiltype2=c(), component1=c(), component2=c(), corr=c())

for (soiltype1 in c("topsoil", "botsoil")) {
    database <- nc_open(paste0("data/", soiltype1, ".nc4"))
    soil1 <- ncvar_get(database, "texture")

    noinfo1 <- is.na(soil1[1,,]) | is.na(soil1[2,,]) | is.na(soil1[3,,]) | (soil1[1,,] == 0 & soil1[2,,] == 0 & soil1[3,,] == 0)

    for (soiltype2 in c("topsoil", "botsoil")) {
        database <- nc_open(paste0("data/", soiltype2, ".bak.nc4"))
        soil2 <- ncvar_get(database, "texture")

        noinfo2 <- is.na(soil2[1,,]) | is.na(soil2[2,,]) | is.na(soil2[3,,]) | (soil2[1,,] == 0 & soil2[2,,] == 0 & soil2[3,,] == 0)

        for (component1 in 1:6) {
            for (component2 in 1:6) {
                print(c(component1, component2))
                comp1 <- soil1[component1,,]
                comp1[noinfo1] <- NA

                comp2 <- soil2[component2,,]
                comp2[noinfo2] <- NA

                data.corrs <- rbind(data.corrs, data.frame(soiltype1, soiltype2, component1, component2, corr=cor(comp1, comp2, method="spearman")))
            }
        }
    }
}

write.csv(data.corrs, file="data/soilcorr.csv", row.names=F)
