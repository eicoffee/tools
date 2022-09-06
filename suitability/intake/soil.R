setwd("~/research/coffee/tools")

library(ncdf4)
library(ggplot2)
library(zoo)

pdf.method <- 'kernel' # bins

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

    if (pdf.method == 'bins') {
        uppers.first3 <- 2 * 1:50
        center.first3 <- uppers.first3 - 1
        uppers.second3 <- 100 * exp(2 * 1:50 / 10) / exp(10)
        center.second3 <- 100 * exp((2 * 1:50 - 1) / 10) / exp(10)
    }

    for (component in 1:6) {
        print(component)

        comp <- soil[component,,]
        comp[noinfo] <- NA
        maxcomp <- max(comp, na.rm=T) + 1

        unweighted <- c()
        arabicaed <- c()
        robustaed <- c()

        if (pdf.method == 'bins') {
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
        } else {
            if (component <= 3) {
                unweighted <- density(comp, bw=4, from=0, to=100, na.rm=T)
                valid.arabica <- !is.na(comp) & !is.na(arabica)
                arabicaed <- density(comp[valid.arabica], bw=4, weights=arabica[valid.arabica] / sum(arabica[valid.arabica]), from=0, to=100)$y
                valid.robusta <- !is.na(comp) & !is.na(robusta)
                robustaed <- density(comp[valid.robusta], bw=4, weights=robusta[valid.robusta] / sum(robusta[valid.robusta]), from=0, to=100)$y
                center <- unweighted$x
                unweighted <- unweighted$y
            } else {
                ## unweighted <- density(comp, bw=.01, from=.01, to=.1, na.rm=T)
                ## valid.arabica <- !is.na(comp) & !is.na(arabica)
                ## arabicaed <- density(comp[valid.arabica], bw=.01, weights=arabica[valid.arabica] / sum(arabica[valid.arabica]), from=.01, to=.1)$y
                ## valid.robusta <- !is.na(comp) & !is.na(robusta)
                ## robustaed <- density(comp[valid.robusta], bw=.01, weights=robusta[valid.robusta] / sum(robusta[valid.robusta]), from=.01, to=.1)$y
                ## center <- unweighted$x
                ## unweighted <- unweighted$y

                valid <- !is.na(comp) & comp > 0
                unweighted <- density(log(comp[valid]), bw=.2, from=log(.01), to=log(100))
                valid.arabica <- valid & !is.na(arabica)
                arabicaed <- density(log(comp[valid.arabica]), bw=.2, weights=arabica[valid.arabica] / sum(arabica[valid.arabica]), from=log(.01), to=log(100))$y
                valid.robusta <- valid & !is.na(robusta)
                robustaed <- density(log(comp[valid.robusta]), bw=.2, weights=robusta[valid.robusta] / sum(robusta[valid.robusta]), from=log(.01), to=log(100))$y
                center <- exp(unweighted$x)
                unweighted <- unweighted$y
            }
        }

        data <- rbind(data, data.frame(soiltype, component, center, unweighted, arabicaed, robustaed))
    }
}

data$component <- factor(data$component)
levels(data$component) <- c("sand", "silt", "clay", "carb", "calc", "gyps")

write.csv(data, file="data/soildist.csv", row.names=F)

source("suitability/intake/lib.R")

macros <- subset(data, component %in% c("sand", "silt", "clay"))
macros <- cbind(rbind(macros[, 1:4], macros[, 1:4]), data.frame(Variety=rep(c("Arabica", "Robusta"), each=nrow(macros)), weighted=c(macros$arabicaed, macros$robustaed)))
macros$soiltype <- as.character(macros$soiltype)
macros$soiltype[macros$soiltype == "topsoil"] <- "Top soil"
macros$soiltype[macros$soiltype == "botsoil"] <- "Bottom soil"
macros$component <- as.character(macros$component)
macros$component[macros$component == "sand"] <- "Sand"
macros$component[macros$component == "silt"] <- "Silt"
macros$component[macros$component == "clay"] <- "Clay"

##ggplot(df.rollmean(macros, c("soiltype", "component", "Variety"), paste(macros$soiltype, macros$component, macros$Variety), 2), aes(x=center)) +
ggplot(macros, aes(x=center)) +
    facet_grid(component ~ soiltype) +
    geom_ribbon(aes(ymin=0, ymax=unweighted), fill="#80FF80", alpha=.5, colour="#70C070") +
    geom_line(aes(y=weighted, colour=Variety)) +
        theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank(), panel.spacing.x = unit(1, "lines")) +
        xlab("Percent of soil") + ylab("Probability density") + ggtitle("Macro soil components") +
        scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
ggsave("suitability/intake/figures/macrosoil.pdf", width=6.5, height=5)

micros <- subset(data, component %in% c("carb", "calc", "gyps"))# & center > 0.0055)
micros <- cbind(rbind(micros[, 1:4], micros[, 1:4]), data.frame(Variety=rep(c("Arabica", "Robusta"), each=nrow(micros)), weighted=c(micros$arabicaed, micros$robustaed)))
micros$soiltype <- as.character(micros$soiltype)
micros$soiltype[micros$soiltype == "topsoil"] <- "Top soil"
micros$soiltype[micros$soiltype == "botsoil"] <- "Bottom soil"
micros$component <- as.character(micros$component)
micros$component[micros$component == "carb"] <- "Organic Carbon"
micros$component[micros$component == "calc"] <- "Calcium Carbonate"
micros$component[micros$component == "gyps"] <- "Gypsum"

##ggplot(df.rollmean(micros, c("soiltype", "component", "Variety"), paste(micros$soiltype, micros$component, micros$Variety), 2), aes(x=center)) +
ggplot(micros, aes(x=center)) +
    facet_grid(component ~ soiltype, scales="free_y") +
    geom_ribbon(aes(ymin=0, ymax=unweighted), fill="#80FF80", alpha=.5, colour="#70C070") +
    geom_line(aes(y=weighted, colour=Variety)) +
        theme(legend.position="top", axis.ticks = element_blank(), axis.text.y = element_blank(), panel.spacing.x = unit(1, "lines")) +
        xlab("Percent of soil") + ylab("Probability density") + ggtitle("Micro soil components") +
        scale_x_log10(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
ggsave("suitability/intake/figures/microsoil.pdf", width=6.5, height=5)

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
