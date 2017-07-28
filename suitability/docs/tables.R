setwd("~/projects/coffee/tools/suitability")

globaldata <- data.frame()

variety <- "robusta" #"arabica" # #

table <- read.csv(paste0("outputs/", variety, "-countries.csv"))
table$avgconf[table$avgconf > 100] <- 100
table$lossperc[is.na(table$lossperc)] <- NA
table$chngperc[table$chngperc > 1000] <- NA
table$harvlossperc[is.nan(table$harvlossperc)] <- 0
table$harvlossperc[table$lossperc == -100] <- -100
table$incconf[table$incconf > 100] <- 100 # infs
table$decconf[table$decconf > 100] <- 100 # infs, and up to 117%
table$harvconf[table$harvconf > 100] <- 100 # don't see any of these

## Add in the harvested area
table$harvest <- NA

faostat <- read.csv("../../database/data/timeseries/fao/faostat.csv")
faostat <- subset(faostat, ElementName == "Area harvested")

skips <- c("World", "Africa", "Eastern Africa", "Middle Africa", "Western Africa", "Americas", "Northern America", "Central America", "Caribbean", "South America", "Asia", "Eastern Asia", "Southern Asia", "South-Eastern Asia", "Western Asia", "Oceania", "Melanesia", "Polynesia", "Least Developed Countries", "Land Locked Developing Countries", "Small Island Developing States", "Low Income Food Deficit Countries", "Net Food Importing Developing Countries", "Cook Islands", "Saint Vincent and the Grenadines")

todo <- unique(faostat$AreaName)

unknown <- c()
for (country in unique(faostat$AreaName)) {
    if (country %in% skips) {
        todo <- todo[todo != country]
        next
    }
    print(country)

    mycountry <- country
    if (mycountry == "Bolivia (Plurinational State of)")
        mycountry <- "Bolivia"
    if (mycountry == "Côte d'Ivoire")
        mycountry <- "C\364te d'Ivoire"
    if (mycountry == "Congo")
        mycountry <- "Republic of Congo"
    if (mycountry == "Cabo Verde")
        mycountry <- "Cape Verde"
    if (mycountry == "Lao People's Democratic Republic")
        mycountry <- "Lao PDR"
    if (mycountry == "Sao Tome and Principe")
        mycountry <- "S\343o Tom\351 and Principe"
    if (mycountry == "China, Taiwan Province of")
        mycountry <- "Taiwan"
    if (mycountry == "United Republic of Tanzania")
        mycountry <- "Tanzania"
    if (mycountry == "United States of America")
        mycountry <- "United States"
    if (mycountry == "Venezuela (Bolivarian Republic of)")
        mycountry <- "Venezuela"
    if (mycountry == "Viet Nam")
        mycountry <- "Vietnam"
    if (mycountry == "French Polynesia")
        mycountry <- "France"

    areas <- subset(faostat, AreaName == country)$Value
    area <- areas[length(areas)]
    if (mycountry == "France") {
        areas <- subset(faostat, AreaName == "Martinique")$Value
        area <- area + areas[length(areas)]
        todo <- todo[todo != "Martinique"]
    }
    if (mycountry == "China") {
        areas <- subset(faostat, AreaName == "China, mainland")$Value
        area <- area + areas[length(areas)]
        todo <- todo[todo != "China, mainland"]
    }

    table$harvest[table$country == mycountry] <- area
    if (sum(table$country == mycountry) == 1 || area == 0)
        todo <- todo[todo != country]
}

table <- table[, c("country", "baseline", "increase", "decrease", "avgconf", "lossperc", "chngperc", "harvest", "harvlossperc", "incconf", "decconf", "harvconf")]

## Make barchart

## Get order right and fix Cote d'Ivoire and Sao Tome and Principe
table$harvest[is.na(table$harvest)] <- 0
table$decconf[table$decconf < 0] <- 0

globaldata <- rbind(globaldata, data.frame(variety=variety, baseline=sum(table$baseline), increase=sum(table$increase), decrease=sum(table$decrease), lossperc=sum(table$decrease) / sum(table$baseline), chngperc=(sum(table$increase) + sum(table$decrease)) / sum(table$baseline), harvest=sum(table$harvest), harvlossperc=sum(table$harvest * table$harvlossperc / 100) / sum(table$harvest), incconf=sum(table$increase * table$incconf, na.rm=T) / sum(table$increase, na.rm=T), decconf=sum(table$decrease * table$decconf, na.rm=T) / sum(table$decrease), harvconf=sum(table$harvest * table$harvlossperc * table$harvconf, na.rm=T) / sum(table$harvest * table$harvlossperc, na.rm=T)))

table$country <- as.character(table$country)
table$country[table$country == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
table$country[table$country == "S\xe3o Tom\xe9 and Principe"] <- "Sao Tome and Principe"

if (variety == "arabica") {
    table2 <- table[table$country != "World" & (table$baseline > 1e6 | table$increase > 1e6 | table$harvest > 1e5),]
} else {
    table2 <- table[table$country != "World" & (table$baseline > 4e6 | table$increase > 4e6 | table$harvest > 1e5),]
}    

data <- data.frame(country=rep(table2$country, 3),
                   positives=c(table2$harvest, pmax(0, table2$baseline - table2$harvest), table2$increase),
                   negatives=c(table2$harvest * table2$harvlossperc / 100, rep(NA, nrow(table2)), table2$decrease - table2$harvest * table2$harvlossperc / 100),
                   posconfs=c(rep(1, 2*nrow(table2)), table2$incconf / 100),
                   negconfs=c(table2$harvconf / 100, rep(NA, nrow(table2)), table2$decconf / 100),
                   group=c(rep("Current cultivation", nrow(table2)),
                       rep("Baseline suitability", nrow(table2)),
                       rep("Suitability change", nrow(table2))))
data$country <- factor(data$country, levels=table2$country[rev(order(table2$harvest))])

library(ggplot2)
library(grid)

## Double-sided sqrt scale, from https://andrewpwheeler.wordpress.com/2015/07/31/custom-square-root-scale-with-negative-values-in-ggplot2-r/
library(scales)
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)

library(R.utils)

ggplot(data, aes(x=country, fill=group)) +
    geom_bar(aes(y=positives / 1e6, alpha=posconfs), stat="identity") +
        geom_bar(aes(y=negatives / 1e6, alpha=negconfs), stat="identity") +
            geom_hline(aes(yintercept=0)) +
                theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), legend.position="top", plot.margin = unit(c(0,1,0,2), "cm"), legend.box = "horizontal") +
                    scale_y_continuous(trans="S_sqrt", expand=c(0, 0)) +
                        xlab("") + ylab("Suitability and changes in millions of hectares") +
                            scale_fill_discrete(name="", breaks=c("Current cultivation", "Baseline suitability", "Suitability change")) +
                                scale_alpha_continuous(name="Confidence", breaks=c(.1, .25, 1), labels=c("10%", "25%", "100%")) +
                                    ggtitle(paste("Changes in", capitalize(variety), "Suitability to 2050"))

ggsave(paste0(variety, "-changes.pdf"), width=9, height=6)

## Make table

## for (ii in 1:nrow(table)) {
##     if (table$increase[ii] > 0)
##         tryCatch({
##             table$incconf[ii] <- uniroot(function(s) pnorm(0, table$increase[ii], s) - (.5 - table$incconf[ii] / 200), c(0, table$increase[ii] * 20))$root
##         }, error=function(e) {
##             table$incconf[ii] <- NA
##         })
##     if (table$decrease[ii] < 0)
##         table$decconf[ii] <- uniroot(function(s) pnorm(0, -table$decrease[ii], s) - (.5 - table$decconf[ii] / 200), c(0, -table$decrease[ii] * 20))$root
##     if (table$harvest[ii] != 0) {
##         if (is.na(table$harvconf[ii]) || table$harvconf[ii] < 5)
##             table$harvconf[ii] <- NA
##         else
##             table$harvconf[ii] <- uniroot(function(s) pnorm(0, -table$harvloss[ii], s) - (.5 - table$harvconf[ii] / 200), c(0, -table$harvloss[ii] * 20))$root
##     }
## }

table <- table[,c("country", "baseline", "increase", "incconf", "decrease", "decconf", "lossperc", "chngperc", "harvest", "harvlossperc", "harvconf")]

names(table) <- c("Country", "Baseline (Ha)", "Increase (Ha)", "IConf. (%)", "Decrease (Ha)", "DConf. (%)", "Loss (%)", "Chng. (%)", "Harvest (Ha)", "H. Loss (%)", "HConf. (%)")

library(xtable)

Sys.setlocale('LC_ALL','C')

xtbl <- xtable(table)
print(xtbl, include.rownames=FALSE)

## Print out global data

globaldata[, 5] <- globaldata[, 5] * 100
globaldata[, 6] <- globaldata[, 6] * 100
globaldata[, 8] <- globaldata[, 8] * 100

globaldata <- globaldata[,c("variety", "baseline", "increase", "incconf", "decrease", "decconf", "lossperc", "chngperc", "harvest", "harvlossperc", "harvconf")]

names(table) <- c("Variety", "Baseline (Ha)", "Increase (Ha)", "IConf. (%)", "Decrease (Ha)", "DConf. (%)", "Loss (%)", "Chng. (%)", "Harvest (Ha)", "H. Loss (%)", "HConf. (%)")

xtbl <- xtable(globaldata, digits=c(0, 0, 0, 0, 2, 0, 2, 1, 1, 0, 1, 2))
print(xtbl, include.rownames=FALSE)
