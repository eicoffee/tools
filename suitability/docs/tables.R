setwd("~/research/coffee/tools/suitability")

globaldata <- data.frame()
pooleddata <- data.frame()
save.results <- F

## Run below twice
for (variety in c("arabica", "robusta")) {

table <- read.csv(paste0("outputs/", variety, "-countries.csv"))
table$harvlossperc[is.nan(table$harvlossperc)] <- 0
table$incconf[table$incconf > 100] <- 100 # infs
table$decconf[table$decconf > 100] <- 100 # infs, and up to 141% just on global
table$harvconf[table$harvconf > 100] <- 100 # don't see any of these

## Add in the harvested area
table$faoharvest <- NA

faostat <- read.csv("../../database/sources/timeseries/fao/faostat.csv")
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

    table$faoharvest[table$country == mycountry] <- area
    if (sum(table$country == mycountry) == 1 || area == 0)
        todo <- todo[todo != country]
}

table <- table[, c("country", "baseline", "increase", "decrease", "avgconf", "lossperc", "chngperc", "harvloss", "harvlossperc", "incconf", "decconf", "harvconf", "unharvbase", "harvest", "faoharvest", "unaccharv")]

## Make barchart

## Get order right and fix Cote d'Ivoire and Sao Tome and Principe
table$faoharvest[is.na(table$faoharvest)] <- 0

globaldata <- rbind(globaldata, data.frame(variety=variety, baseline=sum(table$baseline), increase=sum(table$increase), decrease=sum(table$decrease), lossperc=sum(table$decrease) / sum(table$baseline), chngperc=(sum(table$increase) + sum(table$decrease)) / sum(table$baseline), harvest=sum(table$harvest + table$unaccharv), harvlossperc=sum(table$harvest * table$harvlossperc / 100) / sum(table$harvest), incconf=sum(table$increase * table$incconf, na.rm=T) / sum(table$increase, na.rm=T), decconf=sum(table$decrease * table$decconf, na.rm=T) / sum(table$decrease), harvconf=sum(table$harvest * table$harvlossperc * table$harvconf, na.rm=T) / sum(table$harvest * table$harvlossperc, na.rm=T), unharvbase=sum(table$unharvbase, na.rm=T)))

table$country <- as.character(table$country)
table$country[table$country == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
    table$country[table$country == "S\xe3o Tom\xe9 and Principe"] <- "Sao Tome and Principe"

    pooleddata <- rbind(pooleddata, cbind(data.frame(variety), table))

if (variety == "arabica") {
    table2 <- table[table$country != "World" & (table$baseline > 1e6 | table$increase > 1e6 | table$unharvbase + table$faoharvest > 1e5),]
} else {
    table2 <- table[table$country != "World" & (table$baseline > 5e6 | table$increase > 5e6 | table$unharvbase + table$faoharvest > 2e5),]
}

## data <- data.frame(country=rep(table2$country, 3),
##                    positives=c(table2$faoharvest, table2$unharvbase, table2$increase),
##                    negatives=c(table2$faoharvest * table2$harvlossperc / 100, rep(NA, nrow(table2)), pmax(-table2$unharvbase, pmin(0, table2$decrease - table2$faoharvest * table2$harvlossperc / 100))),
##                    posconfs=c(rep(1, 2*nrow(table2)), table2$incconf / 100),
##                    negconfs=c(table2$harvconf / 100, rep(NA, nrow(table2)), table2$decconf / 100),
##                    group=c(rep("Current cultivation", nrow(table2)),
##                        rep("Additional baseline suitability", nrow(table2)),
##                        rep("Suitability change", nrow(table2))))
data <- data.frame(country=rep(table2$country, 4),
                   positives=c(table2$harvest, table2$unaccharv, table2$unharvbase, table2$increase),
                   negatives=c(table2$harvloss, table2$unaccharv * table2$harvlossperc / 100, rep(NA, nrow(table2)), table2$decrease - table2$harvloss),
                   posconfs=c(rep(1, 2*nrow(table2)), table2$avgconf / 100, table2$incconf / 100),
                   negconfs=c(table2$harvconf / 100, (table2$harvconf / 100) * (table2$harvest / (table2$unaccharv + table2$harvest)), rep(NA, nrow(table2)), table2$decconf / 100),
                   group=c(rep("Modeled cultivation", nrow(table2)),
                           rep("Extrapolated cultivation", nrow(table2)),
                           rep("Additional baseline suitability", nrow(table2)),
                           rep("Suitability change", nrow(table2))))
data$country <- factor(data$country, levels=table2$country[rev(order(table2$harvest + table2$unaccharv))])

library(ggplot2)
library(grid)

## Double-sided sqrt scale, from https://andrewpwheeler.wordpress.com/2015/07/31/custom-square-root-scale-with-negative-values-in-ggplot2-r/
library(scales)
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)

library(R.utils)

data$group <- factor(data$group, levels=c("Suitability change", "Additional baseline suitability", "Extrapolated cultivation", "Modeled cultivation"))

    if (save.results) {
        ggplot(data[order(data$group, decreasing=T),], aes(x=country, fill=group)) +
            geom_bar(aes(y=positives / 1e6, alpha=posconfs), stat="identity") +
            geom_bar(aes(y=negatives / 1e6, alpha=negconfs), stat="identity") +
            geom_hline(aes(yintercept=0)) + theme_bw() +
            theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), plot.margin = unit(c(0,1,0,2), "cm")) + # legend.position="top", , legend.box = "horizontal"
            scale_y_continuous(trans="S_sqrt") +
            xlab("") + ylab("Suitability and changes in millions of hectares") +
            scale_fill_manual(name="Suitable Areas (Ha)", breaks=c("Modeled cultivation", "Extrapolated cultivation", "Additional baseline suitability", "Suitability change"), values=rev(c("#66a61e", "#1b9e77", "#7570b3", "#d95f02"))) +
            scale_alpha_continuous(name="Confidence", breaks=c(.1, .25, 1), labels=c("10%", "25%", "100%")) +
            ggtitle(paste("Changes in", capitalize(variety), "Suitability to 2050")) +
            theme(plot.title = element_text(hjust = 0.5)) + theme(legend.justification=c(1,1), legend.position=c(.999,.999), legend.box="horizontal")

        ggsave(paste0("outputs/", variety, "-changes.pdf"), width=9, height=7)
    }

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

table$allharvest <- table$harvest + table$unaccharv
table$harvmodel <- 100 * table$harvest / (table$harvest + table$unaccharv)

table <- table[,c("country", "baseline", "increase", "incconf", "decrease", "decconf", "lossperc", "chngperc", "allharvest", "harvlossperc", "harvconf", "harvmodel", "unharvbase")]

names(table) <- c("Country", "Baseline (Ha)", "Increase (Ha)", "IConf. (%)", "Decrease (Ha)", "DConf. (%)", "Loss (%)", "Chng. (%)", "Harvest (Ha)", "H. Loss (%)", "HConf. (%)", "Modeled (%)", "Unharvested (Ha)")

library(xtable)

Sys.setlocale('LC_ALL','C')

    if (save.results) {
        xtbl <- xtable(table)
        print(xtbl, include.rownames=FALSE, file=paste0("outputs/", variety, "-table.tex"))
    }
}

## Single pooled data figure

inc.countries <- unique(pooleddata$country[pooleddata$unharvbase + pooleddata$faoharvest > 2e5])

table2 <- pooleddata[pooleddata$country != "World" & pooleddata$country %in% inc.countries,]
table2$var2 <- "Arabica"
table2$var2[table2$variety == "robusta"] <- "Robusta"

data <- data.frame(variety=rep(table2$var2, 4), country=rep(table2$country, 4),
                   positives=c(table2$harvest, table2$unaccharv, table2$unharvbase, table2$increase),
                   negatives=c(table2$harvloss, table2$unaccharv * table2$harvlossperc / 100, rep(NA, nrow(table2)), table2$decrease - table2$harvloss),
                   posconfs=c(rep(1, 2*nrow(table2)), table2$avgconf / 100, table2$incconf / 100),
                   negconfs=c(table2$harvconf / 100, (table2$harvconf / 100) * (table2$harvest / (table2$unaccharv + table2$harvest)), rep(NA, nrow(table2)), table2$decconf / 100),
                   group=c(rep("Modeled cultivation", nrow(table2)),
                           rep("Extrapolated cultivation", nrow(table2)),
                           rep("Additional baseline suitability", nrow(table2)),
                           rep("Suitability change", nrow(table2))))

t2arabica <- table2[table2$variety == "arabica",]
corder <- t2arabica$country[order(t2arabica$harvest + t2arabica$unaccharv)]
data$country <- factor(data$country, levels=corder[!duplicated(corder)])
data$group <- factor(data$group, levels=c("Suitability change", "Additional baseline suitability", "Extrapolated cultivation", "Modeled cultivation"))

ggplot(data, aes(x=country, fill=group)) +
    facet_grid(. ~ variety, space="free") + coord_flip() +
    geom_bar(aes(y=positives / 1e6, alpha=posconfs), stat="identity") +
    geom_bar(aes(y=negatives / 1e6, alpha=negconfs), stat="identity") +
    geom_hline(aes(yintercept=0)) + theme_bw() +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), plot.margin = unit(c(0,1,0,2), "cm")) +
    scale_y_continuous(breaks=c(-5:-1, -.5, -.1, 0, .1, .5, 1:4, 6, 9, 12, 14), trans="S_sqrt") +
    xlab("") + ylab("Suitability and changes in millions of hectares") +
    scale_fill_manual(name="Suitable Areas (Ha)", breaks=c("Modeled cultivation", "Extrapolated cultivation", "Additional baseline suitability", "Suitability change"), values=rev(c("#66a61e", "#1b9e77", "#7570b3", "#d95f02"))) +
scale_alpha_continuous(name="Confidence", breaks=c(.1, .25, 1), labels=c("10%", "25%", "100%")) +
    ggtitle(paste("Changes in suitability by country to 2050")) +
    theme(plot.title = element_text(hjust = 0.5)) + theme(legend.justification=c(1,0), legend.position=c(.590,.002), legend.box="vertical")

## Fraction of cultivated area
data <- data.frame(variety=rep(table2$var2, 4), country=rep(table2$country, 4),
                   fracs=c(-table2$harvloss / (table2$harvest + table2$unaccharv),
                   (-table2$unaccharv * table2$harvlossperc / 100) / (table2$harvest + table2$unaccharv),
                   (table2$unaccharv + table2$unaccharv * table2$harvlossperc / 100) / (table2$harvest + table2$unaccharv),
                   (table2$harvest + table2$harvloss) / (table2$harvest + table2$unaccharv)),
                   confs=c(table2$harvconf / 100, (table2$harvconf / 100) * (table2$harvest / (table2$unaccharv + table2$harvest)), rep(1, 2*nrow(table2))),
                   group=c(rep("Harvest area lost", nrow(table2)),
                           rep("Extrapolated area lost", nrow(table2)),
                           rep("Unmodeled area", nrow(table2)),
                           rep("Remaining area", nrow(table2))))
data$group <- factor(data$group, levels=c("Harvest area lost", "Extrapolated area lost", "Unmodeled area", "Remaining area"))
data <- subset(data, country != "Mozambique" & country != "Australia")
data$country <- factor(data$country, levels=rev(levels(data$country)))

ggplot(data, aes(x=country, fill=group)) +
    facet_grid(. ~ variety, space="free") + coord_flip() +
    geom_bar(aes(y=fracs, alpha=confs), stat="identity") +
    theme_bw() +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), plot.margin = unit(c(0,1,0,2), "cm")) +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Lost fraction of harvested area") +
    scale_fill_manual(name="Harvested areas (%)", breaks=c("Harvest area lost", "Extrapolated area lost", "Unmodeled area", "Remaining area"), values=rev(c("#66a61e", "#1b9e77", "#7570b3", "#d95f02"))) +
    scale_alpha_continuous(name="Confidence", breaks=c(.1, .25, 1), labels=c("10%", "25%", "100%")) +
    ggtitle(paste("Changes in harvested area by country to 2050")) +
    theme(plot.title = element_text(hjust = 0.5)) + theme(legend.justification=c(0, 1), legend.position=c(-.45, 1.11), legend.box="vertical", plot.margin=margin(3, 3, 3, 30, "pt"))

## Simplified version

data <- data.frame(variety=rep(table2$var2, 2), country=rep(table2$country, 2),
                   fracs=c(-table2$harvloss / (table2$harvest + table2$unaccharv) +
                           (-table2$unaccharv * table2$harvlossperc / 100) / (table2$harvest + table2$unaccharv),
                   (table2$unaccharv + table2$unaccharv * table2$harvlossperc / 100) / (table2$harvest + table2$unaccharv) +
                   (table2$harvest + table2$harvloss) / (table2$harvest + table2$unaccharv)),
                   confs=c(table2$harvconf / 100, rep(1, nrow(table2))),
                   group=c(rep("Harvest area lost", nrow(table2)),
                           rep("Remaining area", nrow(table2))))
data$group <- factor(data$group, levels=c("Harvest area lost", "Remaining area"))
data <- subset(data, country != "Mozambique" & country != "Australia")
data$country <- factor(data$country, levels=rev(levels(data$country)))

ggplot(data, aes(x=country, fill=group)) +
    facet_grid(. ~ variety, space="free") + coord_flip() +
    geom_bar(aes(y=fracs, alpha=confs), stat="identity") +
    theme_bw() +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), plot.margin = unit(c(0,1,0,2), "cm")) +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Lost fraction of harvested area") +
    scale_fill_manual(name="Harvested areas (%)", breaks=c("Harvest area lost", "Remaining area"), values=rev(c("#66a61e", "#d95f02"))) +
    scale_alpha_continuous(name="Confidence", breaks=c(.1, .25, 1), labels=c("10%", "25%", "100%")) +
    ggtitle(paste("Changes in harvested area by country to 2050")) +
    theme(plot.title = element_text(hjust = 0.5)) + theme(legend.justification=c(0, 1), legend.position=c(-.45, .65), legend.box="vertical", plot.margin=margin(3, 3, 3, 30, "pt"))

## Bars of available remaining

data <- data.frame(variety=rep(table2$var2, 3), country=rep(table2$country, 3),
                   fracs=c(table2$harvest / (table2$harvest + table2$unharvbase), 1 - table2$harvest / (table2$harvest + table2$unharvbase) - (-table2$decrease + table2$harvloss) / (table2$harvest + table2$unharvbase),
                           (-table2$decrease + table2$harvloss) / (table2$harvest + table2$unharvbase)),
                   confs=c(rep(1, nrow(table2)), table2$avgconf / 100, table2$decconf / 100),
                   group=c(rep("Harvest area", nrow(table2)),
                           rep("Remaining available", nrow(table2)),
                           rep("Loss of available", nrow(table2))))
data$group <- factor(data$group, levels=rev(c("Harvest area", "Remaining available", "Loss of available")))
data <- subset(data, country != "Mozambique" & country != "Australia")
data$country <- factor(data$country, levels=rev(levels(data$country)))

## Zero out harvested area if nothing in usda
usda <- read.csv("../../database/sources/timeseries/usda/usdafas.csv")
library(dplyr)
usda.country <- usda %>% group_by(Country_Name) %>% summarize(arabica=rev(Value[Attribute_Description == "Arabica Production"])[1], robusta=rev(Value[Attribute_Description == "Robusta Production"])[1])
usda.country <- rbind(usda.country, data.frame(Country_Name="Democratic Republic of the Congo", arabica=1, robusta=1))
data2 <- rbind(data[data$variety == 'Arabica' & (data$country %in% usda.country$Country_Name[usda.country$arabica > 0] | data$group != 'Harvest area'),],
               data[data$variety == 'Robusta' & (data$country %in% usda.country$Country_Name[usda.country$robusta > 0] | data$group != 'Harvest area'),])

ggplot(data2, aes(x=country, fill=group)) +
    facet_grid(. ~ variety, space="free") + coord_flip() +
    geom_bar(aes(y=fracs, alpha=confs), stat="identity", position="fill") +
    theme_bw() +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), plot.margin = unit(c(0,1,0,2), "cm")) +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Remaining suitability") +
    scale_fill_manual(name="Available suitability (%)", breaks=c("Harvest area", "Remaining available", "Loss of available"), values=rev(c("#66a61e", "#7570b3", "#d95f02"))) +
    scale_alpha_continuous(name="Confidence", breaks=c(.1, .25, 1), labels=c("10%", "25%", "100%")) +
    ggtitle(paste("Changes in suitable area by country to 2050")) +
    theme(plot.title = element_text(hjust = 0.5)) + theme(legend.justification=c(0, 1), legend.position=c(-.45, .65), legend.box="vertical", plot.margin=margin(3, 3, 3, 30, "pt"))

## Print out global data

globaldata[, 5] <- globaldata[, 5] * 100
globaldata[, 6] <- globaldata[, 6] * 100
globaldata[, 8] <- globaldata[, 8] * 100

globaldata <- globaldata[,c("variety", "baseline", "increase", "incconf", "decrease", "decconf", "lossperc", "chngperc", "harvest", "harvlossperc", "harvconf")]

names(globaldata) <- c("Variety", "Baseline (Ha)", "Increase (Ha)", "IConf. (%)", "Decrease (Ha)", "DConf. (%)", "Loss (%)", "Chng. (%)", "Harvest (Ha)", "H. Loss (%)", "HConf. (%)")

xtbl <- xtable(globaldata, digits=c(0, 0, 0, 0, 2, 0, 2, 1, 1, 0, 1, 2))
print(xtbl, include.rownames=FALSE, file=paste0("outputs/sumtbl.tex"))
