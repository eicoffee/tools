setwd("~/projects/coffee/tools/suitability")

globaldata <- data.frame()

variety <- "robusta" # "arabica"

table <- read.csv(paste0("outputs/", variety, "-countries.csv"))
table$avgconf[table$avgconf > 100] <- 100
table$lossperc[is.na(table$lossperc)] <- NA
table$chngperc[table$chngperc > 1000] <- NA
table$harvlossperc[is.nan(table$harvlossperc)] <- 0
table$harvlossperc[table$lossperc == -100] <- -100

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

table <- table[, c("country", "baseline", "increase", "decrease", "avgconf", "lossperc", "chngperc", "harvest", "harvlossperc")]

## Make barchart

## Get order right and fix Cote d'Ivoire and Sao Tome and Principe
table$harvest[is.na(table$harvest)] <- 0

globaldata <- rbind(globaldata, data.frame(variety=variety, baseline=sum(table$baseline), increase=sum(table$increase), decrease=sum(table$decrease), lossperc=sum(table$decrease) / sum(table$baseline), chngperc=(sum(table$increase) + sum(table$decrease)) / sum(table$baseline), harvest=sum(table$harvest), harvlossperc=sum(table$harvest * table$harvlossperc / 100) / sum(table$harvest)))

table$country <- as.character(table$country)
table$country[table$country == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
table$country[table$country == "S\xe3o Tom\xe9 and Principe"] <- "Sao Tome and Principe"

table2 <- table[table$baseline > 1e6 | table$increase > 1e6 | table$harvest > 1e5,]

data <- data.frame(country=rep(table2$country, 3),
                   positives=c(table2$harvest, pmax(0, table2$baseline - table2$harvest), table2$increase),
                   negatives=c(table2$harvest * table2$harvlossperc / 100, rep(NA, nrow(table2)), table2$decrease - table2$harvest * table2$harvlossperc / 100),
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

ggplot(data, aes(x=country, fill=group)) +
    geom_bar(aes(y=positives), stat="identity") +
        geom_bar(aes(y=negatives), stat="identity") +
            geom_hline(aes(yintercept=0)) +
            theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), legend.position="top", plot.margin = unit(c(0,1,0,2), "cm")) + scale_y_continuous(trans="S_sqrt", expand=c(0, 0)) +
                xlab("") + ylab("Suitability and changes") +
                    scale_fill_discrete(name="", breaks=c("Current cultivation", "Baseline suitability", "Suitability change"))

ggsave("new.pdf", width=7, height=4)
error()
## Make table

names(table) <- c("Country", "Baseline (Ha)", "Increase (Ha)", "Decrease (Ha)", "Conf. (%)", "Loss (%)", "Chng. (%)", "Harvest (Ha)", "H. Loss (%)")

library(xtable)

Sys.setlocale('LC_ALL','C')

xtbl <- xtable(table)
print(xtbl, include.rownames=FALSE)

## Print out global data

globaldata[, 5] <- globaldata[, 5] * 100
globaldata[, 6] <- globaldata[, 6] * 100
globaldata[, 8] <- globaldata[, 8] * 100

names(globaldata) <- c("Variety", "Baseline (Ha)", "Increase (Ha)", "Decrease (Ha)", "Loss (%)", "Chng. (%)", "Harvest (Ha)", "H. Loss (%)")

xtbl <- xtable(globaldata, digits=c(0, 0, 0, 0, 0, 1, 1, 0, 1))
print(xtbl, include.rownames=FALSE)
