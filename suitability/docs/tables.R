setwd("~/projects/coffee/tools/suitability")

variety <- "arabica"

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

names(table) <- c("Country", "Baseline (Ha)", "Increase (Ha)", "Decrease (Ha)", "Conf. (%)", "Loss (%)", "Chng. (%)", "Harvest (Ha)", "H. Loss (%)")

library(xtable)

Sys.setlocale('LC_ALL','C')
xtbl <- xtable(table)
print(xtbl, include.rownames=FALSE)
