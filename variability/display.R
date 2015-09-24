setwd("~/projects/coffee/tools/variability")

library(maps)
source("~/projects/research-common/R/drawmap.R")

yields <- read.csv("yields2.csv") # yields.csv
yields$region <- as.character(yields$region)
yields$region[yields$region == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
yields$region[yields$region == "Viet Nam"] <- "Vietnam"
yields$region[yields$region == "Côte d'Ivoire"] <- "Cote d'Ivoire"

yields$percent <- NA

percent1997 = list("Mexico"=-6.96, "Guatemala"=-22.63, "El Salvador"=5.65, "Costa Rica"=21.24, "Honduras"=14.77, "Nicaragua"=40.61, "Brazil"=-31.63, "Colombia"=-3.43, "Ecuador"=-12.09, "Peru"=4.88, "Venezuela"=-20.35, "Indonesia"=-17.21, "Papua New Guinea"=-18.46, "Vietnam"=2.24, "India"=9.54, "Thailand"=-11.84, "Philippines"=5.06, "Cote d'Ivoire"=3.13, "Cameroon"=-1.05, "Uganda"=-6.04, "Kenya"=-11.72, "Ethiopia"=17.25, "Madagascar"=-5.88, "Angola"=83.10)
for (name in names(percent1997)) {
    if (!(name %in% yields$region)) {
        print(paste("Cannot find", name))
        next
    }

    yields$percent[yields$region == name] <- percent1997[[name]]
}

source("~/projects/coffee/lib/production.R")
faostat <- load.faostat()

yields$yield1996 <- NA

for (country in unique(faostat$Country)) {
    mycountry <- country
    if (country == "Côte d'Ivoire")
        mycountry <- "Cote d'Ivoire"
    if (country == "Venezuela (Bolivarian Republic of)")
        mycountry <- "Venezuela"
    if (country == "Viet Nam")
        mycountry <- "Vietnam"

    if (!(mycountry %in% yields$region)) {
        print(paste("Cannot find", country))
        next
    }

    valid <- faostat$Country == country & faostat$Year == 1996
    if (sum(valid) == 0)
        next

    yields$yield1996[yields$region == mycountry] <- faostat$CorrectedYield[valid]
}

yields$diff.before <- yields$yield1996 * (1 + yields$percent / 100) - yields$yield1996
yields$diff.pred <- yields$yield2016 - yields$yield2015
yields$region[yields$region == "Bolivia (Plurinational State of)"] <- "Bolivia"
yields$region[yields$region == "Cote d'Ivoire"] <- "Ivory Coast"

## Use all values for colors
values <- c(yields$diff.before, yields$diff.pred)

## Map Before
colors <- brewer.pal(9, colorscheme)
brks <- classIntervals(values, n=9, style="quantile")
brks <- brks$brks

par(mfrow=c(2, 1))

map("world", ylim=c(-30, 30), mar=c(1, 2, 1, 0))
for (ii in 1:nrow(yields)) {
    if (is.na(yields$diff.before[ii]))
        next
    map("world", yields$region[ii], col=colors[findInterval(yields$diff.before[ii], brks, all.inside=TRUE)], fill=T, add=T)
}
legend("bottomleft", legend=leglabs(round(brks, digits=2)), fill=colors, bty="n", cex=.5)

map("world", ylim=c(-30, 30), mar=c(1, 2, 1, 0))
for (ii in 1:nrow(yields)) {
    if (is.na(yields$diff.pred[ii]))
        next
    tryCatch({
        map("world", yields$region[ii], col=colors[findInterval(yields$diff.pred[ii], brks, all.inside=TRUE)], fill=T, add=T)
    }, error=function(cond) {})
}
legend("bottomleft", legend=leglabs(round(brks, digits=2)), fill=colors, bty="n", cex=.5)
