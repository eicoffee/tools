setwd("~/projects/coffee/tools/production")

library(maps)
source("~/projects/research-common/R/drawmap.R")

changes <- read.csv("changes.csv")

## Whereever countries show up twice, make combined result
for (country in changes$country[duplicated(changes$country)]) {
    rows <- changes[changes$country == country,]
    changes <- changes[changes$country != country,]
    if ("combined" %in% rows$variety)
        changes <- rbind(changes, rows[rows$variety == "combined",])
    else
        changes <- rbind(changes, data.frame(country, variety="combined", change=mean(rows$change)))
}

map("world", ylim=c(-30, 30))
draw.map("world", changes$country, changes$change, position="bottomleft", add=T, digits=2)

source("~/projects/research-common/tropict/draw_map.R", chdir=T)

colval <- rep(NA, nrow(polydata))
colshade <- rep(NA, nrow(polydata))
for (ii in 1:nrow(changes)) {
    country <- as.character(changes$country[ii])
    if (country %in% polydata$name) {
        colval[country == polydata$name] <- changes$change[ii]
        colshade[country == polydata$name] <- changes$variety[ii]
    } else
        print(changes$country[ii])
}

col <- val2colors(colval)
col[is.na(col)] <- "#00000000"

png("yieldchangeb.png", 1850, 900)
plotMap("#000000", col)
addSeams("#00000080")
val2legend("bottomleft", colval, digits=2, cex=2.5)
dev.off()
