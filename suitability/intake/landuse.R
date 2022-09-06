setwd("~/projects/coffee/tools")

library(ncdf4)
library(PBSmapping)
library(zoo)

## Get grid centers from harvest area
database <- nc_open("../database/harvestarea.nc4")
longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

## Extract urban locations
shape <- importShapefile("data/sources/ne_10m_urban_areas/ne_10m_urban_areas.shp")
proj.abbr <- attr(shape, "projection")

events <- expand.grid(X=longitude, Y=latitude)
events$EID <- 1:nrow(events)
events <- as.EventData(events, projection=proj.abbr)

urban <- c()
for (i0 in seq(1, nrow(events), by=10000)) {
    print(i0)
    some <- findPolys(events[i0:min(i0 + 9999, nrow(events)),], shape)
    urban <- c(urban, some$EID)
}

events$col = round(12 * (events$X - longitude[1]) + 1)
events$row = round(12 * (events$Y - latitude[1]) + 1)

write.csv(events[urban, c('X', 'Y', 'row', 'col')], "data/urban.csv", row.names=F)

## Extract protected locations
shape <- importShapefile("../extdata/WDPA_July2015-shapefile/WDPA_July2015-shapefile-polygons-tropics.shp") # produced polygons-tropics-simple, but can't load
proj.abbr <- attr(shape, "projection")
polydata <- attr(shape, "PolyData")

## Ingore any with status as Proposed or Not Reported
## don't actually drop, because messes up the PID <-> row correspondance
ignore <- polydata$STATUS %in% c("Proposed", "Not Reported")
strict <- polydata$IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III', 'IV')

events <- expand.grid(X=longitude, Y=latitude)
events$EID <- 1:nrow(events)
events <- as.EventData(events, projection=proj.abbr)

protected <- c()
managed <- c()
for (i0 in seq(1, nrow(events), by=10000)) {
    print(i0)
    some <- findPolys(events[i0:min(i0 + 9999, nrow(events)),], shape)
    some <- some[!ignore[some$PID],]
    protected <- c(protected, some$EID[strict[some$PID]])
    managed <- c(managed, some$EID[!strict[some$PID]])
}

events$col = round(12 * (events$X - longitude[1]) + 1)
events$row = round(12 * (events$Y - latitude[1]) + 1)

write.csv(events[protected, c('X', 'Y', 'row', 'col')], "data/protected.csv", row.names=F)
write.csv(events[managed, c('X', 'Y', 'row', 'col')], "data/managed.csv", row.names=F)
