setwd("~/research/coffee/tools/suitability")

library(ncdf4)
library(raster)
library(Hmisc)
library(gurobi)
library(xtable)
library(RColorBrewer)
library(mapdata)
source("intake/lib.R") # now up in coffee/tools

variety <- "arabica" #"robusta"
factdown <- 18 #18

database <- nc_open("../database/harvestarea.nc4")
harvested <- ncvar_get(database, variety)
longitude <- ncvar_get(database, "lon")
latitude <- ncvar_get(database, "lat")

## Turn into probabilities
areas <- matrix(NA, nrow=4320, ncol=720)
for (ii in 1:720) {
    # 1/12 degrees
    londist <- 2*pi * 6371 * cos(latitude[ii] * pi / 180) / (360 * 12)
    latdist <- 2*pi * 6371 / (360 * 12)
    areas[, ii] <- londist * latdist * 100 # in Ha
}

harvested[is.na(harvested)] <- 0

predicted <- nc_open(paste0("suitability/outputs/", variety, "-product.nc4"))
predicted <- ncvar_get(predicted, "suitability")
## Turn into Ha
predicted <- predicted * areas

predicted[is.na(predicted)] <- 0

extrasuit <- matrix(pmax(0, harvested - predicted), dim(areas)[1], dim(areas)[2])

database <- nc_open(paste0("suitability/outputs/", variety, "-future.nc4"))
map <- ncvar_get(database, "suitability") * areas
map[is.na(map)] <- 0
futureextrasuit <- extrasuit * (1 + map * (map < 0) / predicted)
futureextrasuit[is.na(futureextrasuit)] <- extrasuit[predicted == 0]
future <- predicted + map + futureextrasuit

elev <- get.elev.map() # For oceans

## LINPROG APPROACH

redist <- matrix(pmax(0, harvested - future), dim(areas)[1], dim(areas)[2])
available <- future - (harvested - redist)

length(which(redist > 0)) * length(which(available > 1e-6)) # Too big!

sourcelat <- t(matrix(latitude, nrow=720, ncol=4320))
sourcelon <- matrix(longitude, nrow=4320, ncol=720)

## Reduce resolution
redist2 <- as.matrix(aggregate(raster(redist), fact=factdown, fun=sum))
available2 <- as.matrix(aggregate(raster(available), fact=factdown, fun=sum))

sourcelat2 <- as.matrix(aggregate(raster(t(matrix(latitude, nrow=720, ncol=4320))), fact=factdown))
sourcelon2 <- as.matrix(aggregate(raster(matrix(longitude, nrow=4320, ncol=720)), fact=factdown))

deg2rad <- function(deg) return(deg*pi/180)

gcd.slc <- function(long0, lat0, longs, lats) {
    long0 <- deg2rad(long0)
    lat0 <- deg2rad(lat0)
    longs <- deg2rad(longs)
    lats <- deg2rad(lats)

    R <- 6371 # Earth mean radius [km]
    acos(sin(lat0)*sin(lats) + cos(lat0)*cos(lats) * cos(longs-long0)) * R # Distance in km
}

regions <- list("Americas"=c(-180, -25), "Africa"=c(-25, 60), "South Asia/Oceania"=c(60, 180))

results <- data.frame(variety, distance=0, sddistance=0, diffelev=0, sddiffelev=0, diffpole=0, sddiffpole=0, planted=c(sum(extrasuit[predicted == 0]), sum(harvested) - sum(redist) - sum(extrasuit[predicted == 0])), note=c("unmodeled", "nomoving"))

allreiis <- c()
allaviis <- c()
allgresx <- c()

movement <- matrix(0, dim(redist2)[1], dim(redist2)[2]) # For drawing movement

for (region in names(regions)) {
    print(region)

    reiis <- which(redist2 > 0 & sourcelon2 >= regions[[region]][1] & sourcelon2 < regions[[region]][2])
    aviis <- which(available2 > 1e-6 & sourcelon2 >= regions[[region]][1] & sourcelon2 < regions[[region]][2])

    allreiis <- c(allreiis, reiis)
    allaviis <- c(allaviis, aviis)

    ff <- matrix(Inf, length(aviis), length(reiis))
    for (ii in 1:length(reiis))
        ff[, ii] <- gcd.slc(sourcelon2[reiis[ii]], sourcelat2[reiis[ii]], sourcelon2[aviis], sourcelat2[aviis]) + 1

    ff <- as.vector(ff)
    ff[is.nan(ff)] <- Inf

    AA <- matrix(0, length(aviis) + length(reiis), length(reiis) * length(aviis))
    bb <- rep(0, length(aviis) + length(reiis))

    ## Constraint: Available space not exceeded => Sum(redist to avii) < available[avii]
    for (ii in 1:length(aviis)) {
        AApart <- matrix(0, length(aviis), length(reiis))
        AApart[ii, ] <- 1
        AA[ii, ] <- as.vector(AApart)
        bb[ii] <- available2[aviis[ii]]
    }

    ## Constraint: All needed redistribution done => Sum(redist from reii) > redist[reii] -> -Sum(...) < -redist[reii]
    for (ii in 1:length(reiis)) {
        AApart <- matrix(0, length(aviis), length(reiis))
        AApart[, ii] <- -1
        AA[length(aviis) + ii, ] <- as.vector(AApart)
        bb[length(aviis) + ii] <- -redist2[reiis[ii]]
    }

    ub <- matrix(Inf, length(aviis), length(reiis))
    for (ii in 1:length(reiis))
        ub[, ii] <- redist2[reiis[ii]]

    ## TESTING
    ## model <- list(A=AA[1:2511,], obj=ff, modelsense="min", rhs=bb[1:2511], sense=rep('<=', 2511))
    ## gres <- gurobi(model, list(OutputFlag=0))
    ## sum(gres$x * AA[2512,])

    model <- list(A=AA, obj=ff, modelsense="min", rhs=bb, sense=rep('<=', length(bb)))
    gres <- gurobi(model, list(OutputFlag=0))

    allgresx <- c(allgresx, gres$x)

    distance <- gres$objval / sum(gres$x) #wtd.mean(ff[ff < Inf], gres$x[ff < Inf]))
    sddistance <- sqrt(wtd.var(ff[ff < Inf], gres$x[ff < Inf]))

    elev2 <- as.matrix(aggregate(raster(elev), fact=factdown, fun=mean))
    reelev2 <- as.vector(t(matrix(elev2[reiis], length(reiis), length(aviis))))
    avelev2 <- as.vector(matrix(elev2[aviis], length(aviis), length(reiis)))

    diffelev <- wtd.mean(avelev2 - reelev2, gres$x)
    sddiffelev <- sqrt(wtd.var(avelev2 - reelev2, gres$x))

    repole2 <- as.vector(t(matrix(abs(sourcelat[reiis]), length(reiis), length(aviis))))
    avpole2 <- as.vector(matrix(abs(sourcelat[aviis]), length(aviis), length(reiis)))

    diffpole <- wtd.mean(avpole2 - repole2, gres$x) * 2*pi * 6371 / 360
    sddiffpole <- sqrt(wtd.var((avpole2 - repole2) * 2*pi * 6371 / 360, gres$x))

    results <- rbind(results, data.frame(variety, distance, sddistance, diffelev, sddiffelev, diffpole, sddiffpole, planted=sum(gres$x), note=region))

    ## Draw this movement
    movement[reiis] <- -redist2[reiis]
    moveto <- matrix(gres$x, length(aviis), length(reiis))
    movement[aviis] <- movement[aviis] + rowSums(moveto)
}

print(results$variety[1])
disptbl <- data.frame(Variety="", Distance=paste(format(results$distance, nsmall=1, digits=3), "$\\pm$", format(results$sddistance, nsmall=1, digits=3)),
                      Elevation=paste(format(results$diffelev, nsmall=1, digits=3), "$\\pm$", format(results$sddiffelev, nsmall=1, digits=3)),
                      Poleward=paste(format(results$diffpole, nsmall=1, digits=3), "$\\pm$", format(results$sddiffpole, nsmall=1, digits=3)),
                      Planted=results$planted / 1000, Notes=results$note)

print(xtable(disptbl, digits=0), include.rownames=F, sanitize.text.function=identity)

if (factdown == 12) {
    longitude2 <- seq(-179.5, 179.5, by=1)
    latitude2 <- seq(-29.5, 29.5, by=1)
} else if (factdown == 18) {
    longitude2 <- seq(-179.25, 179.25, by=1.5)
    latitude2 <- seq(-29.25, 29.25, by=1.5)
}

mask <- movement
mask[elev2 != 0] <- NA
mask[elev2 == 0] <- 1
mask[is.na(elev2)] <- 1

movement2 <- movement
movement2[movement2 < -range(movement, na.rm=T)[2]] <- -range(movement, na.rm=T)[2]

png(paste0("suitability/outputs/", variety, "-movement.png"), width=4320, height=720)
par(mar=rep(0, 4))
plot(0, 0, ylim=c(-30, 30), xlim=c(-180, 180), xaxs = "i", yaxs = "i")
image(longitude2, latitude2, movement2, col=brewer.pal(11,"BrBG"), add=T)
image(longitude2, latitude2, matrix(mask, dim(elev2)[1], dim(elev2)[2]), col=c("#FDFDFD"), add=T)
map("worldHires", ylim=c(-30, 30), xlim=c(-180, 180), col="#000000A0", add=T)
dev.off()


## SPREAD APPROACH

spread <- function(redist) {
    ## Convolve with spreader, bouncing off of oceans
    bounce <- matrix(0, dim(areas)[1], dim(areas)[2])

    moveup <- .25 * rbind(redist[-1,], rep(0, dim(redist)[2]))
    moveup[1, ] <- moveup[1, ] + .25 * redist[1, ]
    bounce[-1,] <- bounce[-1,] + moveup[-dim(areas)[1],] * is.na(elev[-dim(areas)[1],])
    moveup <- moveup * !is.na(elev)

    movedown <- .25 * rbind(rep(0, dim(redist)[2]), redist[-dim(redist)[1],])
    movedown[dim(redist)[1], ] <- movedown[dim(redist)[1], ] + .25 * redist[dim(redist)[1], ]
    bounce[-dim(areas)[1],] <- bounce[-dim(areas)[1],] + movedown[-1,] * is.na(elev[-1,])
    movedown <- movedown * !is.na(elev)

    moveleft <- .25 * cbind(redist[, -1], rep(0, dim(redist)[1]))
    moveleft[, 1] <- moveleft[, 1] + .25 * redist[, 1]
    bounce[, -1] <- bounce[, -1] + moveleft[, -dim(areas)[2]] * is.na(elev[, -dim(areas)[2]])
    moveleft <- moveleft * !is.na(elev)

    moveright <- .25 * cbind(rep(0, dim(redist)[1]), redist[, -dim(redist)[2]])
    moveright[, dim(redist)[2]] <- moveright[, dim(redist)[2]] + .25 * redist[, dim(redist)[2]]
    bounce[, -dim(areas)[2]] <- bounce[, -dim(areas)[2]] + moveright[, -1] * is.na(elev[, -1])
    moveright <- moveright * !is.na(elev)

    list(moveup, movedown, moveleft, moveright, bounce, redist)
}

spread.sum <- function(spreaded) {
    spreaded[[1]] + spreaded[[2]] + spreaded[[3]] + spreaded[[4]] + spreaded[[5]]
}

spread.weighted.sum <- function(spreaded, weighted) {
    ## When entries from different directions converge on a pixel,
    ## want their full value (4 x, except edges then 2 x) to be
    ## averaged by weights.
    summed <- (4 * weighted[[1]] * spreaded[[1]] + 4 * weighted[[2]] * spreaded[[2]] + 4 * weighted[[3]] * spreaded[[3]] + 4 * weighted[[4]] * spreaded[[4]] + weighted[[5]] * spreaded[[5]]) / (weighted[[1]] + weighted[[2]] + weighted[[3]] + weighted[[4]] + weighted[[5]])
    ## Fix weights for edges
    summed[1, ] <- (2 * weighted[[1]][1,] * spreaded[[1]][1,] + 4 * weighted[[3]][1,] * spreaded[[3]][1,] + 4 * weighted[[4]][1,] * spreaded[[4]][1,] + weighted[[5]][1,] * spreaded[[5]][1,]) / (weighted[[1]][1,] + weighted[[3]][1,] + weighted[[4]][1,] + weighted[[5]][1,])
    summed[dim(areas)[1], ] <- (2 * weighted[[2]][dim(areas)[1], ] * spreaded[[2]][dim(areas)[1],] + 4 * weighted[[3]][dim(areas)[1], ] * spreaded[[3]][dim(areas)[1],] + 4 * weighted[[4]][dim(areas)[1], ] * spreaded[[4]][dim(areas)[1],] + weighted[[5]][dim(areas)[1], ] * spreaded[[5]][dim(areas)[1],]) / (weighted[[2]][dim(areas)[1], ] + weighted[[3]][dim(areas)[1], ] + weighted[[4]][dim(areas)[1], ] + weighted[[5]][dim(areas)[1], ])
    summed[, 1] <- (4 * weighted[[1]][,1] * spreaded[[1]][,1] + 4 * weighted[[2]][,1] * spreaded[[2]][,1] + 2 * weighted[[3]][,1] * spreaded[[3]][, 1] + weighted[[5]][,1] * spreaded[[5]][,1]) / (weighted[[1]][,1] + weighted[[2]][,1] + weighted[[3]][,1] + weighted[[5]][,1])
    summed[, dim(areas)[2]] <- (4 * weighted[[1]][,dim(areas)[2]] * spreaded[[1]][,dim(areas)[2]] + 4 * weighted[[2]][,dim(areas)[2]] * spreaded[[2]][,dim(areas)[2]] + 2 * weighted[[4]][,dim(areas)[2]] * spreaded[[4]][, dim(areas)[2]] + weighted[[5]][,dim(areas)[2]] * spreaded[[5]][, dim(areas)[2]]) / (weighted[[1]][,dim(areas)[2]] + weighted[[2]][,dim(areas)[2]] + weighted[[4]][,dim(areas)[2]] + weighted[[5]][,dim(areas)[2]])
    summed[!is.finite(summed)] <- spreaded[[6]][!is.finite(summed)] # Don't change these
    summed
}

redist <- matrix(pmax(0, harvested - future), dim(areas)[1], dim(areas)[2])

results <- data.frame(distance=0, diffelev=0, sddiffelev=0, crowdist=0, sdcrowdist=0, diffpole=0, sddiffpole=0, planted=c(sum(extrasuit[predicted == 0]), sum(harvested) - sum(redist) - sum(extrasuit[predicted == 0])), note=c("unmodeled", "nomoving"))

available <- future - (harvested - redist)

sourceelev <- elev
sourceelev[is.na(sourceelev)] <- 0

sourcelat <- t(matrix(latitude, nrow=720, ncol=4320))
sourcelon <- matrix(longitude, nrow=4320, ncol=720)
sourcepole <- t(matrix(abs(latitude), nrow=720, ncol=4320))

for (distance in 1:5000) {
    weights <- spread(redist)
    lookhere <- spread.sum(weights)
    spreadelev <- spread.weighted.sum(spread(sourceelev), weights)
    spreadlat <- spread.weighted.sum(spread(sourcelat), weights)
    spreadlon <- spread.weighted.sum(spread(sourcelon), weights)
    spreadpole <- spread.weighted.sum(spread(sourcepole), weights)

    redist <- matrix(pmax(0, lookhere - available), dim(areas)[1], dim(areas)[2])
    planted <- lookhere - redist
    planted[planted < 0] <- 0 # numerical error

    diffelev <- wtd.mean(elev - spreadelev, planted)
    crowdist <- wtd.mean(sqrt((sourcelat - spreadlat)^2 + (sourcelon - spreadlon)^2), planted)
    diffpole <- wtd.mean(sourcepole - spreadpole, planted)

    sddiffelev <- sqrt(wtd.var(elev - spreadelev, planted))
    sdcrowdist <- sqrt(wtd.var(sqrt((sourcelat - spreadlat)^2 + (sourcelon - spreadlon)^2), planted))
    sddiffpole <- sqrt(wtd.var(sourcepole - spreadpole, planted))

    results <- rbind(results, data.frame(distance, diffelev, sddiffelev, crowdist, sdcrowdist, diffpole, sddiffpole, planted=sum(planted), note="shifted"))

    sourceelev <- spreadelev
    sourcelat <- spreadlat

    available <- available - planted

    print(c("Remaining:", sum(redist)))
    print(tail(results))
}

write.csv(results, "movement.csv", row.names=F)
