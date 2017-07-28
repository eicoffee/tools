## Duplicate of research-common/R/tropict/draw_map.R

oldwd <- getwd()
setwd("~/research/coffee/mapper")

library(PBSmapping)

do.versions <- F

shape <- importShapefile("../tools/data/ne_50m_admin_0_countries/ne_50m_admin_0_countries")
polydata <- attributes(shape)$PolyData

plotPolys(shape, ylim=c(-30, 30))

## New world
#plotPolys(shape, ylim=c(-30, 30), xlim=c(-120, -30))
newworld <- unique(shape$PID[shape$X <= -30 & shape$X >= -120 & shape$Y >= -30 & shape$Y <= 30])

## Old world
#plotPolys(shape, ylim=c(-30, 30), xlim=c(-30, 180))
oldworld <- unique(shape$PID[shape$X > -30 & shape$Y >= -30 & shape$Y <= 30])

polydata$name[newworld[newworld %in% oldworld]]

## France is a problem
francepid <- 73
frances <- calcCentroid(subset(shape, PID == francepid))
newworld <- newworld[newworld != francepid]
oldworld <- oldworld[oldworld != francepid]
newfrance <- frances$SID[frances$X <= -30]
oldfrance <- frances$SID[frances$X > -30]

## Hawaii is too far away
usapid <- which(polydata$name == "United States")
usas <- calcCentroid(subset(shape, PID == usapid))
newworld <- newworld[newworld != usapid]
hawaii <- usas$SID[usas$X < -140 & usas$Y < 25]

## Far east
centroids = calcCentroid(shape[shape$X > 140 & shape$Y >= -30 & shape$Y <= 30,], rollup=1)
#plotPolys(shape, ylim=c(-30, 30), xlim=c(140, 180))
#text(centroids$X, centroids$Y, polydata$name[centroids$PID])

fareast <- unique(shape$PID[shape$X > 140 & shape$Y >= -30 & shape$Y <= 30])
fareast <- fareast[!(fareast %in% which(polydata$name %in% c("Australia", "Indonesia", "Japan")))]

oldworld <- oldworld[!(oldworld %in% fareast)]

fareast <- fareast[!(fareast %in% which(polydata$name %in% c("Marshall Is.", "Kiribati", "Micronesia")))] # Produce nothing

## Australia
australia <- which(polydata$name == "Australia")
oldworld <- oldworld[oldworld != australia]

## Create shifted polygons
newshape <- subset(shape, PID %in% newworld | (PID == francepid & SID %in% newfrance) | (PID == usapid & !(SID %in% hawaii)))
oldshape <- subset(shape, PID %in% oldworld | (PID == francepid & SID %in% oldfrance))
farshape <- subset(shape, PID %in% fareast)
ausshape <- subset(shape, PID == australia)
hawshape <- subset(shape, PID == usapid & SID %in% hawaii)

## New world into Africa
shiftright <- newshape
shiftright$X <- shiftright$X + 29

#plotPolys(rbind(oldshape, shiftright), ylim=c(-30, 30))
#addPolys(shiftright, border=2, col=2)

## Far east into South America
shiftleft <- farshape
shiftleft$X[shiftleft$X < 0] <- shiftleft$X[shiftleft$X < 0] + 360 # Fix Fiji
shiftleft$X <- shiftleft$X - 260 + 30

#plotPolys(rbind(shiftright, shiftleft), ylim=c(-30, 30))
#addPolys(shiftleft, border=2, col=2)

## Australia into Indian Ocean
nudged <- ausshape
nudged$X <- nudged$X - 50

#plotPolys(rbind(oldshape, nudged), ylim=c(-30, 30))
#addPolys(nudged, border=2, col=2)

## Hawaii into Australia's spot
dropped <- hawshape
dropped$X <- dropped$X + 280
dropped$Y <- dropped$Y - 40

#plotPolys(rbind(oldshape, nudged, dropped), ylim=c(-30, 30))
#addPolys(dropped, border=2, col=2)

setwd(oldwd)

if (do.versions) {
    pdf("original.pdf", width=3 * 360 / 60, height=3)
    par(mar=rep(0, 4))
    plotPolys(shape, ylim=c(-30, 30), col=6)
    dev.off()

    pdf("version1.pdf", width=6, height=2)
    par(mar=rep(0, 4))
    plot.new()
    plot.window(ylim=c(-30, 30), xlim=c(-90, 142), asp=1)
    addPolys(oldshape, col=6, border="#00000080")
    addPolys(shiftright, col=2, border="#00000080")
    addPolys(shiftleft, col=3, border="#00000080")
    addPolys(nudged, col=4, border="#00000080")
    addPolys(dropped, col=5, border="#00000080")
    dev.off()

    ## Split in two
    plotPolys(shape, ylim=c(-30, 30), xlim=c(-88, 52), mar=rep(0, 4))

    pdf("version2a.pdf", width=3 * 140 / 60, height=3)
    par(mar=rep(0, 4))
    plotPolys(oldshape, ylim=c(-30, 30), xlim=c(-88, 52), col=6)
    ##plot.new()
    ##plot.window(ylim=c(-30, 30), xlim=c(-88, 52), asp=1, mar=rep(0, 4))
    ##addPolys(oldshape, col=6)
    addPolys(shiftright, col=2)
    dev.off()

    plotPolys(shape, ylim=c(-30, 30), xlim=c(52, 180))

    pdf("version2b.pdf", width=3 * 130 / 60, height=3)
    par(mar=rep(0, 4))
    plotPolys(oldshape, ylim=c(-30, 30), xlim=c(52, 182), col=6)
    ##plot.new()
    ##plot.window(ylim=c(-30, 30), xlim=c(52, 180), asp=1, mar=rep(0, 4))
    addPolys(oldshape, col=6)
    addPolys(ausshape, col=6)

    shiftleft <- farshape
    shiftleft$X[shiftleft$X < 0] <- shiftleft$X[shiftleft$X < 0] + 360 # Fix Fiji

    addPolys(shiftleft, col=6)

    dropped <- hawshape
    dropped$X <- dropped$X + 320

    addPolys(dropped, col=5)
    dev.off()

    ## Version 3: Divide Australia, and just shift Hawaii

    pdf("version3.pdf", width=3 * (141.2 + 89.3) / 60, height=3)
    par(mar=rep(0, 4))
    plotPolys(oldshape, ylim=c(-30, 30), xlim=c(-89.3, 141.2), col=6)
    addPolys(ausshape, col=6)

    addPolys(shiftright, col=2)
    addPolys(shiftleft, col=3, border="#00000080")

    nudged <- ausshape
    nudged$X <- nudged$X - 260 + 30
    addPolys(nudged, col=3, border="#00000080")

    dropped <- hawshape
    dropped$X <- dropped$X + 293
    addPolys(dropped, col=5, border="#00000080")
    dev.off()
}

## Version 4: Split on Pakistan, use Gall-Peters
## For aspect ratio, treat x = radian lon, so width = 260 * pi / 180

##pdf("version4.pdf", width=3 * (265 * pi / 180) / 2, height=3)
## ...
##dev.off()

plotMap <- function(border) {
    par(mar=rep(0, 4))

    oldshape.gp <- oldshape
    oldshape.gp$X <- oldshape.gp$X - 1.5
    oldshape.gp$X <- oldshape.gp$X * pi / 180
    oldshape.gp$Y <- 2 * sin(oldshape.gp$Y * pi / 180)
    oldshape.gp$Y[oldshape.gp$Y > 0] <- oldshape.gp$Y[oldshape.gp$Y > 0] - .05 * sin(oldshape.gp$Y[oldshape.gp$Y > 0] * pi)
    oldshape.gp$Y[oldshape.gp$Y > -.25 & oldshape.gp$Y < .25] <- oldshape.gp$Y[oldshape.gp$Y > -.25 & oldshape.gp$Y < .25] - .015 * sin((oldshape.gp$Y[oldshape.gp$Y > -.25 & oldshape.gp$Y < .25] + .25) * 2 * pi)
    plotPolys(oldshape.gp, ylim=c(-1, 1), xlim=c(-165, 65), border=border)

    addMap2(border)
}

addMap <- function(border) {
    oldshape.gp <- oldshape
    oldshape.gp$X <- oldshape.gp$X - 1.5
    oldshape.gp$X <- oldshape.gp$X * pi / 180
    oldshape.gp$Y <- 2 * sin(oldshape.gp$Y * pi / 180)
    oldshape.gp$Y[oldshape.gp$Y > 0] <- oldshape.gp$Y[oldshape.gp$Y > 0] - .05 * sin(oldshape.gp$Y[oldshape.gp$Y > 0] * pi)
    oldshape.gp$Y[oldshape.gp$Y > -.25 & oldshape.gp$Y < .25] <- oldshape.gp$Y[oldshape.gp$Y > -.25 & oldshape.gp$Y < .25] - .015 * sin((oldshape.gp$Y[oldshape.gp$Y > -.25 & oldshape.gp$Y < .25] + .25) * 2 * pi)
    addPolys(oldshape.gp, border=border, col="#00000000")

    addMap2(border)
}

addMap2 <- function(border) {
    oldshape2.gp <- oldshape
    oldshape2.gp$X <- oldshape2.gp$X - 265 + 30 + 1
    ##oldshape2.gp$X <- oldshape2.gp$X + .01 * (oldshape2.gp$X - -170)
    oldshape2.gp$X <- oldshape2.gp$X * pi / 180
    oldshape2.gp$Y <- 2 * sin(oldshape2.gp$Y * pi / 180)
    oldshape2.gp$Y[oldshape2.gp$Y > 0] <- oldshape2.gp$Y[oldshape2.gp$Y > 0] - .05 * sin(oldshape2.gp$Y[oldshape2.gp$Y > 0] * pi)
    oldshape2.gp$Y[oldshape2.gp$Y > -.25 & oldshape2.gp$Y < .25] <- oldshape2.gp$Y[oldshape2.gp$Y > -.25 & oldshape2.gp$Y < .25] - .015 * sin((oldshape2.gp$Y[oldshape2.gp$Y > -.25 & oldshape2.gp$Y < .25] + .25) * 2 * pi)
    addPolys(oldshape2.gp, border=border, col="#00000000")

    nudged.gp <- ausshape
    nudged.gp$X <- nudged.gp$X - 265 + 30 + 1
    nudged.gp$X <- nudged.gp$X * pi / 180
    nudged.gp$Y <- 2 * sin(nudged.gp$Y * pi / 180)
    nudged.gp$Y[nudged.gp$Y > 0] <- nudged.gp$Y[nudged.gp$Y > 0] - .05 * sin(nudged.gp$Y[nudged.gp$Y > 0] * pi)
    nudged.gp$Y[nudged.gp$Y > -.25 & nudged.gp$Y < .25] <- nudged.gp$Y[nudged.gp$Y > -.25 & nudged.gp$Y < .25] - .015 * sin((nudged.gp$Y[nudged.gp$Y > -.25 & nudged.gp$Y < .25] + .25) * 2 * pi)
    addPolys(nudged.gp, border=border, col="#00000000")

    shiftright.gp <- shiftright
    shiftright.gp$X <- shiftright.gp$X + .15
    shiftright.gp$X <- shiftright.gp$X * pi / 180
    shiftright.gp$Y <- 2 * sin(shiftright.gp$Y * pi / 180)
    shiftright.gp$Y[shiftright.gp$Y > 0] <- shiftright.gp$Y[shiftright.gp$Y > 0] - .05 * sin(shiftright.gp$Y[shiftright.gp$Y > 0] * pi)
    shiftright.gp$Y[shiftright.gp$Y > -.25 & shiftright.gp$Y < .25] <- shiftright.gp$Y[shiftright.gp$Y > -.25 & shiftright.gp$Y < .25] - .015 * sin((shiftright.gp$Y[shiftright.gp$Y > -.25 & shiftright.gp$Y < .25] + .25) * 2 * pi)
    addPolys(shiftright.gp, border=border, col="#00000000")

    shiftleft.gp <- shiftleft
    shiftleft.gp$X <- shiftleft.gp$X - 5 + 1
    shiftleft.gp$X <- shiftleft.gp$X * pi / 180
    shiftleft.gp$Y <- 2 * sin(shiftleft.gp$Y * pi / 180)
    shiftleft.gp$Y[shiftleft.gp$Y > 0] <- shiftleft.gp$Y[shiftleft.gp$Y > 0] - .05 * sin(shiftleft.gp$Y[shiftleft.gp$Y > 0] * pi)
    shiftleft.gp$Y[shiftleft.gp$Y > -.25 & shiftleft.gp$Y < .25] <- shiftleft.gp$Y[shiftleft.gp$Y > -.25 & shiftleft.gp$Y < .25] - .015 * sin((shiftleft.gp$Y[shiftleft.gp$Y > -.25 & shiftleft.gp$Y < .25] + .25) * 2 * pi)
    addPolys(shiftleft.gp, border=border, col="#00000000")

    dropped.gp <- hawshape
    dropped.gp$X <- dropped.gp$X + 58.1
    dropped.gp$X <- dropped.gp$X * pi / 180
    dropped.gp$Y <- dropped.gp$Y + .7
    dropped.gp$Y <- 2 * sin((dropped.gp$Y - 2) * pi / 180)
    addPolys(dropped.gp, border=border, col="#00000000")
}

addSeams <- function(col) {
    ## Left old to New
    lines(c(-1.65, -.8, -.8), c(1, -.75, -1), col=col)
    ## New to right old
    lines(c(-.5, -.5, 0, 0), c(1, .4, -.25, -1), col=col)
    ## Hawaii
    lines(c(-1.625, -1.825, -1.825, -1.625, -1.625), c(.55, .55, .75, .75, .55), col=col)
}

mapperImage <- function(array, colors, breaks=NULL, add=F) {
    if (is.null(breaks)) {
        image(seq(-169.5, 61.5, length.out=dim(map)[1]) * pi / 180, seq(-1.035, 1.0, length.out=dim(map)[2]),
              array, col=colors, asp=1, ylim=c(-1, 1), xlim=c(-2.95, 1.05), add=add, xaxt="n", yaxt="n", xlab="", ylab="")
    } else {
        image(seq(-169.5, 61.5, length.out=dim(map)[1]) * pi / 180, seq(-1.035, 1.0, length.out=dim(map)[2]),
              array, col=colors, breaks=breaks, asp=1, ylim=c(-1, 1), xlim=c(-2.95, 1.05), add=add, xaxt="n", yaxt="n", xlab="", ylab="")
    }
}
