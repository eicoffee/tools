setwd("~/projects/coffee/tools/production")

library(maps)
source("~/projects/research-common/R/drawmap.R")

changes <- read.csv("changes.csv")

map("world", ylim=c(-30, 30))
draw.map("world", changes$country, changes$change, position="bottomleft", add=T, digits=2)
