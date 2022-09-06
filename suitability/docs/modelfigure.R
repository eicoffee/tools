setwd("~/research/coffee/tools/suitability")

library(ncdf4)
library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)

variety <- "arabica" #"robusta"
minval <- 50000

model <- nc_open(paste0("outputs/", variety, "-model.nc4"))
product <- ncvar_get(model, "product")
temp <- ncvar_get(model, "temp") / 10
precip <- ncvar_get(model, "precip") / 1000

tdups <- duplicated(temp)
if (tdups[100]) {
    tdups[rev(which(!tdups))[1]] <- T
    tdups[100] <- F
}

pdups <- duplicated(precip)
if (pdups[100]) {
    pdups[rev(which(!pdups))[1]] <- T
    pdups[100] <- F
}

interp.q2t <- approxfun((1:100)[!tdups], temp[!tdups])
interp.q2p <- approxfun((1:100)[!pdups], precip[!pdups])

rownames(product) <- interp.q2p(1:100)
colnames(product) <- interp.q2t(1:100)
df <- melt(product, varnames=c("precip", "temp"))

trans.q2t <- trans_new("temperature", approxfun(temp[!tdups], (1:100)[!tdups]),
                       interp.q2t, domain=range(temp))
trans.q2p <- trans_new("precipitation", approxfun(precip[!pdups], (1:100)[!pdups]),
                       interp.q2p, domain=range(precip))

dfmc <- read.csv(paste0("outputs/", variety, "-modelcountries.csv"))

dfseg <- data.frame(country=c(), temp=c(), precip=c(), temp1=c(), precip1=c())
for (country in unique(dfmc$country)) {
    subdfmc <- dfmc[dfmc$scenario != "present" & dfmc$country == country,]
    dfseg <- rbind(dfseg, data.frame(country, temp=dfmc$temp[dfmc$scenario == 'present' & dfmc$country == country] / 10, precip=dfmc$precip[dfmc$scenario == 'present' & dfmc$country == country] / 1000, temp1=mean(subdfmc$temp) / 10, precip1=mean(subdfmc$precip) / 1000))
}

dfcountry <- read.csv(paste0("outputs/", variety, "-countries.csv"))
dfseg <- dfseg %>% left_join(dfcountry)

ggplot(df, aes(precip, temp)) +
    geom_raster(aes(fill=value)) +
    geom_segment(data=subset(dfseg, baseline > minval), aes(xend=precip1, yend=temp1, size=baseline), arrow=arrow(length=unit(0.2, "cm"))) +
    scale_x_continuous(trans=trans.q2p, limits=range(precip), expand=c(0, 0), breaks=c(.1, .5, 1, 1.5, 2.5)) +
    scale_y_continuous(trans=trans.q2t, limits=range(temp), expand=c(0, 0)) +
    xlab("Precipitation (m)") + ylab("Temperature (C)") +
    scale_fill_continuous(name="Expected\n Area") + scale_size(name="Country Area", limits=range(dfseg$baseline), range=c(0, 1))
ggsave(paste0("outputs/", variety, "-modelfigure.pdf"), width=5, height=4)
