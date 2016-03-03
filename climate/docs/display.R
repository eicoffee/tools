setwd("~/projects/coffee/tools/climate")

library(ggplot2)
library(grid)
library(xtable)

filename <- "colombia.csv" #"tropics.csv"
add.baseline <- F # T (for tropics)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

data <- read.csv(filename)

seq(0, 1, length.out=17)[c(5, 13)]
display <- data.frame(biovar=data$biovar, baseline=data$baseline, median=data$median, p25=NA, p75=NA)
display[1, c('p25', 'p75')] <- sort(data[1, 4:20])[c(5, 13)]
display[2, c('p25', 'p75')] <- sort(data[2, 4:20])[c(5, 13)]
display$median[3] <- 100 * data$median[3] / data$baseline[3]
display[3, c('p25', 'p75')] <- 100 * sort(data[3, 4:20])[c(5, 13)] / data$baseline[3]
display[4, c('p25', 'p75')] <- sort(data[4, 4:20])[c(5, 13)]
display[5, c('p25', 'p75')] <- sort(data[5, 4:20])[c(5, 13)]
display$median[6] <- 100 * data$median[6] / data$baseline[6]
display[6, c('p25', 'p75')] <- 100 * sort(data[6, 4:20])[c(5, 13)] / data$baseline[6]
display$median[7] <- 100 * data$median[7] / data$baseline[7]
display[7, c('p25', 'p75')] <- 100 * sort(data[7, 4:20])[c(5, 13)] / data$baseline[7]
display$median[8] <- 100 * data$median[8] / data$baseline[8]
display[8, c('p25', 'p75')] <- 100 * sort(data[8, 4:20])[c(5, 13)] / data$baseline[8]

names(display) <- c("Quantity", "Baseline", "Change", "25 pct.", "75 pct.")
print(xtable(display, digits=1), include.rownames=F)

if (add.baseline) {
    for (ii in 1:nrow(data))
        data[ii, 3:20] <- data[ii, 3:20] + data$baseline[ii]
}

p1 <- ggplot(data.frame(x=as.numeric(data[1, 4:20]) + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[1]) +
xlab("Annual Mean Temperature (°C)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

p2 <- ggplot(data.frame(x=as.numeric(data[2, 4:20]) + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[2]) +
xlab("Mean Diurnal Range (°C)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

## divide by 100
p3 <- ggplot(data.frame(x=as.numeric(data[3, 4:20]) / 100 + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[3] / 100) +
xlab("Temperature Seasonality (%)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

p4 <- ggplot(data.frame(x=as.numeric(data[4, 4:20]) + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[4]) +
xlab("Max Temperature of Warmest Month (°C)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

p5 <- ggplot(data.frame(x=as.numeric(data[5, 4:20]) + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[5]) +
xlab("Min Temperature of Coldest Month (°C)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

p6 <- ggplot(data.frame(x=as.numeric(data[6, 4:20]) + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[6]) +
xlab("Annual Precipitation (mm)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

p7 <- ggplot(data.frame(x=as.numeric(data[7, 4:20]) + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[7]) +
xlab("Precipitation of Wettest Month (mm)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

p8 <- ggplot(data.frame(x=as.numeric(data[8, 4:20]) + .05 * rnorm(17)), aes(x=x)) +
    geom_density(alpha=.2, fill="#FF6666") + geom_rug() + geom_vline(colour='red', xintercept=data$baseline[8]) +
xlab("Precipitation of Driest Month (mm)") + ylab("density") + theme(plot.margin=unit(c(.5,.5,-.1,.5), "cm"))

multiplot(p1, p4, p5, p2, p6, p7, p8, p3, cols=2)
