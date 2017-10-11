setwd("~/research/coffee/tools/suitability")

library(R.utils)

variety <- "arabica" #"robusta"

df = read.csv(paste0("outputs/", variety, "-variance.csv"))

df$title <- c("Total", "Elevation", "Top Soil Sand", "Top Soil Silt", "Top Soil Clay", "Top Soil Carbon", "Top Soil CaCO3", "Top Soil Gypsum", "Bottom Soil Sand", "Bottom Soil Silt", "Bottom Soil Clay", "Bottom Soil Carbon", "Bottom Soil CaCO3", "Bottom Soil Gypsum", "Mean Temp.", "Diurnal Range", "Isothermality", "Temp. Seasonality", "Max Temp.", "Min Temp.", "Wettest Temp.", "Driest Temp.", "Warmest Temp.", "Coldest Temp.", "Precipitation", "Max Precip.", "Min Precip.", "Precip. Seasonality", "Wettest Precip.", "Driest Precip.", "Warmest Precip.", "Coldest Precip.", "Latitude", "Transport Time", "Constraints")

library(ggplot2)

df$title <- factor(df$title, levels=c("Mean Temp.", "Diurnal Range", "Isothermality", "Temp. Seasonality", "Max Temp.", "Min Temp.", "Wettest Temp.", "Driest Temp.", "Warmest Temp.", "Coldest Temp.", "Precipitation", "Max Precip.", "Min Precip.", "Precip. Seasonality", "Wettest Precip.", "Driest Precip.", "Warmest Precip.", "Coldest Precip.", "Top Soil Sand", "Top Soil Silt", "Top Soil Clay", "Top Soil Carbon", "Top Soil CaCO3", "Top Soil Gypsum", "Bottom Soil Sand", "Bottom Soil Silt", "Bottom Soil Clay", "Bottom Soil Carbon", "Bottom Soil CaCO3", "Bottom Soil Gypsum", "Elevation", "Latitude", "Transport Time", "Constraints"))

ggplot(df[-1,], aes(title, variance)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size=10), legend.position="top", plot.margin = unit(c(0,1,0,2), "cm"), legend.box = "horizontal") +
    scale_y_sqrt(expand=c(0, 0)) + xlab(NULL) + ylab("Contribution to Variance") +
    ggtitle(paste("Contributions to suitability variance for ", capitalize(variety))) +
    theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0("outputs/", variety, "-variance.pdf"), width=9, height=5)
