setwd("~/research/coffee/tools/suitability/docs")

used <- c()
names <- c()
tbl <- matrix(NA, 0, 0)

add.corr <- function(filename, get.name1, get.name2) {
    if (filename %in% used)
        error("Already used.")
    used <<- c(used, filename)

    df <- read.csv(filename)

    for (ii in 1:nrow(df)) {
        name1 <- get.name1(df[ii,])
        name2 <- get.name2(df[ii,])
        if (name1 == name2) {
            if (df$corr[ii] == 1)
                next
            error("Duplicate name.")
        }

        if (!(name1 %in% names)) {
            names <<- c(names, name1)
            tbl <<- rbind(cbind(tbl, rep(NA, length(names)-1)), rep(NA, length(names)))
            tbl[length(names), length(names)] <<- 1
        }
        if (!(name2 %in% names)) {
            names <<- c(names, name2)
            tbl <<- rbind(cbind(tbl, rep(NA, length(names)-1)), rep(NA, length(names)))
            tbl[length(names), length(names)] <<- 1
        }
        if (which(names == name1) < which(names == name2))
            tbl[names == name1, names == name2] <<- df$corr[ii]
        else
            tbl[names == name2, names == name1] <<- df$corr[ii]
    }
}

add.corr("../../data/climcorr.csv",
         function(row) paste("BioClim", row$bioclim1),
         function(row) paste("BioClim", row$bioclim2))
add.corr("../../data/climsoilcorr.csv",
         function(row) paste("BioClim", row$bioclim),
         function(row) paste(ifelse(row$soiltype == "topsoil", "Top Soil", "Bottom Soil"), row$component))
add.corr("../../data/climelevcorr.csv",
         function(row) paste("BioClim", row$bioclim),
         function(row) "Elevation")
add.corr("../../data/elevsoilcorr.csv",
         function(row) "Elevation",
         function(row) paste(ifelse(row$soiltype == "topsoil", "Top Soil", "Bottom Soil"), row$component))
add.corr("../../data/latclimcorr.csv",
         function(row) paste("BioClim", row$bioclim),
         function(row) "Latitude")
add.corr("../../data/latelevcorr.csv",
         function(row) "Elevation",
         function(row) "Latitude")
add.corr("../../data/latsoilcorr.csv",
         function(row) paste(ifelse(row$soiltype == "topsoil", "Top Soil", "Bottom Soil"), row$component),
         function(row) "Latitude")
add.corr("../../data/soilcorr.csv",
         function(row) paste(ifelse(row$soiltype1 == "topsoil", "Top Soil", "Bottom Soil"), row$component1),
         function(row) paste(ifelse(row$soiltype2 == "topsoil", "Top Soil", "Bottom Soil"), row$component2))
add.corr("../../data/travclimcorr.csv",
         function(row) "Transport",
         function(row) paste("BioClim", row$bio))
add.corr("../../data/travelevcorr.csv",
         function(row) "Transport",
         function(row) "Elevation")
add.corr("../../data/travlatcorr.csv",
         function(row) "Transport",
         function(row) "Latitude")
add.corr("../../data/travsoilcorr.csv",
         function(row) "Transport",
         function(row) paste(ifelse(row$soiltype == "topsoil", "Top Soil", "Bottom Soil"), row$component))
add.corr("../../data/gaezclimcorr.csv",
         function(row) paste("BioClim", row$bio),
         function(row) paste0(toupper(substring(row$variety, 1, 1)), substring(row$variety, 2)))
add.corr("../../data/gaezelevcorr.csv",
         function(row) "Elevation",
         function(row) paste0(toupper(substring(row$variety, 1, 1)), substring(row$variety, 2)))
add.corr("../../data/gaezlatcorr.csv",
         function(row) "Latitude",
         function(row) paste0(toupper(substring(row$variety, 1, 1)), substring(row$variety, 2)))
add.corr("../../data/gaezsoilcorr.csv",
         function(row) paste(ifelse(row$soiltype == "topsoil", "Top Soil", "Bottom Soil"), row$component),
         function(row) paste0(toupper(substring(row$variety, 1, 1)), substring(row$variety, 2)))
add.corr("../../data/gaeztravcorr.csv",
         function(row) "Transport",
         function(row) paste0(toupper(substring(row$variety, 1, 1)), substring(row$variety, 2)))

names[names == "BioClim 1"] <- "Mean Temp."
names[names == "BioClim 2"] <- "Diurnal Range"
names[names == "BioClim 3"] <- "Isothermality"
names[names == "BioClim 4"] <- "Temp. Seasonality"
names[names == "BioClim 5"] <- "Max Temp."
names[names == "BioClim 6"] <- "Min Temp."
names[names == "BioClim 7"] <- "Temp. Range"
names[names == "BioClim 8"] <- "Wettest Temp."
names[names == "BioClim 9"] <- "Driest Temp."
names[names == "BioClim 10"] <- "Warmest Temp."
names[names == "BioClim 11"] <- "Coldest Temp."
names[names == "BioClim 12"] <- "Precipitation"
names[names == "BioClim 13"] <- "Max Precip."
names[names == "BioClim 14"] <- "Min Precip."
names[names == "BioClim 15"] <- "Precip. Seasonality"
names[names == "BioClim 16"] <- "Wettest Precip."
names[names == "BioClim 17"] <- "Driest Precip."
names[names == "BioClim 18"] <- "Warmest Precip."
names[names == "BioClim 19"] <- "Coldest Precip."

names[names == "Top Soil 1"] <- "Top Soil Sand"
names[names == "Top Soil 2"] <- "Top Soil Silt"
names[names == "Top Soil 3"] <- "Top Soil Clay"
names[names == "Top Soil 4"] <- "Top Soil Carbon"
names[names == "Top Soil 5"] <- "Top Soil CaCO3"
names[names == "Top Soil 6"] <- "Top Soil Gypsum"
names[names == "Bottom Soil 1"] <- "Bottom Soil Sand"
names[names == "Bottom Soil 2"] <- "Bottom Soil Silt"
names[names == "Bottom Soil 3"] <- "Bottom Soil Clay"
names[names == "Bottom Soil 4"] <- "Bottom Soil Carbon"
names[names == "Bottom Soil 5"] <- "Bottom Soil CaCO3"
names[names == "Bottom Soil 6"] <- "Bottom Soil Gypsum"

names[names == "Transport"] <- "Transport Time"

names[names == "Arabica"] <- "Arabica Constraints"
names[names == "Robusta"] <- "Robusta Constraints"

rownames(tbl) <- names
colnames(tbl) <- names
library(reshape2)
df <- melt(tbl, na.rm = TRUE)

df$Var2 <- factor(df$Var2, levels=rev(levels(df$Var1)))
df$label <- round(df$value, 1)

library(ggplot2)

ggplot(df, aes(Var1, Var2, fill=value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                      midpoint = 0, limit = c(-1,1), space = "Lab",
                      name="Pearson\nCorrelation") +
    theme_minimal() + geom_text(aes(Var1, Var2, label=label), color = "black", size = 2) +

    theme(axis.text.x = element_text(angle = 60, vjust = 1,
                                     size = 9, hjust = 1))+
    coord_fixed() +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.8, 0.6),
        legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
