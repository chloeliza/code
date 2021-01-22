rm(list = ls())
library(tidyverse)
library(openxlsx)
library(sf)
library(magrittr)

###load data##change directory as needed#################################################
data_dir <-
  "C:/Users/Chloe/OneDrive - University of Pittsburgh/Sorrells Lab/DCXPSANeuN/dot mapping/analysis"
file_name <- "dcxpsaneun_12_24y_s68_psag_dcxr_neunfr_MPL2.xlsx"
file_path <- paste(data_dir, file_name, sep = "/")

y24_s68 <- as_tibble(read.xlsx(file_path, sheet = 1))

###label Type variable###################################################################
y24_s68$Type <- factor(y24_s68$Type,
                    levels = c(1,2,3,4),
                    labels = c("DCX+", "NEUN+", "DCX+NEUN+", "ref"))

###subset data###########################################################################
line <- subset(y24_s68, Type=="ref")

points <- subset(y24_s68, Type %in% c("DCX+", "NEUN+", "DCX+NEUN+"))

###plot line#############################################################################
plot(line$MarkerX, line$MarkerY)

plot(line$MarkerX, line$MarkerY, xlim = c(0, 1024), ylim = c(0, 1024))

###calculate distance from points to line################################################
###ref: https://stackoverflow.com/questions/46106489/calculate-distance-of-points-normal-to-a-line
###turn points into coordinates
points_coord <- st_as_sf(points, coords = c("MarkerX","MarkerY"))

###turn line into spatial data
x <- line$MarkerX
y <- line$MarkerY
newline0 = cbind(x,y)
newline <- st_linestring(newline0, dim = "XY")

###calculate distance!!!
points_coord$distance <- c(st_distance(points_coord, newline))

###scale to microns; ADJUST AS NECESSARY ACCORDING TO IMAGE COLLECTION##################
mtopx_scale <- 1/0.66

points_coord %<>% 
  mutate(distance_micron = distance*mtopx_scale)

###identify markers on other side of line for exclusion##################################
identify(points$MarkerX, points$MarkerY, label=row.names(points))
#exclude
ex = c(310, 522, 523, 311, 528, 529, 581, 582, 655, 656, 657,
       658, 660, 661, 662, 663, 878, 879, 880, 881, 886, 901)

points_coord_ex = points_coord[-ex,]

###plot##################################################################################
###ref: https://www.datanovia.com/en/blog/pch-in-r-best-tips/
colors <- c("#00C000", "#FF00FF", "#000000")
colors <- colors[as.numeric(points_coord_ex$Type)]

shapes = c(0, 16, 17) 
shapes <- shapes[as.numeric(points_coord_ex$Type)]

plot_sf(points_coord_ex)
plot(points_coord_ex, add = TRUE, col = colors, pch = shapes)
plot(newline, add = TRUE, lty = 2, lwd = 2)
legend("topleft", legend = c("DCX+", "NEUN+", "DCX+NEUN+"),
       col = c("#00C000", "#FF00FF", "#000000"), pch = c(0, 16, 17))

###violin
points_coord_ex$Type <- factor(points_coord_ex$Type,
                       levels = c('DCX+','DCX+NEUN+', 'NEUN+'),ordered = TRUE)
violin <- points_coord_ex %>% 
  ggplot() +
  geom_violin(aes(x=distance_micron, y=Type, fill=Type), alpha=0.3) +
  geom_point(aes(x=distance_micron, y=Type, color=Type), alpha=0.3, position="jitter") +
  scale_color_manual(values=c("#00C000", "#000000", "#FF00FF")) +
  scale_fill_manual(values=c("#00C000", "#000000", "#FF00FF")) +
  theme_bw() +
    theme(strip.background = element_blank(),
          panel.border = element_rect(colour = "black"))
violin <- violin + theme(axis.title.x = element_text(size = 30),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 30, color = "black"),
          axis.text.y = element_text(size = 30, color = "black")) +
          labs(x = "Distance (microns)")
violin <- violin + theme(legend.position = "none") 
  
###nice graphs
install.packages('Cairo')
library(Cairo)
CairoWin()

setwd("C:/Users/Chloe/OneDrive - University of Pittsburgh/Sorrells Lab/DCXPSANeuN/dot mapping/analysis/images")

tiff(filename="24y_s68_MPL2_violin.tiff",
     type="cairo",
     units="in",
     width=10, 
     height=5,
     res=600)
plot(violin)
dev.off()

###export data############################################################################
points_coord_ex_df <- points_coord_ex %>% st_drop_geometry()
class(points_coord_ex_df)

detach(package:openxlsx)
library(xlsx)
ws_path <- 
  "C:/Users/Chloe/OneDrive - University of Pittsburgh/Sorrells Lab/DCXPSANeuN/dot mapping/analysis/distances"
file_nm_ex <- "dcxpsaneun_12_24y_s68_psag_dcxr_neunfr_MPL2_distances.xlsx"
write.xlsx(points_coord_ex_df, file=paste(ws_path, file_nm_ex, sep = "/"), sheetName="distance", append = FALSE)

