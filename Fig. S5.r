rm(list=ls())
library(ggplot2)
library(basicTrendline)
library(graphics)
library(grid)
library(gridExtra)

add_label_legend <- function(pos = "topleft", label, ...) {
  legend(pos, label, bty = "n", ...)
}

mbc_map <- read.table("mbc_map.txt", sep = "", header = F) 

lat <- rep(seq(89.75, -89.75, length.out = 360), each = 720)
lon <- rep(seq(-179.75, 179.75, length.out = 720), times = 360)

mbc_map_tot <- data.frame(lat, lon, mbc_map)
colnames(mbc_map_tot) = c("lat", "lon", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
mbc_map_tot[mbc_map_tot == -9999] <- NA

mbc_obs_tot <- read.table("data_vali.csv", sep = ",", header = T)

png("Fig_S5.png", width=45, height=35, units="cm",res=600)
par(family = "Times New Roman", mar=c(4,5,2.5,1))

loc=layout(matrix(c(1,1,5,5,9,9,13, 2,2,6,6,10,10,13, 3,3,7,7,11,11,13, 4,4,8,8,12,12,13),7,4))
layout.show(loc)

plot(x=mbc_map_tot$lat, y = mbc_map_tot$jan, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Jan", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==1,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==1,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[1], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$feb, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Feb", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==2,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==2,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[2], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$mar, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Mar", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==3,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==3,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[3], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$apr, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Apr", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==4,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==4,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[4], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$may, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "May", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==5,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==5,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[5], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$jun, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Jun", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==6,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==6,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[6], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$jul, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Jul", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==7,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==7,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[7], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$aug, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Aug", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==8,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==8,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[8], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$sep, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Sep", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==9,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==9,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[9], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$oct, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Oct", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==10,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==10,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[10], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$nov, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Nov", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==11,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==11,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[11], ")"))

plot(x=mbc_map_tot$lat, y = mbc_map_tot$dec, col = "black", Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", main = "Dec", pch=16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=3, cex = 1.8, xlim = c(-89.5, 89.5), xlab=expression(paste("Latitude (", degree, ")")), ylab=expression(paste("Microbial biomass carbon (g C ", m^-2, ")" )), font.axis = 1.8, cex.axis = 2.5, cex.lab = 2.2, tck = 0.02)
points(x = mbc_obs_tot[mbc_obs_tot$month==12,]$lat, y = mbc_obs_tot[mbc_obs_tot$month==12,]$mbc, pch = 23, col="red", cex = 1.5)
add_label_legend("topleft", cex = 3, paste0("(", letters[12], ")"))

plot.new()
legend(x="center", ncol=1, c("Simulated microbial biomass carbon", "Observed microbial biomass carbon"), xpd = NA, horiz = F, pch = c(16, 23), col = c("black", "red"), cex = 3, box.col = "white", title="Legend")

dev.off()
