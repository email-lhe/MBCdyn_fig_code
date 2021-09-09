rm(list=ls())
library(plyr)
library(dplyr)
library(caret)
library(zoo)
library(basicTrendline)
library(graphics)



data_tot_sim <- read.table("datain_mbc_sim_mn.txt", header = F, sep = " ")
colnames(data_tot_sim) = c("ID", "mbc_low", "mbc_mid", "mbc_high")


png("Fig_S3.png", width=10, height=10, units="cm",res=600)
par(family = "Times New Roman")

par(fig=c(0,1.98/4,2/4,3.98/4), new=TRUE, mar=c(1.6,1.8,0.8,0.2))
plot(x = data_tot_sim$ID, y = data_tot_sim$mbc_low, type="l", mgp = c(0.8, -0.1, 0), xlim = c(1, max(data_tot_sim$ID)), ylim = c(min(data_tot_sim$mbc_low, na.rm = TRUE), max(data_tot_sim$mbc_low, na.rm = TRUE)), col = alpha("black", 0.6), main = expression("0-25" * degree * " N"),  Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", pch = 16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=1, cex = 1.2, xlab=expression(paste("Time (Day)")), ylab=expression(paste("MBC (g C ", m^-2, ")")), font.axis = 1, cex.axis =0.8, cex.lab = 0.8, tck = 0.02)

par(fig=c(2/4,3.98/4,2/4,3.98/4), new=TRUE, mar=c(1.6,1.8,0.8,0.2))
plot(x = data_tot_sim$ID, y = data_tot_sim$mbc_mid, type="l", mgp = c(0.8, -0.1, 0), xlim = c(1, max(data_tot_sim$ID)), ylim = c(min(data_tot_sim$mbc_mid, na.rm = TRUE), max(data_tot_sim$mbc_mid, na.rm = TRUE)), col = alpha("black", 0.6), main = expression("25-50" * degree * " N"),  Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", pch = 16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=1, cex = 1.2, xlab=expression(paste("Time (Day)")), ylab=expression(paste("MBC (g C ", m^-2, ")")), font.axis = 1, cex.axis =0.8, cex.lab = 0.8, tck = 0.02)

par(fig=c(0,1.98/4,0/4,1.98/4), new=TRUE, mar=c(1.6,1.8,0.8,0.2))
plot(x = data_tot_sim$ID, y = data_tot_sim$mbc_high, type="l", mgp = c(0.8, -0.1, 0), xlim = c(1, max(data_tot_sim$ID)), ylim = c(min(data_tot_sim$mbc_high, na.rm = TRUE), max(data_tot_sim$mbc_high, na.rm = TRUE)), col = alpha("black", 0.6), main = expression("50-90" * degree * " N"),  Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", pch = 16, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=1, cex = 1.2, xlab=expression(paste("Time (Day)")), ylab=expression(paste("MBC (g C ", m^-2, ")")), font.axis = 1, cex.axis =0.8, cex.lab = 0.8, tck = 0.02)

dev.off()

