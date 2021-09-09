rm(list=ls())
library(plyr)
library(dplyr)
library(caret)
library(zoo)
library(basicTrendline)
library(graphics)


datain_mbc_tot_1 <- read.table("vali_full_low.txt", header = F, sep = " ")
colnames(datain_mbc_tot_1) = c("month", "mbc_sim", "mbc_obs")

datain_mbc_tot_2 <- read.table("vali_full_mid.txt", header = F, sep = " ")
colnames(datain_mbc_tot_2) = c("month", "mbc_sim", "mbc_obs")

datain_mbc_tot_3 <- read.table("vali_full_high.txt", header = F, sep = " ")
colnames(datain_mbc_tot_3) = c("month", "mbc_sim", "mbc_obs")



png("Fig_S4.png", width=10, height=10, units="cm",res=600)
par(family = "Times New Roman")

par(fig=c(0,1.98/4,2/4,3.98/4), new=TRUE, mar=c(1,2,1,0))
plot(x = datain_mbc_tot_1$month, y = datain_mbc_tot_1$mbc_sim, type="b", mgp = c(1, 0.2, 0), xlim = c(1, max(datain_mbc_tot_1$month)), ylim = c(min(datain_mbc_tot_1$mbc_sim, datain_mbc_tot_1$mbc_obs, na.rm = TRUE), max(datain_mbc_tot_1$mbc_sim, data_tot_sim$mbc_obs, na.rm = TRUE)), col = "red", main = expression("0-25" * degree * " N"),  Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", pch = 8, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=1, cex = 1.2, xlab=expression(paste(" ")), xaxt='n', ylab=expression(paste("Normalized MBC")), font.axis = 1, cex.axis =0.8, cex.lab = 0.8, tck = 0.02)
points(x = datain_mbc_tot_1$month, y = datain_mbc_tot_1$mbc_obs, pch = 16, col = alpha("black", 0.6), ylim = c(min(datain_mbc_tot_1$mbc_sim, datain_mbc_tot_1$mbc_obs, na.rm = TRUE), max(datain_mbc_tot_1$mbc_sim, datain_mbc_tot_1$mbc_obs, na.rm = TRUE)), cex = 0.75)
axis(side = 1, mgp = c(2, 0.1, 0), cex.axis = 1.0, at=c(1, 4, 7, 10), labels=c(paste("Jan"), paste("Apr"), paste("Jul"), paste("Oct")), tck = 0.02)


par(fig=c(2/4,3.98/4,2/4,3.98/4), new=TRUE, mar=c(1,2,1,0))
plot(x = datain_mbc_tot_2$month, y = datain_mbc_tot_2$mbc_sim, type="b", mgp = c(1, 0.2, 0), xlim = c(1, max(datain_mbc_tot_2$month)), ylim = c(min(datain_mbc_tot_2$mbc_sim, datain_mbc_tot_2$mbc_obs, na.rm = TRUE), max(datain_mbc_tot_2$mbc_sim, datain_mbc_tot_2$mbc_obs, na.rm = TRUE)), col = "red", main = expression("25-50" * degree * " N"),  Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", pch = 8, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=1, cex = 1.2, xlab=expression(paste(" ")), xaxt='n', ylab=expression(paste("Normalized MBC")), font.axis = 1, cex.axis =0.8, cex.lab = 0.8, tck = 0.02)
points(x = datain_mbc_tot_2$month, y = datain_mbc_tot_2$mbc_obs, pch = 16, col = alpha("black", 0.6), ylim = c(min(datain_mbc_tot_2$mbc_sim, datain_mbc_tot_2$mbc_obs, na.rm = TRUE), max(datain_mbc_tot_2$mbc_sim, datain_mbc_tot_2$mbc_obs, na.rm = TRUE)), cex = 0.75)
axis(side = 1, mgp = c(2, 0.1, 0), cex.axis = 1.0, at=c(1, 4, 7, 10), labels=c(paste("Jan"), paste("Apr"), paste("Jul"), paste("Oct")), tck = 0.02)

par(fig=c(0,1.98/4,0/4,1.98/4), new=TRUE, mar=c(1,2,1,0))
plot(x = datain_mbc_tot_3$month, y = datain_mbc_tot_3$mbc_sim, type="b", mgp = c(1, 0.2, 0), xlim = c(1, max(datain_mbc_tot_3$month)), ylim = c(min(datain_mbc_tot_3$mbc_sim, datain_mbc_tot_3$mbc_obs, na.rm = TRUE), max(datain_mbc_tot_3$mbc_sim, datain_mbc_tot_3$mbc_obs, na.rm = TRUE)), col = "red", main = expression("50-90" * degree * " N"),  Pvalue.corrected = TRUE, CI.level = 0.95, linecolor = "black", pch = 8, ePos.x = NA, eSize= 1.5, summary=TRUE, eDigit = 2, show.equation = FALSE, Pname = 1, cex.main=1, cex = 1.2, xlab=expression(paste(" ")), xaxt='n', ylab=expression(paste("Normalized MBC")), font.axis = 1, cex.axis =0.8, cex.lab = 0.8, tck = 0.02)
points(x = datain_mbc_tot_3$month, y = datain_mbc_tot_3$mbc_obs, pch = 16, col = alpha("black", 0.6), ylim = c(min(datain_mbc_tot_3$mbc_sim, datain_mbc_tot_3$mbc_obs, na.rm = TRUE), max(datain_mbc_tot_3$mbc_sim, datain_mbc_tot_3$mbc_obs, na.rm = TRUE)), cex = 0.75)
axis(side = 1, mgp = c(2, 0.1, 0), cex.axis = 1.0, at=c(1, 4, 7, 10), labels=c(paste("Jan"), paste("Apr"), paste("Jul"), paste("Oct")), tck = 0.02)

par(fig=c(2/4,3.98/4,0/4,1.98/4), new=TRUE, mai=c(0,0,0,0))
legend(x="center", bg="transparent", ncol=1, c("Observed", "Simulated"), xpd = NA, horiz = F, pch = c(16, 8), col = c(alpha("black", 0.6), "red"), cex = 1, box.col = "transparent", title="Legend")

dev.off()

