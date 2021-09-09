rm(list = ls()) 

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(grid)
library(colortools)


data <- read.table("MBC_30cm_lat.txt", sep = " ", header = F)

colnames(data) = c("latitude", "mean", "min", "max", "month")
data[data == -9999] <- NA
data <- na.omit(data)

data1_ori <- subset(data, latitude >= 50)
data2_ori <- subset(data, latitude >= 25 & latitude < 50)
data3_ori <- subset(data, latitude >= 0 & latitude < 25)
data4_ori <- subset(data, latitude >= -25 & latitude < 0)
data5_ori <- subset(data, latitude >= -50 & latitude < -25)
data6_ori <- subset(data,  latitude < -50)

data1 <- aggregate(x = data1_ori, by = list(data1_ori$month), FUN = "mean")
data2 <- aggregate(x = data2_ori, by = list(data2_ori$month), FUN = "mean")
data3 <- aggregate(x = data3_ori, by = list(data3_ori$month), FUN = "mean")
data4 <- aggregate(x = data4_ori, by = list(data4_ori$month), FUN = "mean")
data5 <- aggregate(x = data5_ori, by = list(data5_ori$month), FUN = "mean")
data6 <- aggregate(x = data6_ori, by = list(data6_ori$month), FUN = "mean")

y<-data$latitude
x<-data$month

Vardata<-data$mean
Vardata_min<-data$min
Vardata_max<-data$max

yy=Vardata
xx=y
month2=x
Vardata_min2<-Vardata_min
Vardata_max2<-Vardata_max

Figdata2<-data.frame(xx,yy,month2,Vardata_min,Vardata_max)


TaiESM1 <- read.table("TaiESM1_P6.txt", sep = "", header = F)
NorCPM1 <- read.table("NorCPM1_P6.txt", sep = "", header = F)
CMCC_CM2_SR5 <- read.table("CMCC_CM2_SR5_P6.txt", sep = "", header = F)
CESM2_WACCM <- read.table("CESM2_WACCM_P6.txt", sep = "", header = F)
CESM2 <- read.table("CESM2_P6.txt", sep = "", header = F)
CanESM5 <- read.table("CanESM5_P6.txt", sep = "", header = F)
BCC_ESM1 <- read.table("BCC_ESM1_P6.txt", sep = "", header = F)
BCC_CSM2_MR <- read.table("BCC_CSM2_MR_P6.txt", sep = "", header = F)
ACCESS_ESM1_5 <- read.table("ACCESS_ESM1_5_P6.txt", sep = "", header = F)
GFDL_ESM4 <- read.table("GFDL_ESM4_P6.txt", sep = "", header = F)
E3SM_1_1 <- read.table("E3SM_1_1_P6.txt", sep = "", header = F)
E3SM_1_1_ECA <- read.table("E3SM_1_1_ECA_P6.txt", sep = "", header = F)
MPI_ESM1_2_LR <- read.table("MPI_ESM1_2_LR_P6.txt", sep = "", header = F)
MPI_ESM_1_2_HAM <- read.table("MPI_ESM_1_2_HAM_P6.txt", sep = "", header = F)
NorESM2_LM <- read.table("NorESM2_LM_P6.txt", sep = "", header = F)
Konings <- read.table("Konings_P6.txt", sep = "", header = F)

month3 = rep(seq(1, 12, 1), times = 6)
lat = rep(seq(1, 6, 1), each = 12)

data_TaiESM1 = data.frame(TaiESM1, month3, lat)
data_NorCPM1 = data.frame(NorCPM1, month3, lat)
data_CMCC_CM2_SR5 = data.frame(CMCC_CM2_SR5, month3, lat)
data_CESM2_WACCM = data.frame(CESM2_WACCM, month3, lat)
data_CESM2 = data.frame(CESM2, month3, lat)
data_CanESM5 = data.frame(CanESM5, month3, lat)
data_BCC_ESM1 = data.frame(BCC_ESM1, month3, lat)
data_BCC_CSM2_MR = data.frame(BCC_CSM2_MR, month3, lat)
data_ACCESS_ESM1_5 = data.frame(ACCESS_ESM1_5, month3, lat)
data_GFDL_ESM4 = data.frame(GFDL_ESM4, month3, lat)
data_E3SM_1_1 = data.frame(E3SM_1_1, month3, lat)
data_E3SM_1_1_ECA = data.frame(E3SM_1_1_ECA, month3, lat)
data_MPI_ESM1_2_LR = data.frame(MPI_ESM1_2_LR, month3, lat)
data_MPI_ESM_1_2_HAM = data.frame(MPI_ESM_1_2_HAM, month3, lat)
data_NorESM2_LM = data.frame(NorESM2_LM, month3, lat)
data_Konings = data.frame(Konings, month3, lat)

colnames(data_TaiESM1) = c("RH", "Month","Latitude")
colnames(data_NorCPM1) = c("RH", "Month","Latitude")
colnames(data_CMCC_CM2_SR5) = c("RH", "Month","Latitude")
colnames(data_CESM2_WACCM) = c("RH", "Month","Latitude")
colnames(data_CESM2) = c("RH", "Month","Latitude")
colnames(data_CanESM5) = c("RH", "Month","Latitude")
colnames(data_BCC_ESM1) = c("RH", "Month","Latitude")
colnames(data_BCC_CSM2_MR) = c("RH", "Month","Latitude")
colnames(data_ACCESS_ESM1_5) = c("RH", "Month","Latitude")
colnames(data_GFDL_ESM4) = c("RH", "Month","Latitude")
colnames(data_E3SM_1_1) = c("RH", "Month","Latitude")
colnames(data_E3SM_1_1_ECA) = c("RH", "Month","Latitude")
colnames(data_MPI_ESM1_2_LR) = c("RH", "Month","Latitude")
colnames(data_MPI_ESM_1_2_HAM) = c("RH", "Month","Latitude")
colnames(data_NorESM2_LM) = c("RH", "Month","Latitude")
colnames(data_Konings) = c("RH", "Month","Latitude")

data_Konings[data_Konings <= -9999] <- NA

data_TaiESM1$RH = data_TaiESM1$RH * 10^3 * 10^6 / 12
data_NorCPM1$RH = data_NorCPM1$RH * 10^3 * 10^6 / 12
data_CMCC_CM2_SR5$RH = data_CMCC_CM2_SR5$RH * 10^3 * 10^6 / 12
data_CESM2_WACCM$RH = data_CESM2_WACCM$RH * 10^3 * 10^6 / 12
data_CESM2$RH = data_CESM2$RH * 10^3 * 10^6 / 12
data_CanESM5$RH = data_CanESM5$RH * 10^3 * 10^6 / 12
data_BCC_ESM1$RH = data_BCC_ESM1$RH * 10^3 * 10^6 / 12
data_BCC_CSM2_MR$RH = data_BCC_CSM2_MR$RH * 10^3 * 10^6 / 12
data_ACCESS_ESM1_5$RH = data_ACCESS_ESM1_5$RH * 10^3 * 10^6 / 12
data_GFDL_ESM4$RH = data_GFDL_ESM4$RH * 10^3 * 10^6 / 12
data_E3SM_1_1$RH = data_E3SM_1_1$RH * 10^3 * 10^6 / 12
data_E3SM_1_1_ECA$RH = data_E3SM_1_1_ECA$RH * 10^3 * 10^6 / 12
data_MPI_ESM1_2_LR$RH = data_MPI_ESM1_2_LR$RH * 10^3 * 10^6 / 12
data_MPI_ESM_1_2_HAM$RH = data_MPI_ESM_1_2_HAM$RH * 10^3 * 10^6 / 12
data_NorESM2_LM$RH = data_NorESM2_LM$RH * 10^3 * 10^6 / 12
data_Konings$RH = data_Konings$RH * 10^6 / (365 * 24 * 3600 * 12)

data_TaiESM1_L1 = subset(data_TaiESM1, Latitude == 1)
data_NorCPM1_L1 = subset(data_NorCPM1, Latitude == 1)
data_CMCC_CM2_SR5_L1 = subset(data_CMCC_CM2_SR5, Latitude == 1)
data_CESM2_WACCM_L1 = subset(data_CESM2_WACCM, Latitude == 1)
data_CESM2_L1 = subset(data_CESM2, Latitude == 1)
data_CanESM5_L1 = subset(data_CanESM5, Latitude == 1)
data_BCC_ESM1_L1 = subset(data_BCC_ESM1, Latitude == 1)
data_BCC_CSM2_MR_L1 = subset(data_BCC_CSM2_MR, Latitude == 1)
data_ACCESS_ESM1_5_L1 = subset(data_ACCESS_ESM1_5, Latitude == 1)
data_GFDL_ESM4_L1 = subset(data_GFDL_ESM4, Latitude == 1)
data_E3SM_1_1_L1 = subset(data_E3SM_1_1, Latitude == 1)
data_E3SM_1_1_ECA_L1 = subset(data_E3SM_1_1_ECA, Latitude == 1)
data_MPI_ESM1_2_LR_L1 = subset(data_MPI_ESM1_2_LR, Latitude == 1)
data_MPI_ESM_1_2_HAM_L1 = subset(data_MPI_ESM_1_2_HAM, Latitude == 1)
data_NorESM2_LM_L1 = subset(data_NorESM2_LM, Latitude == 1)
data_Konings_L1 = subset(data_Konings, Latitude == 1)

data_TaiESM1_L2 = subset(data_TaiESM1, Latitude == 2)
data_NorCPM1_L2 = subset(data_NorCPM1, Latitude == 2)
data_CMCC_CM2_SR5_L2 = subset(data_CMCC_CM2_SR5, Latitude == 2)
data_CESM2_WACCM_L2 = subset(data_CESM2_WACCM, Latitude == 2)
data_CESM2_L2 = subset(data_CESM2, Latitude == 2)
data_CanESM5_L2 = subset(data_CanESM5, Latitude == 2)
data_BCC_ESM1_L2 = subset(data_BCC_ESM1, Latitude == 2)
data_BCC_CSM2_MR_L2 = subset(data_BCC_CSM2_MR, Latitude == 2)
data_ACCESS_ESM1_5_L2 = subset(data_ACCESS_ESM1_5, Latitude == 2)
data_GFDL_ESM4_L2 = subset(data_GFDL_ESM4, Latitude == 2)
data_E3SM_1_1_L2 = subset(data_E3SM_1_1, Latitude == 2)
data_E3SM_1_1_ECA_L2 = subset(data_E3SM_1_1_ECA, Latitude == 2)
data_MPI_ESM1_2_LR_L2 = subset(data_MPI_ESM1_2_LR, Latitude == 2)
data_MPI_ESM_1_2_HAM_L2 = subset(data_MPI_ESM_1_2_HAM, Latitude == 2)
data_NorESM2_LM_L2 = subset(data_NorESM2_LM, Latitude == 2)
data_Konings_L2 = subset(data_Konings, Latitude == 2)

data_TaiESM1_L3 = subset(data_TaiESM1, Latitude == 3)
data_NorCPM1_L3 = subset(data_NorCPM1, Latitude == 3)
data_CMCC_CM2_SR5_L3 = subset(data_CMCC_CM2_SR5, Latitude == 3)
data_CESM2_WACCM_L3 = subset(data_CESM2_WACCM, Latitude == 3)
data_CESM2_L3 = subset(data_CESM2, Latitude == 3)
data_CanESM5_L3 = subset(data_CanESM5, Latitude == 3)
data_BCC_ESM1_L3 = subset(data_BCC_ESM1, Latitude == 3)
data_BCC_CSM2_MR_L3 = subset(data_BCC_CSM2_MR, Latitude == 3)
data_ACCESS_ESM1_5_L3 = subset(data_ACCESS_ESM1_5, Latitude == 3)
data_GFDL_ESM4_L3 = subset(data_GFDL_ESM4, Latitude == 3)
data_E3SM_1_1_L3 = subset(data_E3SM_1_1, Latitude == 3)
data_E3SM_1_1_ECA_L3 = subset(data_E3SM_1_1_ECA, Latitude == 3)
data_MPI_ESM1_2_LR_L3 = subset(data_MPI_ESM1_2_LR, Latitude == 3)
data_MPI_ESM_1_2_HAM_L3 = subset(data_MPI_ESM_1_2_HAM, Latitude == 3)
data_NorESM2_LM_L3 = subset(data_NorESM2_LM, Latitude == 3)
data_Konings_L3 = subset(data_Konings, Latitude == 3)

data_TaiESM1_L4 = subset(data_TaiESM1, Latitude == 4)
data_NorCPM1_L4 = subset(data_NorCPM1, Latitude == 4)
data_CMCC_CM2_SR5_L4 = subset(data_CMCC_CM2_SR5, Latitude == 4)
data_CESM2_WACCM_L4 = subset(data_CESM2_WACCM, Latitude == 4)
data_CESM2_L4 = subset(data_CESM2, Latitude == 4)
data_CanESM5_L4 = subset(data_CanESM5, Latitude == 4)
data_BCC_ESM1_L4 = subset(data_BCC_ESM1, Latitude == 4)
data_BCC_CSM2_MR_L4 = subset(data_BCC_CSM2_MR, Latitude == 4)
data_ACCESS_ESM1_5_L4 = subset(data_ACCESS_ESM1_5, Latitude == 4)
data_GFDL_ESM4_L4 = subset(data_GFDL_ESM4, Latitude == 4)
data_E3SM_1_1_L4 = subset(data_E3SM_1_1, Latitude == 4)
data_E3SM_1_1_ECA_L4 = subset(data_E3SM_1_1_ECA, Latitude == 4)
data_MPI_ESM1_2_LR_L4 = subset(data_MPI_ESM1_2_LR, Latitude == 4)
data_MPI_ESM_1_2_HAM_L4 = subset(data_MPI_ESM_1_2_HAM, Latitude == 4)
data_NorESM2_LM_L4 = subset(data_NorESM2_LM, Latitude == 4)
data_Konings_L4 = subset(data_Konings, Latitude == 4)

data_TaiESM1_L5 = subset(data_TaiESM1, Latitude == 5)
data_NorCPM1_L5 = subset(data_NorCPM1, Latitude == 5)
data_CMCC_CM2_SR5_L5 = subset(data_CMCC_CM2_SR5, Latitude == 5)
data_CESM2_WACCM_L5 = subset(data_CESM2_WACCM, Latitude == 5)
data_CESM2_L5 = subset(data_CESM2, Latitude == 5)
data_CanESM5_L5 = subset(data_CanESM5, Latitude == 5)
data_BCC_ESM1_L5 = subset(data_BCC_ESM1, Latitude == 5)
data_BCC_CSM2_MR_L5 = subset(data_BCC_CSM2_MR, Latitude == 5)
data_ACCESS_ESM1_5_L5 = subset(data_ACCESS_ESM1_5, Latitude == 5)
data_GFDL_ESM4_L5 = subset(data_GFDL_ESM4, Latitude == 5)
data_E3SM_1_1_L5 = subset(data_E3SM_1_1, Latitude == 5)
data_E3SM_1_1_ECA_L5 = subset(data_E3SM_1_1_ECA, Latitude == 5)
data_MPI_ESM1_2_LR_L5 = subset(data_MPI_ESM1_2_LR, Latitude == 5)
data_MPI_ESM_1_2_HAM_L5 = subset(data_MPI_ESM_1_2_HAM, Latitude == 5)
data_NorESM2_LM_L5 = subset(data_NorESM2_LM, Latitude == 5)
data_Konings_L5 = subset(data_Konings, Latitude == 5)

data_TaiESM1_L6 = subset(data_TaiESM1, Latitude == 6)
data_NorCPM1_L6 = subset(data_NorCPM1, Latitude == 6)
data_CMCC_CM2_SR5_L6 = subset(data_CMCC_CM2_SR5, Latitude == 6)
data_CESM2_WACCM_L6 = subset(data_CESM2_WACCM, Latitude == 6)
data_CESM2_L6 = subset(data_CESM2, Latitude == 6)
data_CanESM5_L6 = subset(data_CanESM5, Latitude == 6)
data_BCC_ESM1_L6 = subset(data_BCC_ESM1, Latitude == 6)
data_BCC_CSM2_MR_L6 = subset(data_BCC_CSM2_MR, Latitude == 6)
data_ACCESS_ESM1_5_L6 = subset(data_ACCESS_ESM1_5, Latitude == 6)
data_GFDL_ESM4_L6 = subset(data_GFDL_ESM4, Latitude == 6)
data_E3SM_1_1_L6 = subset(data_E3SM_1_1, Latitude == 6)
data_E3SM_1_1_ECA_L6 = subset(data_E3SM_1_1_ECA, Latitude == 6)
data_MPI_ESM1_2_LR_L6 = subset(data_MPI_ESM1_2_LR, Latitude == 6)
data_MPI_ESM_1_2_HAM_L6 = subset(data_MPI_ESM_1_2_HAM, Latitude == 6)
data_NorESM2_LM_L6 = subset(data_NorESM2_LM, Latitude == 6)
data_Konings_L6 = subset(data_Konings, Latitude == 6)

data_TaiESM1_L1$RH = (data_TaiESM1_L1$RH - mean(data_TaiESM1_L1$RH))/mean(data_TaiESM1_L1$RH)
data_NorCPM1_L1$RH = (data_NorCPM1_L1$RH - mean(data_NorCPM1_L1$RH))/mean(data_NorCPM1_L1$RH)
data_CMCC_CM2_SR5_L1$RH = (data_CMCC_CM2_SR5_L1$RH - mean(data_CMCC_CM2_SR5_L1$RH))/mean(data_CMCC_CM2_SR5_L1$RH)
data_CESM2_WACCM_L1$RH = (data_CESM2_WACCM_L1$RH - mean(data_CESM2_WACCM_L1$RH))/mean(data_CESM2_WACCM_L1$RH)
data_CESM2_L1$RH = (data_CESM2_L1$RH - mean(data_CESM2_L1$RH))/mean(data_CESM2_L1$RH)
data_CanESM5_L1$RH = (data_CanESM5_L1$RH - mean(data_CanESM5_L1$RH))/mean(data_CanESM5_L1$RH)
data_BCC_ESM1_L1$RH = (data_BCC_ESM1_L1$RH -mean(data_BCC_ESM1_L1$RH))/mean(data_BCC_ESM1_L1$RH)
data_BCC_CSM2_MR_L1$RH = (data_BCC_CSM2_MR_L1$RH - mean(data_BCC_CSM2_MR_L1$RH))/mean(data_BCC_CSM2_MR_L1$RH)
data_ACCESS_ESM1_5_L1$RH = (data_ACCESS_ESM1_5_L1$RH - mean(data_ACCESS_ESM1_5_L1$RH))/mean(data_ACCESS_ESM1_5_L1$RH)
data_GFDL_ESM4_L1$RH = (data_GFDL_ESM4_L1$RH - mean(data_GFDL_ESM4_L1$RH))/mean(data_GFDL_ESM4_L1$RH)
data_E3SM_1_1_L1$RH = (data_E3SM_1_1_L1$RH - mean(data_E3SM_1_1_L1$RH))/mean(data_E3SM_1_1_L1$RH)
data_E3SM_1_1_ECA_L1$RH = (data_E3SM_1_1_ECA_L1$RH - mean(data_E3SM_1_1_ECA_L1$RH))/mean(data_E3SM_1_1_ECA_L1$RH)
data_MPI_ESM1_2_LR_L1$RH = (data_MPI_ESM1_2_LR_L1$RH - mean(data_MPI_ESM1_2_LR_L1$RH))/mean(data_MPI_ESM1_2_LR_L1$RH)
data_MPI_ESM_1_2_HAM_L1$RH = (data_MPI_ESM_1_2_HAM_L1$RH - mean(data_MPI_ESM_1_2_HAM_L1$RH))/mean(data_MPI_ESM_1_2_HAM_L1$RH)
data_NorESM2_LM_L1$RH = (data_NorESM2_LM_L1$RH - mean(data_NorESM2_LM_L1$RH))/mean(data_NorESM2_LM_L1$RH)
data_Konings_L1$RH = (data_Konings_L1$RH - mean(na.omit(data_Konings_L1$RH)))/mean(na.omit(data_Konings_L1$RH))

data_TaiESM1_L2$RH = (data_TaiESM1_L2$RH - mean(data_TaiESM1_L2$RH))/mean(data_TaiESM1_L2$RH)
data_NorCPM1_L2$RH = (data_NorCPM1_L2$RH - mean(data_NorCPM1_L2$RH))/mean(data_NorCPM1_L2$RH)
data_CMCC_CM2_SR5_L2$RH = (data_CMCC_CM2_SR5_L2$RH - mean(data_CMCC_CM2_SR5_L2$RH))/mean(data_CMCC_CM2_SR5_L2$RH)
data_CESM2_WACCM_L2$RH = (data_CESM2_WACCM_L2$RH - mean(data_CESM2_WACCM_L2$RH))/mean(data_CESM2_WACCM_L2$RH)
data_CESM2_L2$RH = (data_CESM2_L2$RH - mean(data_CESM2_L2$RH))/mean(data_CESM2_L2$RH)
data_CanESM5_L2$RH = (data_CanESM5_L2$RH - mean(data_CanESM5_L2$RH))/mean(data_CanESM5_L2$RH)
data_BCC_ESM1_L2$RH = (data_BCC_ESM1_L2$RH -mean(data_BCC_ESM1_L2$RH))/mean(data_BCC_ESM1_L2$RH)
data_BCC_CSM2_MR_L2$RH = (data_BCC_CSM2_MR_L2$RH - mean(data_BCC_CSM2_MR_L2$RH))/mean(data_BCC_CSM2_MR_L2$RH)
data_ACCESS_ESM1_5_L2$RH = (data_ACCESS_ESM1_5_L2$RH - mean(data_ACCESS_ESM1_5_L2$RH))/mean(data_ACCESS_ESM1_5_L2$RH)
data_GFDL_ESM4_L2$RH = (data_GFDL_ESM4_L2$RH - mean(data_GFDL_ESM4_L2$RH))/mean(data_GFDL_ESM4_L2$RH)
data_E3SM_1_1_L2$RH = (data_E3SM_1_1_L2$RH - mean(data_E3SM_1_1_L2$RH))/mean(data_E3SM_1_1_L2$RH)
data_E3SM_1_1_ECA_L2$RH = (data_E3SM_1_1_ECA_L2$RH - mean(data_E3SM_1_1_ECA_L2$RH))/mean(data_E3SM_1_1_ECA_L2$RH)
data_MPI_ESM1_2_LR_L2$RH = (data_MPI_ESM1_2_LR_L2$RH - mean(data_MPI_ESM1_2_LR_L2$RH))/mean(data_MPI_ESM1_2_LR_L2$RH)
data_MPI_ESM_1_2_HAM_L2$RH = (data_MPI_ESM_1_2_HAM_L2$RH - mean(data_MPI_ESM_1_2_HAM_L2$RH))/mean(data_MPI_ESM_1_2_HAM_L2$RH)
data_NorESM2_LM_L2$RH = (data_NorESM2_LM_L2$RH - mean(data_NorESM2_LM_L2$RH))/mean(data_NorESM2_LM_L2$RH)
data_Konings_L2$RH = (data_Konings_L2$RH - mean(na.omit(data_Konings_L2$RH)))/mean(na.omit(data_Konings_L2$RH))

data_TaiESM1_L3$RH = (data_TaiESM1_L3$RH - mean(data_TaiESM1_L3$RH))/mean(data_TaiESM1_L3$RH)
data_NorCPM1_L3$RH = (data_NorCPM1_L3$RH - mean(data_NorCPM1_L3$RH))/mean(data_NorCPM1_L3$RH)
data_CMCC_CM2_SR5_L3$RH = (data_CMCC_CM2_SR5_L3$RH - mean(data_CMCC_CM2_SR5_L3$RH))/mean(data_CMCC_CM2_SR5_L3$RH)
data_CESM2_WACCM_L3$RH = (data_CESM2_WACCM_L3$RH - mean(data_CESM2_WACCM_L3$RH))/mean(data_CESM2_WACCM_L3$RH)
data_CESM2_L3$RH = (data_CESM2_L3$RH - mean(data_CESM2_L3$RH))/mean(data_CESM2_L3$RH)
data_CanESM5_L3$RH = (data_CanESM5_L3$RH - mean(data_CanESM5_L3$RH))/mean(data_CanESM5_L3$RH)
data_BCC_ESM1_L3$RH = (data_BCC_ESM1_L3$RH -mean(data_BCC_ESM1_L3$RH))/mean(data_BCC_ESM1_L3$RH)
data_BCC_CSM2_MR_L3$RH = (data_BCC_CSM2_MR_L3$RH - mean(data_BCC_CSM2_MR_L3$RH))/mean(data_BCC_CSM2_MR_L3$RH)
data_ACCESS_ESM1_5_L3$RH = (data_ACCESS_ESM1_5_L3$RH - mean(data_ACCESS_ESM1_5_L3$RH))/mean(data_ACCESS_ESM1_5_L3$RH)
data_GFDL_ESM4_L3$RH = (data_GFDL_ESM4_L3$RH - mean(data_GFDL_ESM4_L3$RH))/mean(data_GFDL_ESM4_L3$RH)
data_E3SM_1_1_L3$RH = (data_E3SM_1_1_L3$RH - mean(data_E3SM_1_1_L3$RH))/mean(data_E3SM_1_1_L3$RH)
data_E3SM_1_1_ECA_L3$RH = (data_E3SM_1_1_ECA_L3$RH - mean(data_E3SM_1_1_ECA_L3$RH))/mean(data_E3SM_1_1_ECA_L3$RH)
data_MPI_ESM1_2_LR_L3$RH = (data_MPI_ESM1_2_LR_L3$RH - mean(data_MPI_ESM1_2_LR_L3$RH))/mean(data_MPI_ESM1_2_LR_L3$RH)
data_MPI_ESM_1_2_HAM_L3$RH = (data_MPI_ESM_1_2_HAM_L3$RH - mean(data_MPI_ESM_1_2_HAM_L3$RH))/mean(data_MPI_ESM_1_2_HAM_L3$RH)
data_NorESM2_LM_L3$RH = (data_NorESM2_LM_L3$RH - mean(data_NorESM2_LM_L3$RH))/mean(data_NorESM2_LM_L3$RH)
data_Konings_L3$RH = (data_Konings_L3$RH - mean(na.omit(data_Konings_L3$RH)))/mean(na.omit(data_Konings_L3$RH))

data_TaiESM1_L4$RH = (data_TaiESM1_L4$RH - mean(data_TaiESM1_L4$RH))/mean(data_TaiESM1_L4$RH)
data_NorCPM1_L4$RH = (data_NorCPM1_L4$RH - mean(data_NorCPM1_L4$RH))/mean(data_NorCPM1_L4$RH)
data_CMCC_CM2_SR5_L4$RH = (data_CMCC_CM2_SR5_L4$RH - mean(data_CMCC_CM2_SR5_L4$RH))/mean(data_CMCC_CM2_SR5_L4$RH)
data_CESM2_WACCM_L4$RH = (data_CESM2_WACCM_L4$RH - mean(data_CESM2_WACCM_L4$RH))/mean(data_CESM2_WACCM_L4$RH)
data_CESM2_L4$RH = (data_CESM2_L4$RH - mean(data_CESM2_L4$RH))/mean(data_CESM2_L4$RH)
data_CanESM5_L4$RH = (data_CanESM5_L4$RH - mean(data_CanESM5_L4$RH))/mean(data_CanESM5_L4$RH)
data_BCC_ESM1_L4$RH = (data_BCC_ESM1_L4$RH -mean(data_BCC_ESM1_L4$RH))/mean(data_BCC_ESM1_L4$RH)
data_BCC_CSM2_MR_L4$RH = (data_BCC_CSM2_MR_L4$RH - mean(data_BCC_CSM2_MR_L4$RH))/mean(data_BCC_CSM2_MR_L4$RH)
data_ACCESS_ESM1_5_L4$RH = (data_ACCESS_ESM1_5_L4$RH - mean(data_ACCESS_ESM1_5_L4$RH))/mean(data_ACCESS_ESM1_5_L4$RH)
data_GFDL_ESM4_L4$RH = (data_GFDL_ESM4_L4$RH - mean(data_GFDL_ESM4_L4$RH))/mean(data_GFDL_ESM4_L4$RH)
data_E3SM_1_1_L4$RH = (data_E3SM_1_1_L4$RH - mean(data_E3SM_1_1_L4$RH))/mean(data_E3SM_1_1_L4$RH)
data_E3SM_1_1_ECA_L4$RH = (data_E3SM_1_1_ECA_L4$RH - mean(data_E3SM_1_1_ECA_L4$RH))/mean(data_E3SM_1_1_ECA_L4$RH)
data_MPI_ESM1_2_LR_L4$RH = (data_MPI_ESM1_2_LR_L4$RH - mean(data_MPI_ESM1_2_LR_L4$RH))/mean(data_MPI_ESM1_2_LR_L4$RH)
data_MPI_ESM_1_2_HAM_L4$RH = (data_MPI_ESM_1_2_HAM_L4$RH - mean(data_MPI_ESM_1_2_HAM_L4$RH))/mean(data_MPI_ESM_1_2_HAM_L4$RH)
data_NorESM2_LM_L4$RH = (data_NorESM2_LM_L4$RH - mean(data_NorESM2_LM_L4$RH))/mean(data_NorESM2_LM_L4$RH)
data_Konings_L4$RH = (data_Konings_L4$RH - mean(na.omit(data_Konings_L4$RH)))/mean(na.omit(data_Konings_L4$RH))

data_TaiESM1_L5$RH = (data_TaiESM1_L5$RH - mean(data_TaiESM1_L5$RH))/mean(data_TaiESM1_L5$RH)
data_NorCPM1_L5$RH = (data_NorCPM1_L5$RH - mean(data_NorCPM1_L5$RH))/mean(data_NorCPM1_L5$RH)
data_CMCC_CM2_SR5_L5$RH = (data_CMCC_CM2_SR5_L5$RH - mean(data_CMCC_CM2_SR5_L5$RH))/mean(data_CMCC_CM2_SR5_L5$RH)
data_CESM2_WACCM_L5$RH = (data_CESM2_WACCM_L5$RH - mean(data_CESM2_WACCM_L5$RH))/mean(data_CESM2_WACCM_L5$RH)
data_CESM2_L5$RH = (data_CESM2_L5$RH - mean(data_CESM2_L5$RH))/mean(data_CESM2_L5$RH)
data_CanESM5_L5$RH = (data_CanESM5_L5$RH - mean(data_CanESM5_L5$RH))/mean(data_CanESM5_L5$RH)
data_BCC_ESM1_L5$RH = (data_BCC_ESM1_L5$RH -mean(data_BCC_ESM1_L5$RH))/mean(data_BCC_ESM1_L5$RH)
data_BCC_CSM2_MR_L5$RH = (data_BCC_CSM2_MR_L5$RH - mean(data_BCC_CSM2_MR_L5$RH))/mean(data_BCC_CSM2_MR_L5$RH)
data_ACCESS_ESM1_5_L5$RH = (data_ACCESS_ESM1_5_L5$RH - mean(data_ACCESS_ESM1_5_L5$RH))/mean(data_ACCESS_ESM1_5_L5$RH)
data_GFDL_ESM4_L5$RH = (data_GFDL_ESM4_L5$RH - mean(data_GFDL_ESM4_L5$RH))/mean(data_GFDL_ESM4_L5$RH)
data_E3SM_1_1_L5$RH = (data_E3SM_1_1_L5$RH - mean(data_E3SM_1_1_L5$RH))/mean(data_E3SM_1_1_L5$RH)
data_E3SM_1_1_ECA_L5$RH = (data_E3SM_1_1_ECA_L5$RH - mean(data_E3SM_1_1_ECA_L5$RH))/mean(data_E3SM_1_1_ECA_L5$RH)
data_MPI_ESM1_2_LR_L5$RH = (data_MPI_ESM1_2_LR_L5$RH - mean(data_MPI_ESM1_2_LR_L5$RH))/mean(data_MPI_ESM1_2_LR_L5$RH)
data_MPI_ESM_1_2_HAM_L5$RH = (data_MPI_ESM_1_2_HAM_L5$RH - mean(data_MPI_ESM_1_2_HAM_L5$RH))/mean(data_MPI_ESM_1_2_HAM_L5$RH)
data_NorESM2_LM_L5$RH = (data_NorESM2_LM_L5$RH - mean(data_NorESM2_LM_L5$RH))/mean(data_NorESM2_LM_L5$RH)
data_Konings_L5$RH = (data_Konings_L5$RH - mean(na.omit(data_Konings_L5$RH)))/mean(na.omit(data_Konings_L5$RH))


data_TaiESM1_L6$RH = (data_TaiESM1_L6$RH - mean(data_TaiESM1_L6$RH))/mean(data_TaiESM1_L6$RH)
data_NorCPM1_L6$RH = (data_NorCPM1_L6$RH - mean(data_NorCPM1_L6$RH))/mean(data_NorCPM1_L6$RH)
data_CMCC_CM2_SR5_L6$RH = (data_CMCC_CM2_SR5_L6$RH - mean(data_CMCC_CM2_SR5_L6$RH))/mean(data_CMCC_CM2_SR5_L6$RH)
data_CESM2_WACCM_L6$RH = (data_CESM2_WACCM_L6$RH - mean(data_CESM2_WACCM_L6$RH))/mean(data_CESM2_WACCM_L6$RH)
data_CESM2_L6$RH = (data_CESM2_L6$RH - mean(data_CESM2_L6$RH))/mean(data_CESM2_L6$RH)
data_CanESM5_L6$RH = (data_CanESM5_L6$RH - mean(data_CanESM5_L6$RH))/mean(data_CanESM5_L6$RH)
data_BCC_ESM1_L6$RH = (data_BCC_ESM1_L6$RH -mean(data_BCC_ESM1_L6$RH))/mean(data_BCC_ESM1_L6$RH)
data_BCC_CSM2_MR_L6$RH = (data_BCC_CSM2_MR_L6$RH - mean(data_BCC_CSM2_MR_L6$RH))/mean(data_BCC_CSM2_MR_L6$RH)
data_ACCESS_ESM1_5_L6$RH = (data_ACCESS_ESM1_5_L6$RH - mean(data_ACCESS_ESM1_5_L6$RH))/mean(data_ACCESS_ESM1_5_L6$RH)
data_GFDL_ESM4_L6$RH = (data_GFDL_ESM4_L6$RH - mean(data_GFDL_ESM4_L6$RH))/mean(data_GFDL_ESM4_L6$RH)
data_E3SM_1_1_L6$RH = (data_E3SM_1_1_L6$RH - mean(data_E3SM_1_1_L6$RH))/mean(data_E3SM_1_1_L6$RH)
data_E3SM_1_1_ECA_L6$RH = (data_E3SM_1_1_ECA_L6$RH - mean(data_E3SM_1_1_ECA_L6$RH))/mean(data_E3SM_1_1_ECA_L6$RH)
data_MPI_ESM1_2_LR_L6$RH = (data_MPI_ESM1_2_LR_L6$RH - mean(data_MPI_ESM1_2_LR_L6$RH))/mean(data_MPI_ESM1_2_LR_L6$RH)
data_MPI_ESM_1_2_HAM_L6$RH = (data_MPI_ESM_1_2_HAM_L6$RH - mean(data_MPI_ESM_1_2_HAM_L6$RH))/mean(data_MPI_ESM_1_2_HAM_L6$RH)
data_NorESM2_LM_L6$RH = (data_NorESM2_LM_L6$RH - mean(data_NorESM2_LM_L6$RH))/mean(data_NorESM2_LM_L6$RH)
data_Konings_L6$RH = (data_Konings_L6$RH - mean(na.omit(data_Konings_L6$RH)))/mean(na.omit(data_Konings_L6$RH))

data_exp <- read.table("rh_exp.txt", sep = " ", header = T)

data_exp_L1 = subset(data_exp, Latitude >= 50 & Latitude < 90)
data_exp_L2 = subset(data_exp, Latitude >= 25 & Latitude < 50)
data_exp_L3 = subset(data_exp, Latitude >= 0 & Latitude < 25)
data_exp_L4 = subset(data_exp, Latitude >= -25 & Latitude < 0)
data_exp_L5 = subset(data_exp, Latitude >= -50 & Latitude < -25)
data_exp_L6 = subset(data_exp, Latitude < -50)

data_exp_L1_avg = aggregate(data_exp_L1$RH, by = list(data_exp_L1$Month), FUN = mean, na.rm=TRUE)
data_exp_L2_avg = aggregate(data_exp_L2$RH, by = list(data_exp_L2$Month), FUN = mean, na.rm=TRUE)
data_exp_L3_avg = aggregate(data_exp_L3$RH, by = list(data_exp_L3$Month), FUN = mean, na.rm=TRUE)
data_exp_L4_avg = aggregate(data_exp_L4$RH, by = list(data_exp_L4$Month), FUN = mean, na.rm=TRUE)
data_exp_L5_avg = aggregate(data_exp_L5$RH, by = list(data_exp_L5$Month), FUN = mean, na.rm=TRUE)
data_exp_L6_avg = data.frame(NA, NA)

colnames(data_exp_L1_avg) = c("Month", "RH")
colnames(data_exp_L2_avg) = c("Month", "RH")
colnames(data_exp_L3_avg) = c("Month", "RH")
colnames(data_exp_L4_avg) = c("Month", "RH")
colnames(data_exp_L5_avg) = c("Month", "RH")
colnames(data_exp_L6_avg) = c("Month", "RH")

fig_ht <- seq(0.88, 0.24, length.out = 5)
scaleFUN <- function(x) sprintf("%.2f", x)

color_fill = rainbow(12)

png("Fig_4.png", width=10,height=10,units="cm",res=600)
par(family = "Times New Roman", mar=c(2,2,0.5,0))

my_arrow <- arrow(type = "open", ends = "last", length = unit(4, "mm"))

chart1<-ggplot(data =Figdata2,aes(group=month2, color=color_fill[as.factor(month2)]))+
  geom_ribbon(aes(xx, ymin=Vardata_min2, ymax=Vardata_max2,fill=color_fill[as.factor(month2)]),alpha=0.2,colour = NA, show.legend = F)+
  ylab(expression(paste("MBC (gC m"^"-2", ")"), sep = "")) +
  xlab(expression(paste("Latitude (", degree, ")"))) +
  theme(legend.title=element_blank(), plot.margin = margin(0.2, 4.5, 0, 0.2, "cm"))+
  theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
             axis.text.y=element_text(family = "Times New Roman", colour="black", size = 6),
             axis.text.x=element_text(family = "Times New Roman", colour="black", size = 6),
             axis.title.x=element_text(family = "Times New Roman", face="bold", size = 8),
             axis.title.y=element_text(family = "Times New Roman", face="bold", size = 8),
             plot.background=element_rect(I(0),linetype=0),
             panel.background=element_rect(I(0)),
             panel.grid.major=element_line(colour=NA),
             panel.grid.minor=element_line(colour=NA),
             panel.border = element_rect(colour = "black", fill=NA, size=0.4)) +
             scale_x_continuous(breaks = seq(90, -90, -30), labels = seq(90, -90, -30), limits = c(-75, 75)) +
             scale_y_continuous(breaks = seq(0, 800, 200), labels = seq(0, 800, 200), limits = c(0, 800)) +
  coord_flip()+
#  scale_fill_manual(values=color_fill[as.factor(month2)]) +
  geom_line(mapping=aes(xx,yy),size=0.25, alpha=0.5)+
  guides(col = guide_legend(nrow = 3, byrow = TRUE))  +
  #scale_color_brewer(type='qual',palette="Set3")+
  geom_vline(xintercept = c(50, 25, 0, -25, -50), linetype="dotted", color = "grey", size = 0.5) +
  scale_colour_manual(values = color_fill,  labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(legend.text = element_text(family = "Times New Roman", colour="black", size = 6, face = "bold"), legend.justification = "left", legend.key.size = unit(3, "mm"), legend.margin = margin(0.2, 1, 0.5, 0.1, "cm"), legend.position = c(0.02, 0.08), legend.box = "vertical",
    legend.background = element_rect(fill="transparent", size=0.5, colour ="transparent")) +
  annotate("text", y= 20, x = 75, label="a)", size = 3, family = "Times New Roman", face="bold", hjust=1) 


gt <- ggplot_gtable(ggplot_build(chart1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


view6 <- viewport(width=0.465,height=0.15,x=0.852,y=0.97)

Legend <- c("A", "B", "C", "D")
shots <- rep('Label',4)
xc <-c(1:4)
yc <-c(1:4)
df <-data.frame(shots,Legend,xc,yc)
chart7 <- ggplot(df, aes(x = xc, y = yc, color = c("black", "green", "red", "blue"))) +
  geom_line(alpha = 0.4) +
  scale_color_manual(values = c("black", "green", "red", "blue"),
    labels = c("MBC", "Synthesized HR", "Konings HR", "CIMP6 HR"),
    name = '', drop=FALSE) +
  guides(color = guide_legend(override.aes = list(alpha = 1), # make line visible in legend 
                              direction = "horizontal",
                              keywidth = unit(3, units = "mm"),
                              keyheight = unit(0.02, units = "mm")
                              )) + 
  #Theme
  theme(
    panel.background = element_rect(fill = "transparent",colour = "transparent"),
    plot.margin = unit(c(0.1,2,0.1,1), "cm"),
    plot.title = element_text(family = "Times New Roman", size = 1, hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    axis.title=element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'left',
    legend.title=element_text(family = "Times New Roman", face="bold", size=3),
    legend.text=element_text(family = "Times New Roman", face="bold", size=4),
    legend.background = element_rect(fill = "transparent")
  )

print(chart7, vp=view6)

view1 <- viewport(width=0.465,height=0.25,x=0.782,y=0.85)

chart2 <- ggplot(data1,aes(month,mean))+ geom_point(size = 0.5) + geom_line(size=0.3) +
        scale_y_continuous(limits=c(225, 300), sec.axis = sec_axis( ~ ((. - 225)/(300 - 225) * 5 - 2.5), labels=scaleFUN, name = "Normalized HR")) +
        geom_line(data=data_exp_L1_avg, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), alpha = 1, size=0.6, color="green") + 
        geom_line(data=data_Konings_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), alpha = 1, size=0.6, color="red") + 
        geom_line(data=data_TaiESM1_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorCPM1_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CMCC_CM2_SR5_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_WACCM_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CanESM5_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_ESM1_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_CSM2_MR_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_ACCESS_ESM1_5_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_GFDL_ESM4_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_ECA_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM1_2_LR_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM_1_2_HAM_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorESM2_LM_L1, aes(x=Month, y = ((RH + 2.5)/5 * (300 - 225) + 225)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") +
        ggtitle(expression(bold(paste("b)  50", degree, "N", "~", "90", degree, "N", sep = "")))) +
        ylab(expression(paste("MBC (gC m"^"-2", ")"), sep = "")) + 
        xlab("") +
        theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=0.4),
             axis.line = element_line(colour = "black", size = 0.30, linetype = "solid"),
             axis.text.y=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.text.x=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.title.x=element_text(family = "Times New Roman", size = 6),
             axis.title.y=element_text(family = "Times New Roman", size = 6),
             plot.title = element_text(family = "Times New Roman", face="bold", colour="black", size = 5, hjust = 0, vjust = -2),
             plot.background=element_rect(I(0),linetype=0),
             panel.background=element_rect(I(0)),
             panel.grid.major=element_line(colour=NA),
             panel.grid.minor=element_line(colour=NA)) +
             scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c(" ", " ", " ", " ", " ", " "), limits = c(1,12))

print(chart2, vp=view1)


view2 <- viewport(width=0.465,height=0.25,x=0.782,y=0.69)

chart3 <- ggplot(data2,aes(month,mean))+ geom_point(size = 0.5) + geom_line(size=0.3) +
        scale_y_continuous(limits=c(80, 100), sec.axis = sec_axis( ~ ((. - 80)/(100 - 80) * 3 - 1.5), labels=scaleFUN, name = "Normalized HR")) +
        geom_line(data=data_exp_L2_avg, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), alpha = 1, size=0.6, color="green") + 
        geom_line(data=data_Konings_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), alpha = 1, size=0.6, color="red") + 
        geom_line(data=data_TaiESM1_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorCPM1_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CMCC_CM2_SR5_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_WACCM_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CanESM5_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_ESM1_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_CSM2_MR_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_ACCESS_ESM1_5_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_GFDL_ESM4_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_ECA_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM1_2_LR_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM_1_2_HAM_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorESM2_LM_L2, aes(x=Month, y = ((RH + 1.5)/3 * (100 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") +
        ggtitle(expression(bold(paste("c)  25", degree, "N", "~", "50", degree, "N", sep = "")))) +
        ylab(expression(paste("MBC (gC m"^"-2", ")"), sep = "")) + 
        xlab("") +
        theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=0.4),
             axis.line = element_line(colour = "black", size = 0.30, linetype = "solid"),
             axis.text.y=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.text.x=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.title.x=element_text(family = "Times New Roman", size = 6),
             axis.title.y=element_text(family = "Times New Roman", size = 6),
             plot.title = element_text(family = "Times New Roman", face="bold", colour="black", size = 5, hjust = 0, vjust = -2),
             plot.background=element_rect(I(0),linetype=0),
             panel.background=element_rect(I(0)),
             panel.grid.major=element_line(colour=NA),
             panel.grid.minor=element_line(colour=NA)) +
             scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c(" ", " ", " ", " ", " ", " "), limits = c(1,12))

print(chart3, vp=view2)


view3 <- viewport(width=0.465,height=0.25,x=0.782,y=0.535)

chart4 <- ggplot(data3,aes(month,mean))+ geom_point(size = 0.5) + geom_line(size=0.3) +
        scale_y_continuous(limits=c(80, 120), sec.axis = sec_axis( ~ ((. - 80)/(120 - 80) * 1.6 - 0.8), labels=scaleFUN, name = "Normalized HR")) +
        geom_line(data=data_exp_L3_avg, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), alpha = 1, size=0.6, color="green") + 
        geom_line(data=data_Konings_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), alpha = 1, size=0.6, color="red") + 
        geom_line(data=data_TaiESM1_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorCPM1_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CMCC_CM2_SR5_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_WACCM_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CanESM5_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_ESM1_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_CSM2_MR_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_ACCESS_ESM1_5_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_GFDL_ESM4_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_ECA_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM1_2_LR_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM_1_2_HAM_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorESM2_LM_L3, aes(x=Month, y = ((RH + 0.8)/1.6 * (120 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") +
        ggtitle(expression(bold(paste("d)  0", degree, " ", "~", "25", degree, "N", sep = "")))) +
        ylab(expression(paste("MBC (gC m"^"-2", ")"), sep = "")) + 
        xlab("") +
        theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=0.4),
             axis.line = element_line(colour = "black", size = 0.30, linetype = "solid"),
             axis.text.y=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.text.x=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.title.x=element_text(family = "Times New Roman", size = 6),
             axis.title.y=element_text(family = "Times New Roman", size = 6),
             plot.title = element_text(family = "Times New Roman", face="bold", colour="black", size = 5, hjust = 0, vjust = -2),
             plot.background=element_rect(I(0),linetype=0),
             panel.background=element_rect(I(0)),
             panel.grid.major=element_line(colour=NA),
             panel.grid.minor=element_line(colour=NA)) +
             scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c(" ", " ", " ", " ", " ", " "), limits = c(1,12))

print(chart4, vp=view3)

view4 <- viewport(width=0.465,height=0.25,x=0.782,y=0.385)

chart5 <- ggplot(data4,aes(month,mean))+ geom_point(size = 0.5) + geom_line(size=0.3) +
        scale_y_continuous(limits=c(80, 130), sec.axis = sec_axis( ~ ((. - 80)/(130 - 80) * 1.6 - 0.8), labels=scaleFUN, name = "Normalized HR")) +
        geom_line(data=data_exp_L4_avg, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), size=0.6, alpha = 1, color="green") + 
        geom_line(data=data_Konings_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), size=0.6, alpha = 1, color="red") + 
        geom_line(data=data_TaiESM1_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorCPM1_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CMCC_CM2_SR5_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_WACCM_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CanESM5_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_ESM1_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_CSM2_MR_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_ACCESS_ESM1_5_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_GFDL_ESM4_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_ECA_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM1_2_LR_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM_1_2_HAM_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorESM2_LM_L4, aes(x=Month, y = ((RH + 0.8)/1.6 * (130 - 80) + 80)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") +
        ggtitle(expression(bold(paste("e)  0", degree, " ", "~", "25", degree, "S", sep = "")))) +
        ylab(expression(paste("MBC (gC m"^"-2", ")"), sep = "")) + 
        xlab("") +
        theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=0.4),
             axis.line = element_line(colour = "black", size = 0.30, linetype = "solid"),
             axis.text.y=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.text.x=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.title.x=element_text(family = "Times New Roman", size = 6),
             axis.title.y=element_text(family = "Times New Roman", size = 6),
             plot.title = element_text(family = "Times New Roman", face="bold", colour="black", size = 5, hjust = 0, vjust = -2),
             plot.background=element_rect(I(0),linetype=0),
             panel.background=element_rect(I(0)),
             panel.grid.major=element_line(colour=NA),
             panel.grid.minor=element_line(colour=NA)) +
             scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c(" ", " ", " ", " ", " ", " "), limits = c(1,12))

print(chart5, vp=view4)

view5 <- viewport(width=0.465,height=0.25,x=0.782,y=0.23)

chart6 <- ggplot(data5,aes(month,mean))+ geom_point(size = 0.5) + geom_line(size=0.3) +
        scale_y_continuous(limits=c(90, 150), sec.axis = sec_axis( ~ ((. - 90)/(150 - 90) * 1 - 0.5), labels=scaleFUN, name = "Normalized HR")) +
        geom_line(data=data_exp_L5_avg, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), size=0.6, alpha = 1, color="green") + 
        geom_line(data=data_Konings_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), size=0.6, alpha = 1, color="red") + 
        geom_line(data=data_TaiESM1_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorCPM1_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CMCC_CM2_SR5_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_WACCM_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CanESM5_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_ESM1_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_CSM2_MR_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_ACCESS_ESM1_5_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_GFDL_ESM4_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_ECA_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM1_2_LR_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM_1_2_HAM_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorESM2_LM_L5, aes(x=Month, y = ((RH + 0.5)/1 * (125 - 75) + 75)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") +
        ggtitle(expression(bold(paste("f)  25", degree, "S", "~", "50", degree, "S", sep = "")))) +
        ylab(expression(paste("MBC (gC m"^"-2", ")"), sep = "")) + 
        xlab("") +
        theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, size=0.4),
             axis.line = element_line(colour = "black", size = 0.30, linetype = "solid"),
             axis.text.y=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.text.x=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.title.x=element_text(family = "Times New Roman", size = 6),
             axis.title.y=element_text(family = "Times New Roman", size = 6),
             plot.title = element_text(family = "Times New Roman", colour="black", size = 5, hjust = 0, vjust = -2),
             plot.background=element_rect(I(0),linetype=0),
             panel.background=element_rect(I(0)),
             panel.grid.major=element_line(colour=NA),
             panel.grid.minor=element_line(colour=NA)) +
             scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c(" ", " ", " ", " ", " ", " "), limits = c(1,12))
print(chart6, vp=view5)

view7 <- viewport(width=0.465,height=0.25,x=0.782,y=0.08)

chart8 <- ggplot(data6,aes(month,mean))+ geom_point(size = 0.5) + geom_line(size=0.3) +
        scale_y_continuous(limits=c(150, 360), sec.axis = sec_axis( ~ ((. - 150)/(360 - 150) * 1 - 0.5), labels=scaleFUN, name = "Normalized HR")) +
#        geom_line(data=data_exp_L6_avg, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), size=0.6, alpha = 1, color="green") + 
#        geom_line(data=data_Konings_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), size=0.6, alpha = 1, color="red") + 
        geom_line(data=data_TaiESM1_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorCPM1_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CMCC_CM2_SR5_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_WACCM_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CESM2_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_CanESM5_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_ESM1_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_BCC_CSM2_MR_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_ACCESS_ESM1_5_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_GFDL_ESM4_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_E3SM_1_1_ECA_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM1_2_LR_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_MPI_ESM_1_2_HAM_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") + 
        geom_line(data=data_NorESM2_LM_L6, aes(x=Month, y = ((RH + 0.5)/1 * (250 - 190) + 190)), linetype = "solid", size=0.3, alpha = 0.4, color="blue") +
        ggtitle(expression(bold(paste("g)  50", degree, "S", "~", "90", degree, "S", sep = "")))) +
        ylab(expression(paste("MBC (gC m"^"-2", ")"), sep = "")) + 
        xlab("") +
        theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, size=0.4),
             axis.line = element_line(colour = "black", size = 0.30, linetype = "solid"),
             axis.text.y=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.text.x=element_text(family = "Times New Roman", colour="black", size = 5),
             axis.title.x=element_text(family = "Times New Roman", size = 6),
             axis.title.y=element_text(family = "Times New Roman", size = 6),
             plot.title = element_text(family = "Times New Roman", colour="black", size = 5, hjust = 0, vjust = -2),
             plot.background=element_rect(I(0),linetype=0),
             panel.background=element_rect(I(0)),
             panel.grid.major=element_line(colour=NA),
             panel.grid.minor=element_line(colour=NA)) +
             scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov"), limits = c(1,12))
print(chart8, vp=view7)

dev.off()

