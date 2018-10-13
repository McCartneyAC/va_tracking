setwd("~/Desktop/UVA PhD/Spring 2018/Quant I")
source("stat-functions (1).R")

gs<-read.csv("GovSchoolStats.csv")

plot(x=gs$d_index, y=gs$GS.Rate)
cor(x=gs$d_index, y=gs$GS.Rate)

