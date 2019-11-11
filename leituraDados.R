rm(list=ls())

conf_1 <- read.csv(file='conf_1.csv', sep=',')
conf_2<- read.csv(file='conf_2.csv', sep=',')

dataConf1 <- matrix(as.numeric(unlist(conf_1[,2])),nrow=nrow(conf_1))
dataConf2 <- matrix(as.numeric(unlist(conf_2[,2])),nrow=nrow(conf_2))

dataA <- replicate(34, 0)
dataB <- replicate(34, 0)
for (i in 1:34) {
  dataA[i] <- mean(dataConf1[10*i-1:10*i,])
  dataB[i] <- mean(dataConf2[10*i-1:10*i,])
}

difference <- dataB-dataA

shapiro.test(difference)

qqPlot(difference)

library(CAISEr) # Load package