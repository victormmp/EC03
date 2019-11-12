library(ExpDE)
library(smoof)
library(pracma)
library(dplyr)

# Lê diferentes CSVs e gera dataframes
conf_1 <- read.csv(file='conf_45_1_Matheus.csv', sep=',')
conf_2<- read.csv(file='conf_45_2_Matheus.csv', sep=',')

dataConf1 <- matrix(as.numeric(unlist(conf_1[,2])),nrow=nrow(conf_1))
dataConf2 <- matrix(as.numeric(unlist(conf_2[,2])),nrow=nrow(conf_2))

conf_1 <- read.csv(file='conf_45_1_Victor.csv', sep=',')
conf_2<- read.csv(file='conf_45_2_Victor.csv', sep=',')

dataConf1V <- matrix(as.numeric(unlist(conf_1[,2])),nrow=nrow(conf_1))
dataConf2V <- matrix(as.numeric(unlist(conf_2[,2])),nrow=nrow(conf_2))

dataConf1 <- cbind(dataConf1, dataConf1V)
dataConf2 <- cbind(dataConf2, dataConf2V)

conf_1 <- read.csv(file='conf_45_1_Mayra.csv', sep=',')
conf_2<- read.csv(file='conf_45_2_Mayra.csv', sep=',')

dataConf1V <- matrix(as.numeric(unlist(conf_1[,2])),nrow=nrow(conf_1))
dataConf2V <- matrix(as.numeric(unlist(conf_2[,2])),nrow=nrow(conf_2))

dataConf1 <- cbind(dataConf1, dataConf1V)
dataConf2 <- cbind(dataConf2, dataConf2V)

difs <- c()
all_conf_1 <- c()
all_conf_2 <- c()
for (i  in seq(1, nrow(dataConf1), 10)) {

    samples1 <- as.vector(dataConf1[i:(i+9),])
    samples2 <- as.vector(dataConf2[i:(i+9),])
    all_conf_1 <- c(all_conf_1, mean(samples1))
    all_conf_2 <- c(all_conf_2, mean(samples2))
    dif <- mean(samples1) - mean(samples2)
    difs <- c(difs, dif)
}

# Regenerate CSV
conf1 <- as.vector(all_conf_1)
conf2 <- as.vector(all_conf_2)
confs <- cbind(conf1, conf2)

sh <- sample(seq(1:45))
conf1 = conf1[sh]
conf2 = conf2[sh]

cdf1 <- data.frame(conf1)
colnames(cdf1) <- c('best')
cdf2 <- data.frame(conf2)
colnames(cdf2) <- c('best')

write.csv(cdf1, 'conf_1.csv', row.names=FALSE)
write.csv(cdf2, 'conf_2.csv', row.names=FALSE)



