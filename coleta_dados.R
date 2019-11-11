rm(list=ls())

library(ExpDE)
library(smoof)
library(pracma)
library(dplyr)

# result <- power.t.test(delta=0.5,
#              sig.level=0.05,
#              power=0.8,
#              type='paired',
#              alternative='two.sided')

n_instancias <- 45

dimensions <- linspace(2, 150, n_instancias)

out_dim.conf1 = data.frame(matrix(ncol = 2, nrow = 0))
colnames(out_dim.conf1) <- c("dim", "best")
out_dim.conf2 = data.frame(matrix(ncol = 2, nrow = 0))
colnames(out_dim.conf2) <- c("dim", "best")

count.dim <- 1
for (d in dimensions){

    dim <- ceil(d)

    for (r in 1:10) {


        cat("\nBuilding dimension ", dim)

        fn <- function(X){
            if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
            Y <- apply(X, MARGIN = 1,
                       FUN = smoof::makeRosenbrockFunction(dimensions = dim))
            return(Y)
        }
        selpars <- list(name = "selection_standard")
        stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
        probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
        popsize = 5 * dim

        ## Config 1
        recpars1 <- list(name = "recombination_arith")
        mutpars1 <- list(name = "mutation_rand", f = 4)
        ## Config 2
        recpars2 <- list(name = "recombination_bin", cr = 0.7)
        mutpars2 <- list(name = "mutation_best", f = 3)

        out <- ExpDE(mutpars = mutpars1,
                     recpars = recpars1,
                     popsize = popsize,
                     selpars = selpars,
                     stopcrit = stopcrit,
                     probpars = probpars,
                     showpars = list(show.iters = "dots", showevery = 20))
        de <- list("dim"=dim, "best"=out$Fbest, "repetition"=r)
        out_dim.conf1 <- rbind(out_dim.conf1,de, stringsAsFactors=FALSE)

        out <- ExpDE(mutpars = mutpars2,
                     recpars = recpars2,
                     popsize = popsize,
                     selpars = selpars,
                     stopcrit = stopcrit,
                     probpars = probpars,
                     showpars = list(show.iters = "dots", showevery = 20))
        de <- list("dim"=dim, "best"=out$Fbest, "repetition"=r)
        out_dim.conf2 <- rbind(out_dim.conf2,de, stringsAsFactors=FALSE)

    }

    count.dim <- count.dim + 1
}

write.csv(out_dim.conf1, 'conf_45_1.csv', row.names=FALSE)
write.csv(out_dim.conf2, 'conf_45_2.csv', row.names=FALSE)

norm <- function(x) {
    result <- shapiro.test(x)
    return (result$p-value)
}

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

for (i  in seq(11, nrow(dataConf1), 10)) {

    samples1 <- as.vector(dataConf1[i:(i+9),])
    samples2 <- as.vector(dataConf2[i:(i+9),])
    samples <- mean(samples1) - mean(samples2)
    result <- shapiro.test(samples)$p.value

    if (result < 0.05) {
        print(paste('Amostra', i,'com p-valor', result))
    }
}

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

