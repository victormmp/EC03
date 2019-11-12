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

