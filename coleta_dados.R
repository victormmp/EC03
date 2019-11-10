rm(list=ls())

library(ExpDE)
library(smoof)
library(pracma)
library(CAISEr)

# result <- power.t.test(delta=0.5,
#              sig.level=0.05,
#              power=0.8,
#              type='paired',
#              alternative='two.sided')

n_instancias <- 34

dimensions <- linspace(2, 150, n_instancias)

out_dim.conf1 = data.frame(matrix(ncol = 2, nrow = 0))
colnames(out_dim.conf1) <- c("dim", "best")
out_dim.conf2 = data.frame(matrix(ncol = 2, nrow = 0))
colnames(out_dim.conf2) <- c("dim", "best")


count.dim <- 1
for (d in dimensions){

    dim <- ceil(d)

    cat("\nBuilding dimension ", dim)

    # Defining Function
    fn <- function(X){
        if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
        Y <- apply(X, MARGIN = 1,
                   FUN = smoof::makeRosenbrockFunction(dimensions = dim))
        return(Y)
    }

    # Building ExpDE parameters
    selpars <- list(name = "selection_standard")
    stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
    probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
    popsize = 5 * dim

    ## Config 1 for ExpDE
    recpars1 <- list(name = "recombination_arith")
    mutpars1 <- list(name = "mutation_rand", f = 4)
    ## Config 2 for ExpDE
    recpars2 <- list(name = "recombination_bin", cr = 0.7)
    mutpars2 <- list(name = "mutation_best", f = 3)

    # Calculating the number of repetitions for each algorithm

    alg1 <- list(FUN='ExpDE',
                 alias='conf1',
                 mutpars = mutpars1,
                 recpars = recpars1,
                 popsize = popsize,
                 selpars = selpars,
                 stopcrit = stopcrit,
                 showpars = list(show.iters = "dots", showevery = 20))

    alg2 <- list(FUN='ExpDE',
                 alias='conf2',
                 mutpars = mutpars2,
                 recpars = recpars2,
                 popsize = popsize,
                 selpars = selpars,
                 stopcrit = stopcrit,
                 showpars = list(show.iters = "dots", showevery = 20))

    instance <- list(FUN=function(name){ return (list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))) },
                     alias='probpars',
                     name='fn')

    my.reps <- calc_nreps(instance=instance,
                          algorithms=list(conf1 = alg1,
                                          conf2 = alg2),
                          se.max=0.05,
                          dif='perc',
                          comparisons='all.vs.all')

    print(my.reps)


    out <- ExpDE(mutpars = mutpars1,
                 recpars = recpars1,
                 popsize = popsize,
                 selpars = selpars,
                 stopcrit = stopcrit,
                 probpars = probpars,
                 showpars = list(show.iters = "dots", showevery = 20))
    de <- list("dim"=dim, "best"=out$Fbest)
    out_dim.conf1 <- rbind(out_dim.conf1,de, stringsAsFactors=FALSE)

    out <- ExpDE(mutpars = mutpars2,
                 recpars = recpars2,
                 popsize = popsize,
                 selpars = selpars,
                 stopcrit = stopcrit,
                 probpars = probpars,
                 showpars = list(show.iters = "dots", showevery = 20))
    de <- list("dim"=dim, "best"=out$Fbest)
    out_dim.conf2 <- rbind(out_dim.conf1,de, stringsAsFactors=FALSE)

    count.dim <- count.dim + 1
}

write.csv(out_dim.conf1, 'conf_1.csv', row.names=FALSE)
write.csv(out_dim.conf2, 'conf_2.csv', row.names=FALSE)




