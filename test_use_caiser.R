library(CAISEr)

Ncalc <- calc_instances(
    power=0.8,
    d=0.5,
    sig.level=0.05,
    alternative.side='two.sided',
    test='t.test',
    ncomparisons=1
)
Ncalc$ninstances
