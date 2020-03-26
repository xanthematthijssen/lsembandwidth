library(devtools)
library(testthat)
library(sirt)
sessionInfo()
.rs.restartR()
rm(list = ls())

load_all()
check()
document()
##### testing #####
devtools::test()
devtools::test_coverage()

source("tools/first_simulation.R")
#create_crossvaldata(data=df, K = 5)


#### lsem model ####

model <- train_lsemmodel( simplefactordata, moderator="moderator",
                             moderator.grid=c(1:9)/10,
                             lavmodel=lavmodel, bandwidth =2,
                          par_invariant =c("indicator3~1"))

model$parameters
summary(model)
plot(model, parindex=1:4, ask = F)
B <- 50
pmod <- sirt::lsem.permutationTest( model, B=B)
summary(pmod)
#####  build ignore#####
usethis::use_build_ignore(c("R_drabbels"))

#### MIT license ######
use_mit_license("Xanthe Matthijssen")

data("simplefactordata")
moderator.grid <- 1:9/10
lavmodel <- "
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F"


test_bandwidths(data = simplefactordata, lavmodel = lavmodel,
                bandwidthvector = c(1,2,3), moderator = "moderator",
                moderator.grid = moderator.grid)


test_bandwidths(data = simplefactordata, lavmodel = lavmodel,
                bandwidthvector = c(1,2,3), moderator = "moderator",
                moderator.grid = moderator.grid, statistic  = "CV")
