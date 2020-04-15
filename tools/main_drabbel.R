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
                bandwidthvector = c(0.1,2,3), moderator_name = "moderator",
                moderator_grid = moderator_grid, statistic  = "CV", K=3,
                digits = 2)


data("simplefactordata")
moderator_grid <- 1:9/10
lavmodel <- "
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F
        indicator1 ~ indicator2"
train_data = simplefactordata[1:200,]
test_data = simplefactordata[201:400,]
bandwidth = 1
moderator_name = "moderator"
parameters_train_data <- train_lsemmodel(
  data = train_data,
  lavmodel = lavmodel,
  bandwidth = bandwidth,
  moderator_name  = "moderator",
  moderator_grid = moderator_grid)$parameters

unique_moderators_test_data <- unique(test_data[,moderator_name])

parameters_test_data <- calculate_parameters_permoderator(
  moderators = unique_moderators_test_data,
  kernel = "gaussian",
  bandwidth = bandwidth,
  parameters = parameters_train_data
)

parameters_onemoderator <- parameters_test_data[parameters_test_data$moderators_test_data == parameters_test_data$moderators_test_data[1],]

RAM_list <- parameters_test_data %>%
  dplyr::group_by(.data$moderators_test_data) %>%
  dplyr::group_split() %>%
  purrr::map(calculate_RAM_matrices)



