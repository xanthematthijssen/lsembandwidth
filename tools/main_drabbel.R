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
                bandwidthvector = c(0.1,2,3), moderator = "moderator",
                moderator.grid = moderator.grid, statistic  = "CV", K=3,
                digits = 2)


data("simplefactordata")
moderator.grid <- 1:9/10
lavmodel <- "
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F
        indicator1 ~ indicator2"
train_data = simplefactordata[1:200,]
test_data = simplefactordata[201:400,]
bandwidth = 1
moderator = "moderator"
df_fitted_parameters <- train_lsemmodel(
  data = train_data,
  lavmodel = lavmodel,
  bandwidth = bandwidth,
  moderator = "moderator",
  moderator.grid = moderator.grid)$parameters

unique_moderators <- unique(test_data[,moderator])

df_pars_permod <- calc_pars_permod(
  moderators = unique_moderators,
  kernel = "gaussian",
  bandwidth = bandwidth,
  parameters = df_fitted_parameters
)

df_pars_onemod <- df_pars_permod[df_pars_permod$sample_mods == df_pars_permod$sample_mods[1],]

RAM_list <- df_pars_permod %>%
  dplyr::group_by(.data$sample_mods) %>%
  dplyr::group_split() %>%
  purrr::map(calculate_RAM)



