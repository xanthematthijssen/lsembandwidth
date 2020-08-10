##### load packages #########

library(devtools)
library(testthat)
library(sirt)
library(data.table)
library(tidyverse)
load_all()

###### set constants #########
set.seed(121212)


lavmodel <- "
        F=~ 1*indicator1 + indicator2 + indicator3 + indicator4
        indicator1 ~ 0
        indicator2 ~ 1
        indicator3 ~ 1
        indicator4 ~ 1
        F ~ 1"

moderator_grid <- c(-2, -1.5, -1.0, - 0.5, 0, 0.5, 1, 1.5, 2)

bw_factor <- c(1.1 , 1.5, 2.0, 2.5, 3.0, 4.0, 5.0)
moderator <-  "moderator"

numbers <- 1:6000

##### analysis ####
data <- fread("I:/Pi helm/Promovendi/Xanthe/data scriptie/2020-06-09simulation_unbalancedmix.csv")

data_list <- data %>%
  dplyr::group_split(i)
data_list <- data_list[numbers]
names(data_list) <- numbers

bw_list <- data_list %>%
  lapply(., calculate_bws_simulation, bw_factor = bw_factor)

models <- map2_df(data_list, bw_list, fit_bandwidths_simulation,
                  moderator_name = "moderator",
                  moderator_grid=moderator_grid,
                  lavmodel=lavmodel, .id = "i")

fwrite(models, file = "I:/Pi helm/Promovendi/Xanthe/data scriptie/2020-06-30models_unbalancedmix.csv")
