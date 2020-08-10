##### load packages #########

library(devtools)
library(testthat)
library(sirt)
library(data.table)
library(tidyverse)
load_all()

###### set constants #########


lavmodel <- "
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F"

moderator_grid <- c(-2, -1.5, -1.0, - 0.5, 0, 0.5, 1, 1.5, 2)

bw_factor <- c(1.1 ,2.0, 3.0)
moderator <-  "moderator"

numbers <- 1:3000

##### analysis ####
data <- fread("I:/Promovendi_Annette/Xanthe/data scriptie/2020-06-09simulation_unbalancedmix.csv")

data_list <- data %>%
  dplyr::group_split(i)
data_list <- data_list[numbers]
names(data_list) <- numbers

bw_list <- data_list %>%
  lapply(., calculate_bws_simulation, bw_factor = bw_factor)

models <- map2_df(data_list, bw_list, test_bandwidths,
                  moderator_name = "moderator",
                  moderator_grid=moderator_grid,
                  lavmodel=lavmodel, .id = "i",
                  statistic = "CV", K = 5, silent = T)

fwrite(models, file = "I:/Promovendi_Annette/Xanthe/data scriptie/2020-06-12CVs_unbalancedmix_equal_small.csv")
