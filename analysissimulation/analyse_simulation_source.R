### load packages ###
library(devtools)
library(testthat)
library(sirt)
library(tidyverse)
library(data.table)
load_all()

### load functions ###
fit_bandwidths <- function(data, bw_vector){
  model_list <- list()
  for(i in 1:length(bw_vector)){
    bw <- bw_vector[i]
    model_list[[i]] <-train_lsemmodel( data = data, moderator_name = "moderator",
                                       moderator_grid=moderator_grid,
                                       lavmodel=lavmodel, bandwidth =bw)$parameters

  }
  names(model_list) <- 1:7
  model_df <- do.call(rbind, model_list)
  model_df$bw <-rownames(model_df)
  return(model_df)
}


calulate_bws <- function(data){
  n <- nrow(data)
  sd <- sd(data$moderator)
  bw <- calculate_bandwidth_hildebrandt(n = n, sd = sd, h = bw_factor)
  return(bw)
}


##### set constants ######

lavmodel <- "
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F"

moderator_grid <- c(-2, -1.5, -1.0, - 0.5, 0, 0.5, 1, 1.5, 2)

bw_factor <- c(1.1 , 1.5, 2.0, 2.5, 3.0, 4.0, 5.0)
moderator <-  "moderator"

B <- 1000

