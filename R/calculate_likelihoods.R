#' Calculates likelihood for every row of data with list of RAM matrices for
#' every moderator value
#'
#' @param data dataframe for which likelihoods have to be calculated
#' @param RAM_list list of RAM matrices for every moderator value
#' @param moderator_name name of the moderator in the dataframe
#'
#' @return vector of loglikelihoods for every row of data
#'

calculate_likelihoods <- function(data, RAM_list, moderator_name){

  moderator_values <- sapply(RAM_list,function(list) list[["moderator_value"]])

  #  extract variables that are observed in right sequence
  observed_variables <- intersect(names(RAM_list[[1]][["expected_means"]]),
                         colnames(data))

  # lists for means and covariances for every moderator value
  list_means <- lapply(RAM_list,function(list) {list[["expected_means"]][observed_variables]})
  list_covs <- lapply(RAM_list,function(list) {list[["expected_covariance"]][observed_variables, observed_variables]})

  # calculate loglikelihoods
  apply(data, 1, function(x){
    index = which(moderator_values == x[moderator_name])
    list <- RAM_list[[index]]
    if(!list$moderator_value == x[moderator_name]) cat("selected wrong list")
    mvtnorm::dmvnorm(x[observed_variables], mean = list_means[[index]], sigma = list_covs[[index]],
            log = TRUE)
  })

}
