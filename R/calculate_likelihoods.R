#' Calculates likelihood for every row of data with list of RAM matrices for
#' every moderator value
#'
#' @param data dataframe for which likelihoods have to be calculated
#' @param RAM_list list of RAM matrices for every moderator value
#' @param moderator name of the moderator in the dataframe
#'
#' @return vector of loglikelihoods for every row of data
#'

calculate_likelihoods <- function(data, RAM_list, moderator){

  values_moderator <- sapply(RAM_list,function(list) list[["value_moderator"]])

  #  extract variables that are observed in right sequence
  observed_variables <- intersect(names(RAM_list[[1]][["vector_means"]]),
                         colnames(data))

  # lists for means and covariances for every moderator value
  list_means <- lapply(RAM_list,function(list) {list[["vector_means"]][observed_variables]})
  list_covs <- lapply(RAM_list,function(list) {list[["matrix_covariance"]][observed_variables, observed_variables]})

  # calculate loglikelihoods
  apply(data, 1, function(x){
    index = which(values_moderator == x[moderator])
    list <- RAM_list[[index]]
    if(!list$value_moderator == x[moderator]) cat("selected wrong list")
    mvtnorm::dmvnorm(x[observed_variables], mean = list_means[[index]], sigma = list_covs[[index]],
            log = TRUE)
  })

}
