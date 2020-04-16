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
  if(sum(is.na(moderator_values))> 0) print("missing moderator values, please exclude")

  #  extract variables that are observed in right sequence
  observed_variables <- intersect(colnames(RAM_list[[1]][["expected_covariance"]]),
                         colnames(data))

  # lists for means and covariances for every moderator value
  list_means <- lapply(RAM_list,function(list) {list[["expected_means"]][observed_variables,]})
  list_covs <- lapply(RAM_list,function(list) {list[["expected_covariance"]][observed_variables, observed_variables]})

  # calculate loglikelihoods
  apply(data, 1, function(x){
    nonmissing_variables <- observed_variables[!is.na(x[observed_variables])]
    index <- c(1:length(moderator_values))[moderator_values == as.numeric(x[moderator_name])]
    # here we use that the marginal density of the joint Gaussian is gaussian
    # http://cs229.stanford.edu/section/more_on_gaussians.pdf
    if(length(nonmissing_variables) == 0) {
      cat("observation with onlymissing values")
      return(0)
    } else if (length(nonmissing_variables) == 1) {
      return(stats::dnorm(
        x = as.numeric(x[nonmissing_variables]),
        mean = list_means[[index]][nonmissing_variables],
        sd = sqrt(list_covs[[index]][nonmissing_variables, nonmissing_variables]),
        log = TRUE
      ))
    } else {
      return(mvtnorm::dmvnorm(as.numeric(x[nonmissing_variables]),
                              mean = list_means[[index]][nonmissing_variables],
                              sigma = list_covs[[index]][nonmissing_variables, nonmissing_variables],
                              log = TRUE))
    }



  })

}
