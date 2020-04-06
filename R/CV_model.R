#' Cross validates lavaan model over list with bandwidths and training and test sets
#'
#' @param list  list with bandwidths and training and test sets
#' @param moderator name of moderator
#' @param moderator.grid grid for moderator values
#' @param lavmodel lavaan model
#' @param kernel kernel used
#' @param digits number of digits test_data moderator is rounded to, can be lowerd to increase speed
#' @param ... further arguments to be passed to lavaan::sem or lavaan::lavaan
#'
#' @return dataframe of bandwidths and likelihoods
#' @export
#' @importFrom rlang .data
#'
#' @examples
#'data(simplefactordata)
#'
#'lavmodel <- "
#'         F=~ indicator1 + indicator2 + indicator3 + indicator4
#'         F ~~ 1*F"
#'
#'bandwidthvector <- c(1,2)
#'
#'
#'
#'list <- purrr::cross2(create_crossvaldata(data = simplefactordata, K = 10),
#'    bandwidthvector)
#'
#'CV_model(list = list[[1]], moderator = "moderator",
#'        moderator.grid =c(1:9)/10,
#'        lavmodel = lavmodel, kernel ="gaussian")
#'
#'
CV_model <- function(list, moderator, moderator.grid, lavmodel, kernel, digits, ...){

  bandwidth <- list[[2]]
  train_data <- list[[1]][["training"]]
  test_data <- list[[1]][["test"]]

  df_fitted_parameters <- train_lsemmodel(
    data = train_data,
    lavmodel = lavmodel,
    bandwidth = bandwidth,
    moderator = moderator,
    moderator.grid = moderator.grid,
    ...
  )$parameters

  # round moderator values reduce calculations
  test_data[,moderator] <- round(test_data[,moderator], digits= digits)


  unique_moderators <- unique(test_data[,moderator])

  # calculate estimate parameters for every unique moderator value in the test set
  df_pars_permod <- calc_pars_permod(
    moderators = unique_moderators,
    kernel = kernel,
    bandwidth = bandwidth,
    parameters = df_fitted_parameters
  )
  # calculate RAM matrices for every unique moderator value in the test set
  RAM_list <- df_pars_permod %>%
    dplyr::group_by(.data$sample_mods) %>%
    dplyr::group_split() %>%
    purrr::map(calculate_RAM)

  # calculate loglikelihoods for every observation in the test set
  loglikelihoods <- calculate_likelihoods(test_data, RAM_list, moderator)

  loglikelihood_sum <- sum(loglikelihoods)

  return(data.frame(
    statistic = "CV",
    bandwidth = bandwidth,
    loglikelihood = loglikelihood_sum
  ))
}
