#' Cross validates lavaan model over list with bandwidths and training and test sets
#'
#' @param list  list with bandwidths and training and test sets
#' @param moderator_name name of moderator
#' @param moderator_grid grid for moderator values
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
#'CV_model(list = list[[1]], moderator_name = "moderator",
#'        moderator_grid =c(1:9)/10,
#'        lavmodel = lavmodel, kernel ="gaussian")
#'
#'
CV_model <- function(list, moderator_name, moderator_grid, lavmodel, kernel, digits, ...){

  bandwidth <- list[[2]]
  train_data <- list[[1]][["training"]]
  test_data <- list[[1]][["test"]]

  parameters_train_data <- train_lsemmodel(
    data = train_data,
    lavmodel = lavmodel,
    bandwidth = bandwidth,
    moderator_name = moderator_name,
    moderator_grid = moderator_grid,
    ...
  )$parameters

  # round moderator values in test set to reduce calculations
  test_data[,moderator_name] <- round(test_data[,moderator_name], digits= digits)

  unique_moderators_test_data <- unique(test_data[,moderator_name])

  # calculate estimate parameters for every unique moderator value in the test set
  parameters_test_data <- calculate_parameters_permoderator(
    moderators_test_data = unique_moderators_test_data,
    kernel = kernel,
    bandwidth = bandwidth,
    parameters_train_data = parameters_train_data
  )
  # calculate RAM matrices for every unique moderator value in the test set
  RAM_matrices_test_data <- parameters_test_data %>%
    dplyr::group_by(.data$moderators_test_data) %>%
    dplyr::group_split() %>%
    purrr::map(calculate_RAM_matrices)

  # calculate loglikelihoods for every observation in the test set
  loglikelihoods_test_data <- calculate_likelihoods(test_data, RAM_matrices_test_data, moderator_name)

  loglikelihood_sum_test_data <- sum(loglikelihoods_test_data)

  return(data.frame(
    statistic = "CV",
    bandwidth = bandwidth,
    loglikelihood = loglikelihood_sum_test_data
  ))
}
