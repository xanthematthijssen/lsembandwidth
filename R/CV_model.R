#' Cross validates lavaan model over list with bandwidths and training and test sets
#'
#' @param list  list with bandwidths and training and test sets
#' @param moderator_name name of moderator
#' @param moderator_grid grid for moderator values
#' @param lavmodel lavaan model
#' @param kernel kernel used
#' @param digits number of digits test_data moderator is rounded to, can be lowerd to increase speed
#' @param silent if false print output in the meantime (false = default)
#' @param warningabort whether to stop further calculations when lavaan model fitting produces
#' a warning
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
CV_model <- function(list,
                     moderator_name,
                     moderator_grid,
                     lavmodel,
                     kernel,
                     digits,
                     warningabort = FALSE,
                     silent = FALSE,
                     ...) {


  bandwidth <- list[[2]]
  train_data <- list[[1]][["training"]]
  test_data <- list[[1]][["test"]]

  parameters_train_data <- tryCatch(
    train_lsemmodel(
      data = train_data,
      lavmodel = lavmodel,
      bandwidth = bandwidth,
      moderator_name = moderator_name,
      moderator_grid = moderator_grid,
      ...
    )$parameters,
    error = function(error) {
      if(!silent){print(error)}
      return(NA)
    },
    warning=function(w){
      if(!silent) print(w)
      if(warningabort) return(NA)
    }
  )


 if (!is.data.frame(parameters_train_data)) {
    return(data.frame(
      statistic = "Deviance",
      bandwidth = bandwidth,
      deviance_test_data = NA
    ))
  }

  # round moderator values in test set to reduce calculations
  test_data[,moderator_name] <- round(test_data[,moderator_name], digits= digits)

  unique_moderators_test_data <- unique(test_data[,moderator_name])

  # calculate estimate parameters for every unique moderator value in the test set
  parameters_test_data <- calculate_parameters_permoderator(
    moderators_test_data = unique_moderators_test_data,
    kernel = kernel,
    bandwidth = bandwidth,
    parameters_train_data = parameters_train_data,
    silent = silent
  )
  # calculate RAM matrices for every unique moderator value in the test set
  RAM_matrices_test_data <- parameters_test_data %>%
    dplyr::group_by(.data$moderators_test_data) %>%
    dplyr::group_split() %>%
    purrr::map(calculate_RAM_matrices)

  # calculate loglikelihoods for every observation in the test set
  loglikelihoods_test_data <- calculate_likelihoods(test_data, RAM_matrices_test_data, moderator_name)

  deviance_test_data <- -2*sum(loglikelihoods_test_data)

  return(data.frame(
    statistic = "Deviance",
    bandwidth = bandwidth,
    deviance_test_data = deviance_test_data
  ))
}
