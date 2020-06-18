#' Calculates weighted parameters per moderator value
#'
#' @param moderators_test_data moderator values for which parameters have to be estimated
#' @param kernel  kernel
#' @param bandwidth bandwidth
#' @param parameters_train_data dataframe with estimated parameterer in moderator grid
#' @param silent if false print output in the meantime (false = default)
#' @return dataframe with parameter values per moderator value
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#'
#'data(simplefactordata)
#'
#'lavmodel <- "
#'         F=~ indicator1 + indicator2 + indicator3 + indicator4
#'         F ~~ 1*F"
#'
#'model <- train_lsemmodel( simplefactordata, moderator_name="moderator",
#' moderator_grid=c(1:9)/10,
#'lavmodel=lavmodel, bandwidth =2)
#'
#'calculate_parameters_permoderator(moderators_test_data =  unique(simplefactordata$moderator),
#' bandwidth = 0.2, parameters_train_data = model$parameters, kernel = "gaussian" )
#'
#'
#'
calculate_parameters_permoderator <- function(moderators_test_data,
                 kernel,
                 bandwidth,
                 parameters_train_data,
                 silent = FALSE){
  # make dataframe for every combination of moderator value given to the function
  # and moderator values in the moderator grid (also known as focal points)
  # moderators are moderator values given to the function
  # focal_points are moderator values in the moderator grid

  parameters_train_data$par <- as.character(parameters_train_data$par)
  parameters_train_data_list <- dplyr::group_split(parameters_train_data, par)

  parnames <- sapply(parameters_train_data_list, function(x) x$par[1])

  names(parameters_train_data_list) <- parnames

  intrapolation_funs <- purrr::map(
    parameters_train_data_list, ~
    stats::splinefun(
      x = .$moderator,
      y = .$est,
      method = "natural"
    )
  )
  if(is.data.frame(moderators_test_data)){moderators_test_data <- moderators_test_data$moderator}
  names(intrapolation_funs) <- parnames
  parameter_df <- purrr::map_df(intrapolation_funs, exec, moderators_test_data)
  parameter_df$moderators_test_data <- moderators_test_data

  parameters_test_data <- tidyr::pivot_longer(parameter_df,
                                              -.data$moderators_test_data,
                                              names_to = "par",
                                              values_to = "weighted_estimate")

  parameters_train_tomerge <-
    parameters_train_data %>%
    subset(moderator == min(moderator)) %>%
    select(par, lhs, op, rhs)


  parameters_permoderator <- merge(parameters_test_data,
                                 parameters_train_tomerge ,
                                 by = "par", all = T)

  if(!silent) {print(parameters_permoderator)}

  return(parameters_permoderator)

}
