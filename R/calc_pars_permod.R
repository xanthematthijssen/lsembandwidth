#' Calculates weighted parameters per moderator value
#'
#' @param moderators_test_data moderator values for which parameters have to be estimated
#' @param kernel  kernel
#' @param bandwidth bandwidth
#' @param parameters_train_data dataframe with estimated parameterer in moderator grid
#'
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
                 parameters_train_data){
  # make dataframe for every combination of moderator value given to the function
  # and moderator values in the moderator grid (also known as focal points)
  # moderators are moderator values given to the function
  # focal_points are moderator values in the moderator grid
  cross_df_focalpoints_moderators <-
    purrr::cross_df(list(
      moderators_test_data = moderators_test_data,
      focal_points = unique(parameters_train_data$moderator)
    ))

  # calculate weigths for these combinations
  cross_df_focalpoints_moderators$weights <-
    lsem_kernel_weights(cross_df_focalpoints_moderators$moderators_test_data,
                        cross_df_focalpoints_moderators$focal_points,
                        bw = bandwidth,
                        kernel = kernel)

  # merge in parameter values
  cross_df_focalpoints_moderators <-
    merge(
      cross_df_focalpoints_moderators,
      parameters_train_data,
      by.x = "focal_points",
      by.y = "moderator",
      all.x = T,
      all.y = F,
      sort = F
    )

  # calculate weighted mean of every parameter for every value of the moderator given to the function
  parameters_permoderator <-
    cross_df_focalpoints_moderators %>%
    dplyr::group_by(.data$moderators_test_data, .data$par) %>%
    dplyr::mutate(weighted_estimate = sum(.data$weights * .data$est) / sum(.data$weights)) %>%
    dplyr::slice(1) %>%
    dplyr::select(.data$moderators_test_data,
                  .data$par,
                  .data$lhs,
                  .data$op,
                  .data$rhs,
                  .data$weighted_estimate)


  print(parameters_permoderator)

  return(parameters_permoderator)

}
