

#' Calculates weighted parameters per moderator value
#'
#' @param moderators moderator values for which parameters have to be estimated
#' @param kernel  kernel
#' @param bandwidth bandwidth
#' @param parameters dataframe with estimated parameters for moderators in moderator grid
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
#'model <- train_lsemmodel( simplefactordata, moderator="moderator",
#' moderator.grid=c(1:9)/10,
#'lavmodel=lavmodel, bandwidth =2)
#'
#'calc_pars_permod(moderators =  unique(simplefactordata$moderator),
#' bandwidth = 0.2, parameters = model$parameters, kernel = "gaussian" )
#'
#'
#'
calc_pars_permod <- function(moderators,
                 kernel,
                 bandwidth,
                 parameters){
  # make dataframe for every combination of moderator value given to the function
  # and moderator values in the moderator grid (also known as focal points)
  # sample_mods are moderator values given to the function
  # fitted_mods are moderator values in the moderator grid
  df_mods <-
    purrr::cross_df(list(
      sample_mods = moderators,
      fitted_mods = unique(parameters$moderator)
    ))

  # calculate weigths for these combinations
  df_mods$weights <-
    lsem_kernel_weights(df_mods$sample_mods,
                        df_mods$fitted_mods,
                        bw = bandwidth,
                        kernel = kernel)

  # merge in parameter values
  df_mods <-
    merge(
      df_mods,
      parameters,
      by.x = "fitted_mods",
      by.y = "moderator",
      all.x = T,
      all.y = F,
      sort = F
    )

  # calculate weighted mean of every parameter for every value of the moderator given to the function
  df_pars_permod <-
    df_mods %>%
    dplyr::group_by(.data$sample_mods, .data$par) %>%
    dplyr::mutate(weighted_est = sum(.data$weights * .data$est) / sum(.data$weights)) %>%
    dplyr::slice(1) %>%
    dplyr::select(.data$sample_mods,
                  .data$par,
                  .data$lhs,
                  .data$op,
                  .data$rhs,
                  .data$weighted_est)


  print(df_pars_permod)

  return(df_pars_permod)

}
