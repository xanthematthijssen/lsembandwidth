#' Fits LSEM models with predifined bandwidths and measures fit
#'
#' @param data dataset for model
#' @param lavmodel Specified SEM in lavaan
#' @param bandwidthvector vector of tested bandwidths
#' @param moderator Variable name of the moderator
#' @param moderator.grid Focal points at which the LSEM should be evaluated.
#' @param statistic can be AIC or CV (cross-validated likelihood)
#' @param K number of folds for cross-validation, defaults to 10
#' @param kernel used kernel to weight observations
#' @param digits number of digits test_data moderator is rounded to,
#' can be lowered to increase speed crossvalidation
#' defaults to 6
#' @param ... further arguments to be passed to lavaan::sem or lavaan::lavaan.
#'
#' @return dataframe with fit measures for every bandwidth
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#'
#'data(simplefactordata)
#'
#'lavmodel <- "
#'         F=~ indicator1 + indicator2 + indicator3 + indicator4
#'         F ~~ 1*F"
#'
#' test_bandwidths(simplefactordata,moderator="moderator",moderator.grid=c(1:9)/10,
#' lavmodel=lavmodel, bandwidthvector = c(1,2), statistic = "AIC")
#'
test_bandwidths <- function(data,
                            lavmodel,
                            bandwidthvector,
                            moderator,
                            moderator.grid,
                            statistic = "AIC",
                            K = 10,
                            kernel = "gaussian",
                            digits = 6,
                            ...) {
  if (statistic == "AIC") {
    statistic_vector <-
      bandwidthvector %>% purrr::map_dbl(
             function(x) {
               train_lsemmodel(
                 data = data,
                 lavmodel = lavmodel,
                 bandwidth = x,
                 moderator = "moderator",
                 moderator.grid = moderator.grid,
                 fit_measures = "aic",
                 kernel = kernel,
               )$fitstats_joint$value
             },
             ...)
    df <- data.frame(statistic = statistic,
                     bandwidth = bandwidthvector,
                     value = statistic_vector)
  }

  if (statistic == "CV") {
   df_likelihood <- purrr::map_dfr(purrr::cross2(create_crossvaldata(data = data, K = K),
                                          bandwidthvector),
              CV_model, moderator = moderator, moderator.grid = moderator.grid,
                       lavmodel = lavmodel, kernel = kernel, digits = digits, ...)

   print(df_likelihood)

   df <-
     df_likelihood %>%
     dplyr::group_by(.data$statistic, .data$bandwidth) %>%
     dplyr::summarise(value = sum(.data$loglikelihood))
  }
  return(df)
}


