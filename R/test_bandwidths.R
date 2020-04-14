#' Fits LSEM models with predifined bandwidths and measures fit
#'
#' @param data dataset for model
#' @param lavmodel Specified SEM in lavaan
#' @param bandwidthvector vector of tested bandwidths
#' @param moderator_name Variable name of the moderator
#' @param moderator_grid Focal points at which the LSEM should be evaluated.
#' @param statistic can be AIC or CV (cross-validated likelihood)
#' @param K number of folds for cross-validation, defaults to 10
#' @param kernel used kernel to weight observations
#' @param digits number of digits test_data moderator is rounded to,
#' can be lowered to increase speed crossvalidation
#' defaults to 6
#' @param maxbandwidthdistance the amount of bandwidths from every point in the
#' moderator grid that a point in the dataset must be present
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
#' test_bandwidths(simplefactordata,moderator_name="moderator",moderator_grid=c(1:9)/10,
#' lavmodel=lavmodel, bandwidthvector = c(1,2), statistic = "AIC")
#'
test_bandwidths <- function(data,
                            lavmodel,
                            bandwidthvector,
                            moderator_name,
                            moderator_grid,
                            statistic = "AIC",
                            K = 10,
                            kernel = "gaussian",
                            digits = 6,
                            maxbandwidthdistance = 2,
                            ...) {

  # if all points in dataset too far from point in moderator grid return error
  if (max(sapply(moderator_grid, function(x)
    min(abs((x - data[, moderator_name]) / min(bandwidthvector)
    ),
    na.rm = T))) > maxbandwidthdistance
  ){
    cat("datapoints too far away from certain points in moderator grid \n")
    cat("remove point from moderator grid, increase bandwidth \n")
    cat("or increase maxbandwidthdisance \n")
    return("ended with error")
  }

  if (statistic == "AIC") {
    statistic_vector <-
      bandwidthvector %>% purrr::map_dbl(function(x) {
        train_lsemmodel(
          data = data,
          lavmodel = lavmodel,
          bandwidth = x,
          moderator_name = "moderator",
          moderator_grid = moderator_grid,
          fit_measures = "aic",
          kernel = kernel,
        )$fitstats_joint$value
      },
      ...)

    df_statistics <- data.frame(statistic = statistic,
                                bandwidth = bandwidthvector,
                                value = statistic_vector)
  }

  if (statistic == "CV") {
    # calculate loglikelihoods for every test/training combination for every
    # bandwidht
    df_loglikelihood <-
      purrr::map_dfr(
        purrr::cross2(create_crossvaldata(data = data, K = K),
                      bandwidthvector),
        CV_model,
        moderator_name = moderator_name,
        moderator_grid = moderator_grid,
        lavmodel = lavmodel,
        kernel = kernel,
        digits = digits,
        ...
      )

    print(df_loglikelihood)

    # sum loglikelihoods of cross-validation sets to get statistic per bandwidth
    df_statistics <-
      df_loglikelihood %>%
      dplyr::group_by(.data$statistic, .data$bandwidth) %>%
      dplyr::summarise(value = sum(.data$loglikelihood))
  }
  return(df_statistics)
}
