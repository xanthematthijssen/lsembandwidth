#' This functions makes a matrix with same sequence of colnames and rownames
#' from a dataframe of fitted values
#'
#' @param estimates dataframe of fitted values of one type (regression and loadings or (co-)variance)
#' @param ordered_variablenames sequence of variable names to ensure compatability of all matrices
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return a matrix with same sequence of colnames and rownames as ordered_variablenames
#' and values from the dataframe of fitted values and everything else 0

make_full_rammatrix <- function(estimates, ordered_variablenames){

  # when something has no fitted value it is assumed to be 0
  all_combinations_zero <- data.frame(moderators_test_data = -1,
                       par = "all_combinations_zero",
                       lhs = rep(ordered_variablenames, times = length(ordered_variablenames)),
                       op = "all_combinations_zero",
                       rhs = rep(ordered_variablenames, each = length(ordered_variablenames)),
                       weighted_estimate = 0
  )

  estimates <- rbind(estimates, all_combinations_zero)

  estimates$duplicated <-
    duplicated(estimates[,c("rhs","lhs")]) |
    duplicated(estimates[,c("rhs","lhs")], fromLast = TRUE)

  # remove duplicate combinations due to all_combinations_zero
  estimates <- dplyr::filter(estimates,!(duplicated & .data$par == "all_combinations_zero"))
  estimates <- dplyr::select(estimates,
                                -.data$op,
                                -.data$par,-.data$moderators_test_data,
                                -.data$duplicated)

  # pivor_wider to make rows and colums the variables and values the fitted values
  rammatrix <- tidyr::pivot_wider(
    estimates,
    names_from = .data$rhs,
    values_from = .data$weighted_estimate
  )
  rammatrix <- as.data.frame(rammatrix)

  # make rownames of first column and remove to end up with only numbers
  rownames(rammatrix) <- rammatrix$lhs
  rammatrix <- dplyr::select(rammatrix, -.data$lhs)

  # ensure same sequence rownames, colnames and everything
  rammatrix <- rammatrix[ordered_variablenames,]
  rammatrix <- rammatrix[,ordered_variablenames]

  rammatrix <- as.matrix(rammatrix)

  return(rammatrix)

}
