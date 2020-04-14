#' Calculates RAM matrices from dataframe of fittted values
#'
#'
#' @param parameters_onemoderator df of estimated parameters of lsem model for one value of the moderator
#' @return list:
#' value_moderator = value of moderator for the matrices;
#' expected_means = estimated means of multivariate normal;
#' matrix_covariance = estimated covariance of multivariate normal;
#' ordered_variablenames = sequence variable names in which all matrices / vectors are ordered;
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%


calculate_RAM_matrices <- function(parameters_onemoderator) {
  ordered_variablenames <- unique(c(parameters_onemoderator$lhs, parameters_onemoderator$rhs))
  ordered_variablenames <- ordered_variablenames[ordered_variablenames != ""]

  n_vars <- length(ordered_variablenames)

  #input dataframe is grouped, so ungroup it to avoid errors
  parameters_onemoderator <- dplyr::ungroup(parameters_onemoderator)

  # split up data frame in types
  intercepts <- parameters_onemoderator %>% dplyr::filter(.data$op == "~1")
  psis <- parameters_onemoderator %>% dplyr::filter(.data$op == "~~")
  lambdas <- parameters_onemoderator %>% dplyr::filter(.data$op == "=~")
  betas <- parameters_onemoderator %>% dplyr::filter(.data$op == "~")

  moderator_value <- parameters_onemoderator$moderators_test_data[1]

  # make intercept vector (Theta)
  ramvector_theta <- intercepts$weighted_estimate
  names(ramvector_theta) <- intercepts$lhs
  ramvector_theta <- ramvector_theta[ordered_variablenames]
  ramvector_theta[is.na(ramvector_theta)] <- 0
  names(ramvector_theta) <- ordered_variablenames



  rammatrix_beta <-
    make_full_rammatrix(rbind(lambdas, betas),
                     ordered_variablenames = ordered_variablenames)

  # (co-)variance matrix (Eta)
  psis <-
    rbind(psis,
          psis %>%
            dplyr::mutate(hs = .data$lhs,
                          lhs = .data$rhs,
                          rhs = .data$hs) %>%
            dplyr::select(-.data$hs))
  psis <- psis[!duplicated(psis),]

  rammatrix_eta <-
    make_full_rammatrix(psis, ordered_variablenames = ordered_variablenames)



  # combine into multivariate normal covariance matrix and expected value vector
  # if (diag(n_vars) - rammatrix_beta) is invertable
  if (det(diag(n_vars) - rammatrix_beta) > 0) {
    expected_covariance <-
      solve(diag(n_vars) - rammatrix_beta) %*%
      rammatrix_eta %*%
      t(solve(diag(n_vars) - rammatrix_beta))

    expected_means <-
      solve(diag(n_vars) - rammatrix_beta) %*%
      ramvector_theta
  } else {
    cat("diag minus beta matrix is not invertable")
  }

  # filtering will happen later

  return(list(
    moderator_value = moderator_value,
    expected_means = expected_means,
    ordered_variablenames = ordered_variablenames,
    expected_covariance = expected_covariance
  ))
}
