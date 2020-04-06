#' Calculates RAM matrices from dataframe of fittted values
#'
#'
#' @param df_pars_onemod df of estimated parameters of lsem model for one value of the moderator
#' @return list:
#' value_moderator = value of moderator for the matrices;
#' vector_means = estimated means of multivariate normal;
#' matrix_covariance = estimated covariance of multivariate normal;
#' vector_variablenames = sequence variable names in which all matrices / vectors are ordered;
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%


calculate_RAM <- function(df_pars_onemod) {
  vector_variablenames <- unique(c(df_pars_onemod$lhs, df_pars_onemod$rhs))
  vector_variablenames <- vector_variablenames[vector_variablenames != ""]

  n_vars <- length(vector_variablenames)

  #input dataframe is grouped, so ungroup it to avoid errors
  df_pars_onemod <- dplyr::ungroup(df_pars_onemod)

  # split up data frame in types
  fitted_intercepts <- df_pars_onemod %>% dplyr::filter(.data$op == "~1")
  fitted_covariances <- df_pars_onemod %>% dplyr::filter(.data$op == "~~")
  fitted_loadings <- df_pars_onemod %>% dplyr::filter(.data$op == "=~")
  fitted_betas <- df_pars_onemod %>% dplyr::filter(.data$op == "~")

  value_moderator <- df_pars_onemod$sample_mods[1]

  # make intercept vector (Theta)
  vector_intercepts <- fitted_intercepts$weighted_est
  names(vector_intercepts) <- fitted_intercepts$lhs
  vector_intercepts <- vector_intercepts[vector_variablenames]
  vector_intercepts[is.na(vector_intercepts)] <- 0
  names(vector_intercepts) <- vector_variablenames

  # make regression and loading matrix (Beta)
  matrix_beta <-
    make_full_matrix(rbind(fitted_loadings, fitted_betas),
                     vector_variablenames = vector_variablenames)

  # (co-)variance matrix (Eta)
  fitted_covariances <-
    rbind(fitted_covariances,
          fitted_covariances %>%
            dplyr::mutate(hs = .data$lhs,
                          lhs = .data$rhs,
                          rhs = .data$hs) %>%
            dplyr::select(-.data$hs))
  fitted_covariances <- fitted_covariances[!duplicated(fitted_covariances),]

  matrix_additional_covariance <-
    make_full_matrix(fitted_covariances, vector_variablenames = vector_variablenames)



  # combine into multivariate normal covariance matrix and expected value vector
  # if (diag(n_vars) - matrix_beta) is invertable
  if (det(diag(n_vars) - matrix_beta) > 0) {
    matrix_covariance <-
      solve(diag(n_vars) - matrix_beta) %*%
      matrix_additional_covariance %*%
      t(solve(diag(n_vars) - matrix_beta))

    vector_means <-
      solve(diag(n_vars) - matrix_beta) %*%
      vector_intercepts
  } else {
    cat("diag minus beta matrix is not invertable")
  }


  return(list(
    value_moderator = value_moderator,
    vector_means = vector_means,
    vector_variablenames = vector_variablenames,
    matrix_covariance = matrix_covariance
  ))
}
