#' This functions makes a matrix with same sequence of colnames and rownames
#' from a dataframe of fitted values
#'
#' @param df_fittedvalues dataframe of fitted values of one type (regression and loadings or (co-)variance)
#' @param vector_variablenames sequence of variable names to ensure compatability of all matrices
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return a matrix with same sequence of colnames and rownames as vector_variablenames
#' and values from the dataframe of fitted values and everything else 0

make_full_matrix <- function(df_fittedvalues, vector_variablenames){
  # when something has no fitted value it is assumed to be 0
  df_allcombinations <- data.frame(sample_mods = -1,
                       par = "test",
                       lhs = rep(vector_variablenames, times = length(vector_variablenames)),
                       op = "test",
                       rhs = rep(vector_variablenames, each = length(vector_variablenames)),
                       weighted_est = 0
  )

  df_fittedvalues <- rbind(df_fittedvalues, df_allcombinations)

  df_fittedvalues$duplicated <-
    duplicated(df_fittedvalues[,c("rhs","lhs")]) |
    duplicated(df_fittedvalues[,c("rhs","lhs")], fromLast = TRUE)

  # remove duplicate combinations due to df_allcombinations
  df_fittedvalues <- dplyr::filter(df_fittedvalues,!(duplicated & .data$par == "test"))
  df_fittedvalues <- dplyr::select(df_fittedvalues,
                                -.data$op,
                                -.data$par,-.data$sample_mods,
                                -.data$duplicated)

  # pivor_wider to make rows and colums the variables and values the fitted values
  matrix <- tidyr::pivot_wider(
    df_fittedvalues,
    names_from = .data$rhs,
    values_from = .data$weighted_est
  )
  matrix <- as.data.frame(matrix)

  # make rownames of first column and remove to end up with only numbers
  rownames(matrix) <- matrix$lhs
  matrix <- dplyr::select(matrix, -.data$lhs)

  # ensure same sequence rownames, colnames and everything
  matrix <- matrix[vector_variablenames,]
  matrix <- matrix[,vector_variablenames]

  matrix <- as.matrix(matrix)

  return(matrix)

}
