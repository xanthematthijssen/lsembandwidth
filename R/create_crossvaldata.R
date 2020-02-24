#' Creates list of dataset from dataset
#'
#' Creates list of datasets for K-fold crossvalidaton
#' labelled as training and test for cross-validation
#'
#' @param data dataframe
#' @param K numeric
#'
#' @return list of training and test sets
#' @export
#'
#' @examples
#' create_crossvaldata(rbind(rnorm(10), runif(10)), 2)
#'
create_crossvaldata <- function(data, K){
  row_vector <- 1:nrow(data)
  row_vector <- sample(row_vector)
  K_vector <- cut(row_vector, K, labels = FALSE)
  data_list <- list()
  for(i in 1:K){
    data_list[[i]] <- list(training = data[K_vector != i,],
                           test = data[K_vector == i,] )
  }
  return(data_list)
}
