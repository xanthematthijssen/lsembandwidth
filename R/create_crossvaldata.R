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
