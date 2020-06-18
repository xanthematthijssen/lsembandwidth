#' Function to simulate modulated simple factor model with 4 indicators
#'
#' @param type type of distribution of the moderator, can be "rnorm", "mixture",
#' "increasing","unbalanced mixture"
#' @param effect whether the effect of the moderator is "equal" between loading 3 and 4
#' or "ascending"
#' @param N nrow of simulated dataset
#' @param seed seed for simulation
#' @param path where to write the dataset
#' @param i extra variabele to give simulated dataset a number
#'
#' @return simulated dataset
#' @export
#'
#' @examples
#'
#' simulate_data(type = "rnorm", effect = "equal", N = 100, seed = 121212)
simulate_data <- function(type, effect, N, seed, path = NA, i= NA){
  set.seed(seed)

  #### patient nummer ####
  df <- data.frame(patnr = 1:N)

  #### moderator ####
  if(type == "rnorm"){
    df$moderator <- stats::rnorm(N)
  } else if ( type == "mixture"){
    means <- sample(c(-1,1), size = N, replace = T)
    df$moderator <- stats::rnorm(N, mean = means, sd = 0.5)
  } else if(type == "increasing"){
    df$moderator<- extraDistr::rtriang(N, a = -2, b = 2, c = -2)
  } else if ( type == "unbalanced mixture"){
    means <- sample(c(-1,1), size = N, replace = T, prob =c(0.8, 0.2))
    df$moderator <- stats::rnorm(N, mean = means, sd = 0.5)
  } else {
    print("No valid function given")
    return(0)
  }

  #### latent ####
  df$true_latent <- stats::rnorm(N)

  #### loadings ####
  if(effect == "equal"){
    loading1 <- rep(0.8, N)
    loading2 <- rep(0.8, N)
    loading3 <- 0.8 + df$moderator /3
    loading4 <- 0.8 + df$moderator /3
  } else if(effect == "ascending"){
    loading1 <- 0.8
    loading2 <- 0.8
    loading3 <- 0.8 + df$moderator /3
    loading4 <- 0.8 + df$moderator /6
  } else {
    print("No valid effect given")
    return(0)
  }

  #### intercept ####
  intercept1 <- 0
  intercept2 <- 0
  intercept3 <- 0
  intercept4 <- 0


  #### indicators ####
  df$indicator1 <- stats::rnorm(N, mean = df$true_latent*(loading1*df$moderator) +
                           intercept1
                         ,sd = 0.6)
  df$indicator2 <- stats::rnorm(N, mean = df$true_latent*(loading2*df$moderator) +
                           intercept2
                         ,sd = 0.6)
  df$indicator3 <- stats::rnorm(N, mean = df$true_latent*(loading3*df$moderator) +
                           intercept3
                         ,sd = 0.6)
  df$indicator4 <- stats::rnorm(N, mean = df$true_latent*(loading4*df$moderator) +
                           intercept4
                         ,sd = 0.6)

  df$i <- i


  if(!is.na(path)){
    utils::write.csv2(df, file = path)
  }

  return(df)
}
