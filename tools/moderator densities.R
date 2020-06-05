sd(runif(1000000, min = -2, max = 2))
# 1.26

sd(rnorm(10000000))
hist(rnorm(100000))
# 1.00

sd(c(rnorm(10000000, -1, 0.5), rnorm(10000000, 1, 0.5)))
hist(c(rnorm(100000, -1, 0.5), rnorm(100000, 1, 0.5)))
# 1.12

mean(abs(c(rnorm(100000, -1, 0.5), rnorm(100000, 1, 0.5))) > 2)
mean(abs(c(rnorm(1000000))) > 2)

calculate_bandwidth <- function(n, sd, h){
  bw <- (h* sd )/ n^0.2
  return(bw)
}

calculate_bandwidth(300, c(1.00,1.12,1.26), 3)
calculate_bandwidth(300, c(1.00,1.12,1.26), 2)
calculate_bandwidth(500, c(1.00,1.12,1.26), 2)
calculate_bandwidth(1000, c(1.00,1.12,1.26), 2)
calculate_bandwidth(1000, c(1.00,1.12,1.26), 1.1)
(3:12)/10
