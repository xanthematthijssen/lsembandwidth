##### parameters #####
n <- 1000

#### simulatie ####
#### init ####
df <- data.frame(patnr = 1:n)

#### latent ####
df$true_latent <- rnorm(n)

#### moderator ####
df$moderator  <- runif(n)

#### loadings ####
loading1 <- 1
loading2 <- 0.5
loading3 <- 2
loading4 <- 1

#### intercept ####
intercept1 <- 0
intercept2 <- 1
intercept3 <- -1
intercept4 <- 0

#### mv #### The slope of the loadinge to the moderator
m_slope1 <- 0
m_slope2 <- 0
m_slope3 <- 0.5
m_slope4 <- 1

#### indicators ####
df$indicator1 <- rnorm(n, mean = df$true_latent*(loading1+m_slope1*moderator) + intercept1)
df$indicator2 <- rnorm(n, mean = df$true_latent*(loading2+m_slope2*moderator) + intercept2)
df$indicator3 <- rnorm(n, mean = df$true_latent*(loading3+m_slope3*moderator) + intercept3)
df$indicator4 <- rnorm(n, mean = df$true_latent*(loading4+m_slope4*moderator) + intercept4)

