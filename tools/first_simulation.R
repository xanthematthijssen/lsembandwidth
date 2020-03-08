##### parameters #####
n <- 1000

#### simulatie ####
#### init ####
df <- data.frame(patnr = 1:n)

#### latent ####
df$true_latent <- rnorm(n)

#### moderator ####
df$moderator  <- runif(n)

#### indicators ####
df$indicator1 <- rnorm(n, mean = df$true_latent)
df$indicator2 <- rnorm(n, mean = df$true_latent)
df$indicator3 <- rnorm(n, mean = df$true_latent*df$moderator)
df$indicator4 <- rnorm(n, mean = df$true_latent*df$moderator)

rm(n)
