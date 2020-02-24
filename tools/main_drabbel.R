library(devtools)
rm(list = ls())

load_all()
check()
##### testing #####
data <- cbind(rnorm(100), runif(100))
create_crossvaldata(data=data, K = 5)

#####  build ignore#####
usethis::use_build_ignore(c("R_drabbels"))

#### MIT license ######
use_mit_license("Xanthe Matthijssen")
