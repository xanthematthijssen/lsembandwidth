

library(devtools)
rm(list = ls())
load_all()
check()

data <- cbind(rnorm(100), runif(100))
# build ignore
usethis::use_build_ignore(c("R_drabbels"))
create_crossvaldata(data=data, K = 5)
