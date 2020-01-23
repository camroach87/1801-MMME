rm(list=ls())

ts_l <- 10
al_l <- 24
n_sim <- 10

alpha <- sin(seq(0, 2*pi, length.out = 24))*0.5
z <- matrix(NA, ncol=ts_l, nrow=n_sim)
z[,1] <- 0
for (iR in 1:n_sim) {
  for (iC in 2:ts_l) {
    al_idx <- iC %% al_l
    z[iR, iC] <- alpha[if (al_idx == 0) al_l else al_idx]*z[iR, iC-1] + rnorm(1, 0, 0.2)
  }
}

