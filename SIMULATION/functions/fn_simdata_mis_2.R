#---------------------------------------------------------------
# FUNCTION TO SIMULATE DATA FOR GLMM
# INPUTS:
#   m : number of clusters
#   uniform_cluster_size : cluster size
#   seed : seed assigned to particular iteration and scenario
#   x_pars : simulation parameters of covariates
#
# OUTPUT: 
#   dataframe whose columns are the covariates, response, and
#   group number
#---------------------------------------------------------------

fn_simdata_glmm <- function(m, uniform_cluster_size, seed, x_pars){
  
  n_h <- rep(uniform_cluster_size, m)
  n = sum(n_h) #total sample size
  
  # Set simulation parameters
  b <- c(-4.20753, 0.33348, -0.25252, 1.20088, 0.53604)
  var_h <- 32
  sigma_h <- sqrt(var_h)
  
  # Generate predictor data
  set.seed(seed)
  x1 <- rnorm(n, x_pars$x1_norm_mean, x_pars$x1_norm_sd)
  x2 <- rpois(n, x_pars$x2_pois_lambda)
  x3 <- rmultinom(n, 1, x_pars$x3_multinom_p)
  X <- cbind(matrix(c(x1, x2), ncol = 2, byrow = F), t(x3)[,-1]) #exclude ref cat
  u0 <- rlnorm(m, 0, sigma_h) 
  z <- model.matrix(~-1 + ., data.frame(grp_num = as.factor(rep(1:m, n_h))))
  
  # Generate response data
  xb <- b[1] + scale(X[, 1:2]) %*% b[2:3] + X[, 3:4] %*% b[4:5] + z %*% u0
  p <- 1/(1 + exp(-xb))
  y <- sapply(p, rbinom, n = 1, size = 1)
  
  # Create dataframe
  data.frame(x1=x1, x2=x2, x3=as.factor(ifelse(x3[1,]==1, 1,
                                     ifelse(x3[2,]==1, 2, 3))),
             y=y, g=as.factor(rep(1:m, n_h)))
}