#---------------------------------------------------------------
# FUNCTION TO COMPUTE LOG-LIKELIHOOD
# INPUTS:
#   b : domain of parameter values
#   data : dataframe
#   deg : Taylor polynomial degree
#   approx : logical; approximate log-likelihood?
#
# OUTPUT: 
#   log-likelihood function value
#---------------------------------------------------------------

# Define loglik function
loglik.f <- function(b, data, deg = NULL, approx = FALSE){
    y <- data$y; x <- data$x
    eta <- b * scale(x)
    if(approx){
        fn <- function(x) log(1 + exp(x))
        coef <- taylor(fn, var = c(x=0), order = deg)$terms$coef
        pol <- sapply(0:deg, function(pow){
                coef[pow + 1] * eta^pow/factorial(pow)
               })
        return(sum(y * eta) - sum(pol))
    } else{return(sum(y * eta) - sum(log(1 + exp(eta))))}
}