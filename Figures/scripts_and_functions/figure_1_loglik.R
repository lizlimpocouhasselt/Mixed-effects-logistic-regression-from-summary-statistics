#-------------------------------------------------------------------
# FIGURE 1: COMPARING LOG-LIKELIHOODS
#-------------------------------------------------------------------

# Load packages
library(maxLik)
library(matrixStats)
library(MASS)
library(pracma)
library(calculus)


# Call functions
source(file.path("Figures", "scripts_and_functions", "fn_figure_1_loglik.R"))
source(file.path("SIMULATION", "functions", "fn_compute_summary.R"))
source(file.path("SIMULATION", "functions", "fn_pseudodata_4thmom.R"))
source(file.path("SIMULATION", "functions", "gen_pseudo.R"))
source(file.path("SIMULATION", "functions", "lsqnonlin_2.R"))

# Simulate data 
set.seed(121314)
n <- 250
xbar <- 25; s <- 5
beta <- 1.5
x <- rnorm(n, xbar, s)
z <- scale(x)
zb <- beta * z
p.i <- 1/(1 + exp(-zb))
y <- sapply(p.i, function(i) rbinom(1, 1, i))
data <- data.frame(y, x, z)
    
# Identify maximum likelihood estimate
M <- maxLik(loglik.f, start = 0, data = data)
     
# Compute summary
data$g <- rep(1, n)
summary <- fn_compute_summary(1, n, seed = 121314, data[-3])
mean_cov <- summary[[1]]
var_cov_mat <- summary[[2]]

# Generate pseudodata
y_name = 'y'
names_ind_vars = "x"
pseudodata <- fn_pseudodata_4thmom_glmm(1, 1, y_name, n, 121314, 'x', 
'x', mean_cov, var_cov_mat)
    
# Identify maximum likelihood estimate
M.ps <- maxLik(loglik.f, start = 0, data = pseudodata[[1]])
    
# Define approx loglik function
deg <- 4
# Identify maximum likelihood estimate
M.approx <- maxLik(loglik.f, start = 0, data = data, deg = deg, approx = TRUE)

 # Plot
b <- seq(-5, 5, by = 0.1)
actual <- sapply(b, loglik.f, data = data)
ps <- sapply(b, loglik.f, data = pseudodata[[1]])
t.approx <- sapply(b, loglik.f, data = data, deg = deg, approx = T)
postscript(file.path("Figures", "outputs",
                     "fig1_concavity_approx_loglik.eps"), onefile = FALSE)
plot(b, actual, lwd = 3, type = "l", ylab = 'loglikelihood', 
    xlab = expression(beta[1]), cex.lab = 1.5,
    ylim = c(min(actual, ps, t.approx),
                max(actual, ps, t.approx)),
    xlim = c(-6,6))
lines(b, ps, lty = 2, lwd = 3, col = 'blue')
lines(b, t.approx, lty = 2, lwd = 3, col = 'orange')
abline(v = M$estimate, lwd = 3, col = 'black', lty = 1)
abline(v = (M.ps$estimate), lwd = 3, col = 'blue')
abline(v = M.approx$estimate, lwd = 3, col = 'orange')
abline(v = M.approx$estimate - 1.96*stdEr(M.approx), lty = 2, lwd = 3, col = 'orange')
abline(v = M.approx$estimate + 1.96*stdEr(M.approx), lty = 2, col = 'orange', lwd = 3)
abline(v = beta, lwd = 3, col = 'red')
legend('topleft', legend = c('true par', 'sim', 'ps', 'taylor approx'), lwd = 3,
 col = c('red', 'black', 'blue', 'orange'), lty = c(1,2,2,2), cex = 2)
dev.off()
