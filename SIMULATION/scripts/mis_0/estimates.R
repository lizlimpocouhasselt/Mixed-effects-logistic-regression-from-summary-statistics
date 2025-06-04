#-------------------------------------------------------------------
# MODEL ESTIMATION FOR MODEL MISSPECIFICATION: VARIABLE OMISSION
#     WITH LARGE RANDOM EFFECTS VARIANCE
#-------------------------------------------------------------------

# Load packages
library(lme4)

# Call functions
source(file.path("SIMULATION", "functions", "fn_estimate_helper.R"))

# Load the parameter settings
par_settings <- read.csv(file.path("SIMULATION", "par_settings.csv"))

# Estimate parameters of model
sapply(1:3000, function(row){
  iter <- par_settings$iter[row]
  m <- par_settings$m[row]
  uniform_cluster_size <- par_settings$uniform_cluster_size[row]
  seed <- par_settings$seed[row]

  # Load data
  load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0", "simdata", 
                            "simdata_%04d_%04d_%04d.RData"), iter, m, uniform_cluster_size))
  load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0", "ps2", 
                            "pseudodata_2ndmom_%04d_%04d_%04d.RData"),
                             iter, m, uniform_cluster_size))
  load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0", "ps3", 
                            "pseudodata_3rdmom_%04d_%04d_%04d.RData"),
                             iter, m, uniform_cluster_size))
  load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0", "ps4", 
                            "pseudodata_4thmom_%04d_%04d_%04d.RData"),
                             iter, m, uniform_cluster_size))
  
  # Generate point estimates
  mod.result.sim <- tryCatch({glmer(y ~ scale(x1) + x3 + (1|g), 
                                simdata, family = binomial)},
                              error = function(e){return(NULL)}, 
                              warning = function(w){return(NULL)})
  mod.result.ps2 <- tryCatch({glmer(y ~ scale(x1) + x32 + x33 + (1|level2), 
                                do.call(rbind, pseudodata_2ndmom), family = binomial)}, 
                              error = function(e){return(NULL)}, 
                              warning = function(w){return(NULL)})
  mod.result.ps3 <- tryCatch({glmer(y ~ scale(x1) + x32 + x33 + (1|level2), 
                                do.call(rbind, pseudodata_3rdmom), family = binomial)}, 
                              error = function(e){return(NULL)}, 
                              warning = function(w){return(NULL)})
  mod.result.ps4 <- tryCatch({glmer(y ~ scale(x1) + x32 + x33 + (1|level2), 
                                do.call(rbind, pseudodata_4thmom), family = binomial)}, 
                              error = function(e){return(NULL)},
                              warning = function(w){return(NULL)})

  # Generate interval estimates
  confint.result.sim <- tryCatch({confint(mod.result.sim)}, 
                        error = function(e){return(NULL)}, 
                        warning = function(w){return(NULL)})
  confint.result.ps2 <- tryCatch({confint(mod.result.ps2)}, 
                        error = function(e){return(NULL)}, 
                        warning = function(w){return(NULL)})
  confint.result.ps3 <- tryCatch({confint(mod.result.ps3)}, 
                        error = function(e){return(NULL)}, 
                        warning = function(w){return(NULL)})
  confint.result.ps4 <- tryCatch({confint(mod.result.ps4)}, 
                        error = function(e){return(NULL)}, 
                        warning = function(w){return(NULL)})
  
  # Tabulate results
  point_estimate <- data.frame(
                      true = c(sqrt(32), -4.20753, 0.33348, 1.20088, 0.53604),
                      sim = point_mis_1(mod.result.sim),
                      ps2 = point_mis_1(mod.result.ps2),
                      ps3 = point_mis_1(mod.result.ps3),
                      ps4 = point_mis_1(mod.result.ps4)
                    )
  interval_estimate <- list(
                          sim = interval_mis_1(confint.result.sim),
                          ps2 = interval_mis_1(confint.result.ps2),
                          ps3 = interval_mis_1(confint.result.ps3),
                          ps4 = interval_mis_1(confint.result.ps4)
  )

  # Save results
  filename_point <- sprintf(file.path("SIMULATION", "intermediate_results", "mis_0",
   "point_estimates", "point_estimate_%04d_%04d_%04d"), iter, m, uniform_cluster_size)
save("point_estimate", file = sprintf("%s.RData", filename_point))



 filename_interval <- sprintf(file.path("SIMULATION", "intermediate_results", "mis_0",
  "interval_estimates", "interval_estimate_%04d_%04d_%04d"), iter, m, uniform_cluster_size)
 save("interval_estimate", file = sprintf("%s.RData", filename_interval))
})
