#----------------------------------------------------
# COMPUTING THE AIC VALUES
#----------------------------------------------------

# Load libraries
library(lme4)

# Load the parameter settings
par_settings <- read.csv(file.path("SIMULATION", "par_settings.csv"))

# Compute AIC values
sapply(1:nrow(par_settings), function(row){
  iter <- par_settings$iter[row]
  m <- par_settings$m[row]
  uniform_cluster_size <- par_settings$uniform_cluster_size[row]
  seed <- par_settings$seed[row]

  # Load data
  load(sprintf(file.path("SIMULATION", "intermediate_results", "orig_setting","simdata", 
                            "simdata_%04d_%04d_%04d.RData"), iter, m, uniform_cluster_size))
  load(sprintf(file.path("SIMULATION", "intermediate_results", "orig_setting", "ps2", 
                            "pseudodata_2ndmom_%04d_%04d_%04d.RData"),
                             iter, m, uniform_cluster_size))
  load(sprintf(file.path("SIMULATION", "intermediate_results", "orig_setting", "ps3", 
                            "pseudodata_3rdmom_%04d_%04d_%04d.RData"),
                             iter, m, uniform_cluster_size))
  load(sprintf(file.path("SIMULATION", "intermediate_results", "orig_setting", "ps4", 
                            "pseudodata_4thmom_%04d_%04d_%04d.RData"),
                             iter, m, uniform_cluster_size))

  # Compute AIC values
  mod.result.sim <- tryCatch({summary(glmer(y ~ scale(x1) + scale(x2) + x3 + (1|g), 
                                simdata, family = binomial))},
                              error = function(e){return(NULL)}, 
                              warning = function(w){return(NULL)})
  mod.result.ps2 <- tryCatch({summary(glmer(y ~ scale(x1) + scale(x2) + x32 + x33 + (1|level2), 
                                do.call(rbind, pseudodata_2ndmom), family = binomial))}, 
                              error = function(e){return(NULL)}, 
                              warning = function(w){return(NULL)})
  mod.result.ps3 <- tryCatch({summary(glmer(y ~ scale(x1) + scale(x2) + x32 + x33 + (1|level2), 
                                do.call(rbind, pseudodata_3rdmom), family = binomial))}, 
                              error = function(e){return(NULL)}, 
                              warning = function(w){return(NULL)})
  mod.result.ps4 <- tryCatch({summary(glmer(y ~ scale(x1) + scale(x2) + x32 + x33 + (1|level2), 
                                do.call(rbind, pseudodata_4thmom), family = binomial))}, 
                              error = function(e){return(NULL)},
                              warning = function(w){return(NULL)})

  aic_vals <- c(if(!is.null(mod.result.sim))
  mod.result.sim$AICtab[1] else NA,
  if(!is.null(mod.result.ps2))
    mod.result.ps2$AICtab[1] else NA,
  if(!is.null(mod.result.ps3))
    mod.result.ps3$AICtab[1] else NA,
  if(!is.null(mod.result.ps4))
    mod.result.ps4$AICtab[1] else NA)
  
  # Save results
  filename_save <- sprintf(file.path("SIMULATION", "intermediate_results", "orig_setting", "aic", 
                                        "aic_vals_%04d_%04d_%04d"), iter, m, uniform_cluster_size)
  save("aic_vals", file = sprintf("%s.RData", filename_save))
})