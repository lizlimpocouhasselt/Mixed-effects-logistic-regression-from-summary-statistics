#----------------------------------------------------
# SIMULATION: GENERALIZED LINEAR MIXED MODEL - LOGIT LINK
#----------------------------------------------------


# Call functions
source(file.path("SIMULATION", "functions", "fn_simdata_mis_2.R"))

# Set parameters based on real CHOP dataset
x_pars <- list(x1_norm_mean = 7.290774,
               x1_norm_sd = 9.341565,
               x2_pois_lambda = 63.51643,
               x3_multinom_p = c(0.5205371, 0.2139021, 0.2655608))

# Load the parameter settings
par_settings <- read.csv(file.path("SIMULATION", "par_settings.csv"))

# Simulate datasets
sapply(1:3000, function(row){
    iter <- par_settings$iter[row]
    m <- par_settings$m[row]
    uniform_cluster_size <- par_settings$uniform_cluster_size[row]
    seed <- par_settings$seed[row]

    simdata <- fn_simdata_glmm(m, uniform_cluster_size, seed, x_pars) 
    filename_save <- sprintf(file.path("SIMULATION", "intermediate_results",
                                        "mis_2", "simdata",
                                        "simdata_%04d_%04d_%04d"),
                            iter, m, uniform_cluster_size)
    save("simdata", file = sprintf("%s.RData", filename_save))
})