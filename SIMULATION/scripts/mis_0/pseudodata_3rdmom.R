#-------------------------------------------------------------------
# PSEUDO-DATA (K = 3): GENERALIZED LINEAR MIXED MODEL - LOGIT LINK
#-------------------------------------------------------------------


# Load packages
library(pracma)
library(MASS)

# Call functions
source(file.path("SIMULATION", "functions", "lsqnonlin_2.R"))
source(file.path("SIMULATION", "functions", "gen_pseudo.R"))
source(file.path("SIMULATION", "functions", "fn_pseudodata_3rdmom.R"))

# Load the parameter settings
par_settings <- read.csv(file.path("SIMULATION", "par_settings.csv"))

# Specify model
formula <- y ~ x1 + x2 + x32 + x33
y_name <- all.vars(formula)[1]
names_ind_vars <- all.vars(formula)[-1]
numeric_var_names <- c('x1', 'x2')

# Generate pseudo-data per cluster/group
sapply(1:3000, function(row){
    iter <- par_settings$iter[row]
    m <- par_settings$m[row]
    uniform_cluster_size <- par_settings$uniform_cluster_size[row]
    seed <- par_settings$seed[row]

    # Load summary data
    load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0", "mean_cov", 
                            "mean_cov_%04d_%04d_%04d.RData"), iter, m, uniform_cluster_size))
    load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0", "var_cov_mat", 
                            "var_cov_mat_%04d_%04d_%04d.RData"), iter, m, uniform_cluster_size))
    load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0",
     "mv_moment_3_4_bypair_df", "mv_moment_3_4_bypair_df_%04d_%04d_%04d.RData"), 
                            iter, m, uniform_cluster_size))
    load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0",
     "mv_moment_3_4_by3_df", "mv_moment_3_4_by3_df_%04d_%04d_%04d.RData"), 
                            iter, m, uniform_cluster_size))
    load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_0", "mv_moment_4_df", 
                            "mv_moment_4_df_%04d_%04d_%04d.RData"), 
                            iter, m, uniform_cluster_size))

    # Generate pseudo-data
    pseudodata_3rdmom <- fn_pseudodata_3rdmom_glmm(iter, m, y_name, uniform_cluster_size, 
                    seed, names_ind_vars, numeric_var_names, mean_cov, var_cov_mat,
                    mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df, mv_moment_4_df) 
    
    # Save pseudo-data
    filename_save <- sprintf(file.path("SIMULATION", "intermediate_results", "mis_0",
     "ps3", "pseudodata_3rdmom_%04d_%04d_%04d"), iter, m, uniform_cluster_size)
    save("pseudodata_3rdmom", file = sprintf("%s.RData", filename_save))
})
