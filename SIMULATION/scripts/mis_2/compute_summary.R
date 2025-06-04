#----------------------------------------------------
# SUMMARIES: GENERALIZED LINEAR MIXED MODEL - LOGIT LINK
#----------------------------------------------------


# Load packages
library(matrixStats)

# Call functions
source(file.path("SIMULATION", "functions", "fn_compute_summary.R"))

# Load the parameter settings
par_settings <- read.csv(file.path("SIMULATION", "par_settings.csv"))

# Compute summary data per cluster/group
sapply(1:3000, function(row){
    iter <- par_settings$iter[row]
    m <- par_settings$m[row]
    uniform_cluster_size <- par_settings$uniform_cluster_size[row]
    seed <- par_settings$seed[row]

    # Load the simulated data
    load(sprintf(file.path("SIMULATION", "intermediate_results", "mis_2", "simdata",
                           "simdata_%04d_%04d_%04d.RData"), iter, m, uniform_cluster_size))
    
    # Compute summary data
    summary_info <- fn_compute_summary(m, uniform_cluster_size, seed, simdata) 
    mean_cov <- summary_info[[1]]
    var_cov_mat <- summary_info[[2]]
    mv_moment_3_4_bypair_df <- summary_info[[3]]
    mv_moment_3_4_by3_df <- summary_info[[4]]
    mv_moment_4_df <- summary_info[[5]]

    # Save [intermediate] outputs
    filename_save_mean_cov <- sprintf(file.path("SIMULATION", "intermediate_results",
                                                "mis_2", "mean_cov",
                                                "mean_cov_%04d_%04d_%04d"),
                                     iter, m, uniform_cluster_size)
    save("mean_cov", file = sprintf("%s.RData", filename_save_mean_cov))

    filename_save_var_cov_mat <- sprintf(file.path("SIMULATION", "intermediate_results",
                                                   "mis_2", "var_cov_mat",
                                                   "var_cov_mat_%04d_%04d_%04d"),
                                        iter, m, uniform_cluster_size)
    save("var_cov_mat", file = sprintf("%s.RData", filename_save_var_cov_mat))

    filename_save_mv_moment_3_4_bypair_df <- sprintf(file.path(
                                                        "SIMULATION", "intermediate_results",
                                                        "mis_2", "mv_moment_3_4_bypair_df",
                                                        "mv_moment_3_4_bypair_df_%04d_%04d_%04d"),
                                                    iter, m, uniform_cluster_size)
    save("mv_moment_3_4_bypair_df", file = sprintf("%s.RData",
     filename_save_mv_moment_3_4_bypair_df))

    filename_save_mv_moment_3_4_by3_df <- sprintf(file.path(
                                                        "SIMULATION", "intermediate_results",
                                                        "mis_2", "mv_moment_3_4_by3_df",
                                                        "mv_moment_3_4_by3_df_%04d_%04d_%04d"),
                                                    iter, m, uniform_cluster_size)
    save("mv_moment_3_4_by3_df", file = sprintf("%s.RData", filename_save_mv_moment_3_4_by3_df))

    filename_save_mv_moment_4_df <- sprintf(file.path("SIMULATION", "intermediate_results",
                                                      "mis_2", "mv_moment_4_df",
                                                      "mv_moment_4_df_%04d_%04d_%04d"),
                                                    iter, m, uniform_cluster_size)
    save("mv_moment_4_df", file = sprintf("%s.RData", filename_save_mv_moment_4_df))
})
