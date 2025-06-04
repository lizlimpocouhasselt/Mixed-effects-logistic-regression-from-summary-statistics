#---------------------------------------------------------------
# FUNCTION TO COMPUTE COVERAGE
# INPUTS:
#   nsim : number of simulations
#   m : number of clusters
#   uniform_cluster_size : cluster size
#   parnum : parameter number
#   lookup_df : dataframe consisting of the true parameter values
#
# OUTPUT: 
#   vector of 95% confidence interval coverage for
#      estimates from pseudo- and actual data
#---------------------------------------------------------------

fn_coverage <- function(nsim, m, uniform_cluster_size, parnum, lookup_df){
  sapply(c('sim', 'ps2', 'ps3', 'ps4'), function(data.type){
    mean(sapply(1:nsim, function(iter){
      load(sprintf(file.path("SIMULATION", "intermediate_results", 
                             "orig_setting", "interval_estimates",
                             "interval_estimate_%04d_%04d_%04d.RData"),
                   iter, m, uniform_cluster_size))
      partrue <- lookup_df$true[match(parnum, lookup_df$num)]
      partrue >= interval_estimate[[data.type]][parnum, 1] &
        partrue <= interval_estimate[[data.type]][parnum, 2]
    }), na.rm = TRUE)
  })
}