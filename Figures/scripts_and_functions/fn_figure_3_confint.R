#---------------------------------------------------------------
# FUNCTION TO TABULATE ESTIMATES (POINT & INTERVAL)
# INPUTS:
#   nsim : number of simulations
#   m : number of clusters
#   uniform_cluster_size : cluster size
#   parnum : nth parameter
#
# OUTPUT: 
#   long dataframe consisting of point and interval estimates
#---------------------------------------------------------------

fn_confint <- function(nsim, m, uniform_cluster_size, parnum){
    ci.ls <- lapply(c('sim', 'ps2', 'ps3', 'ps4'), function(data.type){
              mat <- t(sapply(1:nsim, function(iter){
                      load(sprintf(file.path("SIMULATION", "intermediate_results", 
                                            "orig_setting", "point_estimates",
                                      "point_estimate_%04d_%04d_%04d.RData"),
                                   iter, m, uniform_cluster_size))
                      load(sprintf(file.path("SIMULATION", "intermediate_results", 
                                             "orig_setting", "interval_estimates",
                                      "interval_estimate_%04d_%04d_%04d.RData"),
                                   iter, m, uniform_cluster_size))
                      c(interval_estimate[[data.type]][parnum, 1],
                        point_estimate[parnum, data.type], 
                        interval_estimate[[data.type]][parnum, 2])
                    }))
              df <- as.data.frame(mat)
              df$dat <- rep(data.type, nsim)
              df
            })
    ci.ls <- lapply(ci.ls, function(ci) ci <- ci[order(unlist(ci.ls[[1]][, 2])), ])
    ci.df <- do.call(rbind, ci.ls)
    colnames(ci.df)[1:3] <- c('LL', 'point', 'UL')
    return(ci.df)
}