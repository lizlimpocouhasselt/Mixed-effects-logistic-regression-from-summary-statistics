#---------------------------------------------------------------
# FUNCTION TO TABULATE BIAS
# INPUTS:
#   nsim : number of simulations
#   m : number of clusters
#   uniform_cluster_size : cluster size
#
# OUTPUT: 
#   long dataframe ready for creating boxplots
#---------------------------------------------------------------

fn_bias <- function(nsim, m, uniform_cluster_size){
    bias.ls <- lapply(c('sim', 'ps2', 'ps3', 'ps4'), function(data.type){
              mat <- t(sapply(1:nsim, function(iter){
                      load(sprintf(file.path("SIMULATION", "intermediate_results", 
                                            "mis_0", "point_estimates",
                                      "point_estimate_%04d_%04d_%04d.RData"),
                                   iter, m, uniform_cluster_size))
                      point_estimate[, data.type] - point_estimate[, 'true']
                    }))
              df <- as.data.frame(mat)
              df$dat <- rep(data.type, nsim)
              df$iter <- 1:nsim
              df
            })
    bias.wide <- do.call(rbind, bias.ls)
    colnames(bias.wide)[1:5] <- c('sig.u', 'b0', 'b1', 'b32', 'b33')

    bias.df <- melt(bias.wide, id.vars = c("dat", "iter"),
                variable.name = "pars",
                value.name = "bias")
    return(bias.df)
}



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
                             "mis_0", "interval_estimates",
                             "interval_estimate_%04d_%04d_%04d.RData"),
                   iter, m, uniform_cluster_size))
      partrue <- lookup_df$true[match(parnum, lookup_df$num)]
      partrue >= interval_estimate[[data.type]][parnum, 1] &
        partrue <= interval_estimate[[data.type]][parnum, 2]
    }), na.rm = TRUE)
  })
}