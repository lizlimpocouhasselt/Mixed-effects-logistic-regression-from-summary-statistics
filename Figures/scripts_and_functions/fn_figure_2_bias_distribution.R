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
                                            "orig_setting", "point_estimates",
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
    colnames(bias.wide)[1:6] <- c('sig.u', 'b0', 'b1', 'b2', 'b32', 'b33')

    bias.df <- melt(bias.wide, id.vars = c("dat", "iter"),
                variable.name = "pars",
                value.name = "bias")
    return(bias.df)
}