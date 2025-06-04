#---------------------------------------------------------------
# FUNCTION TO COMPUTE SUMMARY DATA
# INPUTS:
#   m : number of clusters
#   uniform_cluster_size : cluster size
#   seed : seed assigned to particular iteration and scenario
#   simdata : simulated data
#
# OUTPUT: 
#   list containing:
#   1. mean and covariance dataframe
#   2. variance-covariance matrix
#   3. bivariate 3rd and 4th central moments
#   4. trivariate 3rd and 4th central moments
#   5. quadvariate 4th central moments
#---------------------------------------------------------------

fn_compute_summary <- function(m, uniform_cluster_size, seed, simdata){
  
  set.seed(seed) #for reproducibility

  # Convert categorical variable to dummy variables
  X.names <- setdiff(names(simdata), c('y', 'g'))
  X <- model.matrix(~ -1 + ., data = simdata[X.names])
  simdata <- cbind(X, simdata[c('y', 'g')])
  
  # Enumerate combinations of variable names
  if(ncol(X) > 3) wxyz <- combn(colnames(X), 4) 
  if(ncol(X) > 2) xyz <- combn(colnames(X), 3) 
  if(ncol(X) > 1) xy <- combn(colnames(X), 2) 
  
  # Prepare summary statistics (data provider task) ------------------
  
  # Break up data per group
  group_data_design_ls <- split(simdata, simdata$g)
  
  # Compute univariate statistics 
  mean_cov <- lapply(group_data_design_ls, function(group){
    group <- group[-ncol(group)] #exclude group number
    orig.means <- colMeans(group, na.rm = TRUE) 
    orig.variances <- colVars(as.matrix(group), na.rm = TRUE)
    cn.data <- scale(group, scale = FALSE)
    mean_cov_ <- data.frame(variable = names(group),  
               type = if(ncol(X) == 1){c('num', 'bin')} else{c('num', 'num',
                'bin', 'bin', 'bin', 'bin')},
               n = uniform_cluster_size,
               mean = orig.means,
               variance = orig.variances,
               target_3_moment = colMeans(cn.data^3, na.rm = TRUE),
               target_4_moment = colMeans(cn.data^4, na.rm = TRUE),
               row.names = NULL)
    cn.data[,mean_cov_$type == "num" & 
              orig.variances > 0] <- scale(cn.data[,mean_cov_$type == "num" &
               orig.variances > 0])
    mean_cov_$std_mean <- ifelse(mean_cov_$type == "num", 0, mean_cov_$mean)
    mean_cov_$std_var <- ifelse(mean_cov_$type == "num", 1, mean_cov_$variance)
    mean_cov_$std_target_3_moment <- ifelse(mean_cov_$type == "num", 
                                            colMeans(cn.data^3, na.rm = TRUE), 
                                            mean_cov_$target_3_moment)
    mean_cov_$std_target_4_moment <- ifelse(mean_cov_$type == "num", 
                                            colMeans(cn.data^4, na.rm = TRUE), 
                                            mean_cov_$target_4_moment)
    mean_cov_
  })
  var.type <- mean_cov[[1]][,'type']

  # Compute variance covariance matrix
  var_cov_mat <- lapply(group_data_design_ls, function(group){
    group <- group[-ncol(group)]
    group[var.type == "num" & 
            colVars(as.matrix(group)) > 0] <- scale(group[var.type == "num" & 
                                              colVars(as.matrix(group)) > 0])
    cov(group)
    })
  
  if(ncol(X) > 1){
    # Compute bivariate 3rd and 4th moments
    mv_moment_3_4_bypair_df <- lapply(group_data_design_ls, function(group){
    group <- group[-ncol(group)]
    group[var.type == "num" & 
            colVars(as.matrix(group)) > 0] <- scale(group[var.type == "num" & 
                                              colVars(as.matrix(group)) > 0])
    cn.data <- scale(group, scale = FALSE)
    var1 <- cn.data[, xy[1, ]]
    var2 <- cn.data[, xy[2, ]]
    data.frame(vars = paste0(xy[1, ], "_", xy[2, ]),
               a2b = colMeans(var1^2 * var2),
               ab2 = colMeans(var1 * var2^2),
               a3b = colMeans(var1^3 * var2),
               a2b2 = colMeans(var1^2 * var2^2),
               ab3 = colMeans(var1 * var2^3)
              )
    })

    if(ncol(X) > 2){
      # Compute trivariate 3rd and 4th moments
      mv_moment_3_4_by3_df <- lapply(group_data_design_ls, function(group){
      group <- group[-ncol(group)]
      group[var.type == "num" & 
            colVars(as.matrix(group)) > 0] <- scale(group[var.type == "num" & 
                                              colVars(as.matrix(group)) > 0])
      cn.data <- scale(group, scale = FALSE)
      var1 <- cn.data[, xyz[1, ]]
      var2 <- cn.data[, xyz[2, ]]
      var3 <- cn.data[, xyz[3, ]]
      data.frame(vars = paste0(xyz[1, ], "_", xyz[2, ], "_", xyz[3, ]),
               abc = colMeans(var1 * var2 * var3),
               a2bc = colMeans(var1^2 * var2 * var3),
               ab2c = colMeans(var1 * var2^2 * var3),
               abc2 = colMeans(var1 * var2 * var3^2)
                )
      })

      if(ncol(X) > 3){
        # Compute quadvariate 4th moments
        mv_moment_4_df <- lapply(group_data_design_ls, function(group){
        group <- group[-ncol(group)]
        group[var.type == "num" & 
            colVars(as.matrix(group)) > 0] <- scale(group[var.type == "num" & 
                                              colVars(as.matrix(group)) > 0])
        cn.data <- scale(group, scale = FALSE)
        var1 <- cn.data[, wxyz[1, ]]
        var2 <- cn.data[, wxyz[2, ]]
        var3 <- cn.data[, wxyz[3, ]]
        var4 <- cn.data[, wxyz[4, ]]
        data.frame(vars = paste0(wxyz[1, ], "_", wxyz[2, ], "_", wxyz[3, ], "_", wxyz[4, ]),
               abcd = colMeans(var1 * var2 * var3 * var4)
                )
        })
        return(list(mean_cov = mean_cov,
                  var_cov_mat = var_cov_mat,
                  mv_moment_3_4_bypair_df = mv_moment_3_4_bypair_df,
                  mv_moment_3_4_by3_df = mv_moment_3_4_by3_df,
                  mv_moment_4_df = mv_moment_4_df))
      }
      return(list(mean_cov = mean_cov,
                  var_cov_mat = var_cov_mat,
                  mv_moment_3_4_bypair_df = mv_moment_3_4_bypair_df,
                  mv_moment_3_4_by3_df = mv_moment_3_4_by3_df))
    }
    return(list(mean_cov = mean_cov,
                  var_cov_mat = var_cov_mat,
                  mv_moment_3_4_bypair_df = mv_moment_3_4_bypair_df))
  }
  
  return(list(mean_cov = mean_cov,
              var_cov_mat = var_cov_mat))
}