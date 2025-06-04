#---------------------------------------------------------------
# FUNCTION TO GENERATE PSEUDO-DATA DEPENDING ON K
#
# INPUTS:
#   moment : Taylor polynomial degree (K = 2, 3, or 4)
#   y_syn : vector of pseudo-responses
#   name_ind : name of covariate for which pseudo-data
#                will be generated
#   numeric_var_names : names of all numeric covariates
#   syn_df : dataframe containing the pseudo-data
#              generated in the previous iterations
#   mean_cov : mean and covariance dataframe
#   var_cov_mat : variance-covariance matrix
#   mv_moment_3_4_bypair_df : bivariate 3rd and 4th central moments
#   mv_moment_3_4_by3_df : trivariate 3rd and 4th central moments
#   mv_moment_4_df : quadvariate 4th central moments
#
# OUTPUT: 
#   dataframe of pseudo-data
#---------------------------------------------------------------

gen_pseudo <- function(moment, y_syn, name_ind, numeric_var_names, syn_df, 
mean_cov, var_cov_mat, mv_moment_3_4_bypair_df = NULL,
 mv_moment_3_4_by3_df = NULL, mv_moment_4_df = NULL){
  
  # Define objective function
  row.idx <- mean_cov[,'variable'] == name_ind
  target_mean <- mean_cov[row.idx, 'std_mean']
  target_var <- mean_cov[row.idx, 'std_var']
  target_cov_y_x <- var_cov_mat[y_name, name_ind]
  target_3_moment <- mean_cov[row.idx, 'std_target_3_moment']
  target_4_moment <- mean_cov[row.idx, 'std_target_4_moment']
  fn <- function(x){
    cn.x <- scale(x, scale = FALSE)[, 1] #centered pseudo-data
    idx.indvar <- which(names_ind_vars == name_ind)
    other.inds <- names(syn_df)[-1]
    
    unlist(c(
      # Define univariate constraints
      mean(x) - target_mean,
      var(x) - target_var,
      cov(y_syn, x) - target_cov_y_x,
      if(moment >= 3) mean(cn.x^3, na.rm = TRUE) - target_3_moment,
      if(moment == 4) mean(cn.x^4, na.rm = TRUE) - target_4_moment,
      # Define bivariate constraints
      if(idx.indvar > 1){
        c(sapply(other.inds, function(other.ind){
          cn.other.ind <- scale(syn_df[, other.ind], scale = FALSE)[, 1]
          bv.con <- c(cov(syn_df[, other.ind], x) - 
                      var_cov_mat[other.ind, name_ind])
          if(moment >= 3){
            bivars <- mv_moment_3_4_bypair_df$vars
            row.idx <- grepl(other.ind, bivars, fixed = TRUE) &
                        grepl(name_ind, bivars, fixed = TRUE)
            target_a2b <- mv_moment_3_4_bypair_df[row.idx, 'a2b']
            target_ab2 <- mv_moment_3_4_bypair_df[row.idx, 'ab2']
            bv.con <- c(bv.con,
              mean(cn.other.ind^2 * cn.x, na.rm = TRUE) - target_a2b,
              mean(cn.other.ind * cn.x^2, na.rm = TRUE) - target_ab2
            )
            if(moment == 4){
              target_a3b <- mv_moment_3_4_bypair_df[row.idx, 'a3b']
              target_ab3 <- mv_moment_3_4_bypair_df[row.idx, 'ab3']
              target_a2b2 <- mv_moment_3_4_bypair_df[row.idx, 'a2b2']
              bv.con <- c(bv.con,
                mean(cn.other.ind^3 * cn.x, na.rm = TRUE) - target_a3b,
                mean(cn.other.ind * cn.x^3, na.rm = TRUE) - target_ab3,
                mean(cn.other.ind^2 * cn.x^2, na.rm = TRUE) - target_a2b2
              )
            }
          }
          bv.con
          }))
      },
      # Define trivariate constraints
      if(idx.indvar > 2){
        x12 <- combn(other.inds, 2)
        c(sapply(1:ncol(x12), function(nth.pair){
          other.ind1 <- x12[1, nth.pair]
          other.ind2 <- x12[2, nth.pair]
          cn.other.ind1 <- scale(syn_df[, other.ind1], scale = FALSE)[, 1]
          cn.other.ind2 <- scale(syn_df[, other.ind2], scale = FALSE)[, 1]
          trivars <- mv_moment_3_4_by3_df$vars
          row.idx <- grepl(other.ind1, trivars, fixed = TRUE) &
                      grepl(other.ind2, trivars, fixed = TRUE) &
                      grepl(name_ind, trivars, fixed = TRUE)
          if(moment >= 3){
            target_abc <- mv_moment_3_4_by3_df[row.idx, 'abc']
            tv.con <- mean(cn.other.ind1 * cn.other.ind2 * cn.x) - target_abc
            if(moment == 4){
              target_a2bc <- mv_moment_3_4_by3_df[row.idx, 'a2bc']
              target_ab2c <- mv_moment_3_4_by3_df[row.idx, 'ab2c']
              target_abc2 <- mv_moment_3_4_by3_df[row.idx, 'abc2']
              tv.con <- c(tv.con,
                  mean(cn.other.ind1^2 * cn.other.ind2 * cn.x) - target_a2bc,
                  mean(cn.other.ind1 * cn.other.ind2^2 * cn.x) - target_ab2c,
                  mean(cn.other.ind1 * cn.other.ind2 * cn.x^2) - target_abc2
                  )
            }
            tv.con
          }
        }))
      },
      # Define quadvariate constraints
      if(idx.indvar > 3 & moment == 4){
        x123 <- combn(other.inds, 3)
        c(sapply(1:ncol(x123), function(nth.triple){
          other.ind1 <- x123[1, nth.triple]
          other.ind2 <- x123[2, nth.triple]
          other.ind3 <- x123[3, nth.triple]
          cn.other.ind1 <- scale(syn_df[, other.ind1], scale = FALSE)[, 1]
          cn.other.ind2 <- scale(syn_df[, other.ind2], scale = FALSE)[, 1]
          cn.other.ind3 <- scale(syn_df[, other.ind3], scale = FALSE)[, 1]
          quadvars <- mv_moment_4_df$vars
          row.idx <- grepl(other.ind1, quadvars, fixed = TRUE) &
                      grepl(other.ind2, quadvars, fixed = TRUE) &
                      grepl(other.ind3, quadvars, fixed = TRUE) &
                      grepl(name_ind, quadvars, fixed = TRUE)
          target_abcd <- mv_moment_4_df[row.idx, 'abcd']
          mean(cn.other.ind1 * cn.other.ind2 * cn.other.ind3 * cn.x) - target_abcd
        }))
      }
    ))
  }

  # Generate pseudo-data
  orig.mean <- mean_cov[mean_cov[,'variable'] == name_ind, 'mean']
  orig.variance <- mean_cov[mean_cov[,'variable'] == name_ind, 'variance']
  n <- nrow(syn_df)
  if(!(name_ind %in% numeric_var_names) & 
          target_mean == 0){
    cbind(syn_df, rep(0, n))
  } else if(!(name_ind %in% numeric_var_names) & 
      target_mean == 1){cbind(syn_df, rep(1, n))
    } else if(orig.variance == 0){
          cbind(syn_df, rep(orig.mean, n))
      } else{start <- if(name_ind %in% numeric_var_names){
                            c(mvrnorm(n, mu = 0, Sigma = 1, empirical = T))
                      } else if(target_mean > 0 & target_mean < 1){
                                  n.ones <- round(n * orig.mean)
                                  sample(c(rep(1, n.ones), 
                                          rep(0, n - n.ones)), n)
                        }
             vals <- lsqnonlin_2(fn, start,
                        options = list(tolx = 1e-10, tolg = 1e-10, maxeval = 100))
             cbind(syn_df, vals$x)
        }
}
