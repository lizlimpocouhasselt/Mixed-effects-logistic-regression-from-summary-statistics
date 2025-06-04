#---------------------------------------------------------------
# FUNCTION TO GENERATE PSEUDO-DATA (K = 3)
# INPUTS:
#   y_name : response variable name
#   names_ind_vars : covariate names
#   numeric_var_names : numeric covariate names
#   mean_cov : list of cluster univariate summary statistics
#   var_cov_mat : list of cluster variance-covariance matrices
#   mv_moment_3_4_bypair_df : list of bivariate 3rd and 4th central moments
#   mv_moment_3_4_by3_df : list of trivariate 3rd and 4th central moments
#
# OUTPUT: 
#   dataframe of pseudo-data
#---------------------------------------------------------------

fn_pseudodata_3rdmom_glmm <- function(y_name, names_ind_vars, numeric_var_names, mean_cov,
 var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df){

  # Generate pseudo-responses per cluster/group
  n_h <- unlist(lapply(mean_cov, function(df) df[1,'n']))
  y_mean <- vapply(mean_cov, function(df) df[df$variable == y_name, 'std_mean'], numeric(1))
  y_syn <- lapply(seq_along(y_mean), function(i){
    n.events <- round(n_h[i] * y_mean[i])
    sample(c(rep(1, n.events),
      rep(0, n_h[i] - n.events)), n_h[i])
    })
  
  # Generate pseudo-data per cluster
  m <- length(mean_cov)
  pseudo_data_3rd_mom <- lapply(1:m, function(group_num){
    # Use y_syn as first column
    syn_df_3rd_mom <- data.frame(y_syn[[group_num]])
    names(syn_df_3rd_mom) <- y_name

    # Continue with the covariates
    for(name_ind in names_ind_vars){
      syn_df_3rd_mom <- gen_pseudo(moment = 3, y_syn[[group_num]], name_ind, numeric_var_names, 
          syn_df_3rd_mom, mean_cov[[group_num]], var_cov_mat[[group_num]],
           mv_moment_3_4_bypair_df[[group_num]], mv_moment_3_4_by3_df[[group_num]])
      names(syn_df_3rd_mom)[ncol(syn_df_3rd_mom)] <- name_ind
      print(paste0('grp ', group_num, '; ', 'syn_df_3rd_mom; ', name_ind, ' is finished'))
    }
    syn_df_3rd_mom$level2 <- group_num
    syn_df_3rd_mom
  })
  
  # Unstandardize numeric pseudo-data
  unstd_syn_df_3rd_mom <- lapply(1:m, function(group_num){
    df <- pseudo_data_3rd_mom[[group_num]]
    mean_cov_ <- mean_cov[[group_num]]
    as.data.frame(sapply(names(df), function(name){
      x.variance <- mean_cov_[mean_cov_[, 'variable'] == name, 'variance']
      x.mean <- mean_cov_[mean_cov_[, 'variable'] == name, 'mean']
      if(name %in% numeric_var_names){
        df[, name] * sqrt(x.variance) + x.mean
        } else{df[, name]}
  }))})
  ## Save pseudo-data in the interest of time
  pseudodata_3rdmom <- unstd_syn_df_3rd_mom
  save(pseudodata_3rdmom, file = file.path("DEMO", "ps", "pseudodata_3rdmom.RData"))
  
  return(pseudodata_3rdmom)
}