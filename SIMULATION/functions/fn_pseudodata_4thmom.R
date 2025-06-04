#---------------------------------------------------------------
# FUNCTION TO GENERATE PSEUDO-DATA (K = 4)
# INPUTS:
#   iter : iteration number
#   m : number of clusters
#   y_name : response variable name
#   uniform_cluster_size : cluster size
#   seed : seed assigned to particular iteration and scenario
#   names_ind_vars : covariate names
#   numeric_var_names : numeric covariate names
#   mean_cov : list of cluster univariate summary statistics
#   var_cov_mat : list of cluster variance-covariance matrices
#   mv_moment_3_4_bypair_df : bivariate 3rd and 4th central moments
#   mv_moment_3_4_by3_df : trivariate 3rd and 4th central moments
#   mv_moment_4_df : quadvariate 4th central moments
#
# OUTPUT: 
#   dataframe of pseudo-data
#---------------------------------------------------------------


fn_pseudodata_4thmom_glmm <- function(iter, m, y_name, uniform_cluster_size, seed, names_ind_vars, 
numeric_var_names, mean_cov, var_cov_mat, mv_moment_3_4_bypair_df = NULL,
 mv_moment_3_4_by3_df = NULL, mv_moment_4_df = NULL){
  set.seed(seed)  #for reproducibility

  # Generate pseudo-responses per cluster/group
  n_h <- rep(uniform_cluster_size, m)
  y_mean <- vapply(mean_cov, function(df) df[df$variable == y_name, 'std_mean'], numeric(1))
  y_syn <- lapply(seq_along(y_mean), function(i){
    n.events <- round(n_h[i] * y_mean[i])
    sample(c(rep(1, n.events),
      rep(0, n_h[i] - n.events)), n_h[i])
    })
  
  # Generate pseudo-data per cluster
  pseudo_data_4th_mom <- lapply(1:m, function(group_num){
    # Use y_syn as first column
    syn_df_4th_mom <- data.frame(y_syn[[group_num]])
    names(syn_df_4th_mom) <- y_name

    # Continue with the covariates
    for(name_ind in names_ind_vars){
      syn_df_4th_mom <- gen_pseudo(moment = 4, y_syn[[group_num]], name_ind, numeric_var_names, 
          syn_df_4th_mom, mean_cov[[group_num]], var_cov_mat[[group_num]], 
          mv_moment_3_4_bypair_df[[group_num]], mv_moment_3_4_by3_df[[group_num]],
          mv_moment_4_df[[group_num]])
      names(syn_df_4th_mom)[length(names(syn_df_4th_mom))] <- name_ind
      print(paste0('iter ', iter, '; ', 'grp ', group_num, '; ', 
      'syn_df_4th_mom; ', name_ind, ' is finished'))
    }
    syn_df_4th_mom$level2 <- group_num
    syn_df_4th_mom
  })
  
  # Unstandardize numeric pseudo-data
  unstd_syn_df_4th_mom <- lapply(1:m, function(group_num){
    df <- pseudo_data_4th_mom[[group_num]]
    mean_cov_ <- mean_cov[[group_num]]
    as.data.frame(sapply(names(df), function(name){
      x.variance <- mean_cov_[mean_cov_[,'variable'] == name, 'variance']
      x.mean <- mean_cov_[mean_cov_[,'variable'] == name, 'mean']
      if(name %in% numeric_var_names){
        df[, name] * sqrt(x.variance) + x.mean
        } else{df[, name]}
  }))})
  
  return(unstd_syn_df_4th_mom)
}