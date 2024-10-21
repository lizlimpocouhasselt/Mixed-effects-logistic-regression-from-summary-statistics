fn_pseudodata_3rdmom_glmm <- function(y_name, names_ind_vars, numeric_var_names, summary_stats, var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df, mv_moment_4_df){
  m <- length(summary_stats)
  n_h <- unlist(lapply(summary_stats, function(df) df[1,'n']))
  y_mean <- lapply(1:m, function(group_num){summary_stats[[group_num]][summary_stats[[group_num]][,'variable'] == y_name, 'std_mean']})
  
  y_syn <- lapply(1:m, function(group_num){sample(c(rep(1, round(n_h[group_num] * y_mean[[group_num]])), rep(0, n_h[group_num] - round(n_h[group_num] * y_mean[[group_num]]))), n_h[[group_num]])})
  
  pseudo_data_3rd_mom <- lapply(1:m, function(group_num){
    syn_df_3rd_mom <- data.frame(y_syn[[group_num]])
    names(syn_df_3rd_mom) <- y_name
    for(name_ind in names_ind_vars){
      syn_df_3rd_mom <- gen_pseudo(moment = 3, y_syn[[group_num]], name_ind, numeric_var_names, syn_df_3rd_mom, summary_stats[[group_num]], var_cov_mat[[group_num]], mv_moment_3_4_bypair_df[[group_num]], mv_moment_3_4_by3_df[[group_num]], mv_moment_4_df[[group_num]])
      names(syn_df_3rd_mom)[length(names(syn_df_3rd_mom))] <- name_ind
      print(paste0('grp ', group_num, '; ', 'syn_df_3rd_mom; ', name_ind, ' is finished'))
    }
    syn_df_3rd_mom$level2 <- group_num 
    syn_df_3rd_mom
  })
  
  unstd_syn_df_3rd_mom <- lapply(1:m, function(group_num){as.data.frame(sapply(names(pseudo_data_3rd_mom[[group_num]]), function(name_syn_df){
    if(name_syn_df %in% numeric_var_names){pseudo_data_3rd_mom[[group_num]][, name_syn_df] * sqrt(summary_stats[[group_num]][summary_stats[[group_num]][,'variable'] == name_syn_df,'variance']) + summary_stats[[group_num]][summary_stats[[group_num]][,'variable'] == name_syn_df,'mean']} else{pseudo_data_3rd_mom[[group_num]][, name_syn_df]}
  }))})
  
  unstd_syn_df_3rd_mom
}