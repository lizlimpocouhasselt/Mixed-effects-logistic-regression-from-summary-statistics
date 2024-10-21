gen_pseudo <- function(moment, y_syn, name_ind, numeric_var_names, syn_df, summary_stats, var_cov_mat, mv_moment_3_4_bypair_df = NULL, mv_moment_3_4_by3_df = NULL, mv_moment_4_df = NULL){
  target_mean <- summary_stats[summary_stats[,'variable'] == name_ind, 'std_mean']
  target_var <- summary_stats[summary_stats[,'variable'] == name_ind, 'std_var']
  target_cov_y_x <- var_cov_mat[y_name, name_ind]
  target_3_moment <- summary_stats[summary_stats[,'variable'] == name_ind, 'std_target_3_moment']
  target_4_moment <- summary_stats[summary_stats[,'variable'] == name_ind, 'std_target_4_moment']
  
  n <- length(y_syn)
  
  fn <- function(x){
    obj <- c(
      mean(x) - target_mean,
      var(x) - target_var,
      cov(y_syn, x) - target_cov_y_x
    )
    if(moment >= 3){
      obj <- c(obj, sum((x - mean(x))^3)/n - target_3_moment)
    }
    if(moment == 4){
      obj <- c(obj, sum((x - mean(x))^4)/n - target_4_moment)
    }
    
    if(which(names_ind_vars == name_ind) > 1){
      obj <- c(obj,
               c(sapply(names(syn_df)[2:(length(names(syn_df)))], function(other_ind){
                 target_cov_x1_x2 <- var_cov_mat[other_ind, name_ind]
                 cov(syn_df[,other_ind],x) - target_cov_x1_x2
               })))
    }
    if(which(names_ind_vars == name_ind) > 1 & moment >= 3){
      obj <- c(obj,
               c(sapply(names(syn_df)[2:(length(names(syn_df)))], 
                        function(other_ind){
                          target_a2b <- mv_moment_3_4_bypair_df[mv_moment_3_4_bypair_df[,'vars'] == paste0(other_ind,'_',name_ind), 'a2b']
                          target_ab2 <- mv_moment_3_4_bypair_df[mv_moment_3_4_bypair_df[,'vars'] == paste0(other_ind,'_',name_ind), 'ab2']
                          
                          return(
                            c(sum((syn_df[,other_ind] - mean(syn_df[,other_ind]))^2 * (x - mean(x)))/n - target_a2b,
                              sum((syn_df[,other_ind] - mean(syn_df[,other_ind])) * (x - mean(x))^2)/n - target_ab2))
                        })))
    }
    if(which(names_ind_vars == name_ind) > 1 & moment == 4){
      obj <- c(obj,
               c(sapply(names(syn_df)[2:(length(names(syn_df)))], 
                        function(other_ind){
                          target_a3b <- mv_moment_3_4_bypair_df[mv_moment_3_4_bypair_df[,'vars'] == paste0(other_ind,'_',name_ind), 'a3b']
                          target_ab3 <- mv_moment_3_4_bypair_df[mv_moment_3_4_bypair_df[,'vars'] == paste0(other_ind,'_',name_ind), 'ab3']
                          target_a2b2 <- mv_moment_3_4_bypair_df[mv_moment_3_4_bypair_df[,'vars'] == paste0(other_ind,'_',name_ind), 'a2b2']
                          return(
                            c(sum((syn_df[,other_ind] - mean(syn_df[,other_ind]))^3 * (x - mean(x)))/n - target_a3b,
                              sum((syn_df[,other_ind] - mean(syn_df[,other_ind])) * (x - mean(x))^3)/n - target_ab3,
                              sum((syn_df[,other_ind] - mean(syn_df[,other_ind]))^2 * (x - mean(x))^2)/n - target_a2b2
                            ))})))
    }
    
    if(which(names_ind_vars == name_ind) > 2 & moment >= 3){
      xy_nodep <- combn(names(syn_df)[-1], 2)
      obj <- c(obj,
               c(sapply(1:dim(xy_nodep)[2], function(xy){
                 c(sum((syn_df[,xy_nodep[1,xy]] - mean(syn_df[,xy_nodep[1,xy]])) * (syn_df[,xy_nodep[2,xy]] - mean(syn_df[,xy_nodep[2,xy]])) * (x - mean(x)))/n - mv_moment_3_4_by3_df[mv_moment_3_4_by3_df[,'vars'] == paste0(xy_nodep[1,xy],'_',xy_nodep[2,xy],'_',name_ind), 'abc'])})))
    }
    if(which(names_ind_vars == name_ind) > 2 & moment == 4){
      xy_nodep <- combn(names(syn_df)[-1], 2)
      obj <- c(obj,
               c(sapply(1:dim(xy_nodep)[2], function(xy){
                 c(sum((syn_df[,xy_nodep[1,xy]] - mean(syn_df[,xy_nodep[1,xy]]))^2 * (syn_df[,xy_nodep[2,xy]] - mean(syn_df[,xy_nodep[2,xy]])) * (x - mean(x)))/n - mv_moment_3_4_by3_df[mv_moment_3_4_by3_df[,'vars'] == paste0(xy_nodep[1,xy],'_',xy_nodep[2,xy],'_',name_ind), 'a2bc'],
                   sum((syn_df[,xy_nodep[1,xy]] - mean(syn_df[,xy_nodep[1,xy]])) * (syn_df[,xy_nodep[2,xy]] - mean(syn_df[,xy_nodep[2,xy]]))^2 * (x - mean(x)))/n - mv_moment_3_4_by3_df[mv_moment_3_4_by3_df[,'vars'] == paste0(xy_nodep[1,xy],'_',xy_nodep[2,xy],'_',name_ind), 'ab2c'],
                   sum((syn_df[,xy_nodep[1,xy]] - mean(syn_df[,xy_nodep[1,xy]])) * (syn_df[,xy_nodep[2,xy]] - mean(syn_df[,xy_nodep[2,xy]])) * (x - mean(x))^2)/n - mv_moment_3_4_by3_df[mv_moment_3_4_by3_df[,'vars'] == paste0(xy_nodep[1,xy],'_',xy_nodep[2,xy],'_',name_ind), 'abc2'])})))
    }
    
    if(which(names_ind_vars == name_ind) > 3 & moment == 4){
      xyz_nodep <- combn(names(syn_df)[-1], 3)
      obj <- c(obj,
               c(sapply(1:dim(xyz_nodep)[2], function(xyz){
                 sum((syn_df[,xyz_nodep[1,xyz]] - mean(syn_df[,xyz_nodep[1,xyz]])) * 
                     (syn_df[,xyz_nodep[2,xyz]] - mean(syn_df[,xyz_nodep[2,xyz]])) * 
                     (syn_df[,xyz_nodep[3,xyz]] - mean(syn_df[,xyz_nodep[3,xyz]])) * 
                     (x - mean(x))
                 )/n - mv_moment_4_df[mv_moment_4_df[,'vars'] == paste0(xyz_nodep[1,xyz],'_',xyz_nodep[2,xyz],'_',xyz_nodep[3,xyz],'_',name_ind), 'abcd']})))
    }
    obj
  }
  
  if(!(name_ind %in% numeric_var_names) & target_mean == 0) {cbind(syn_df, rep(0, n))} else if(!(name_ind %in% numeric_var_names) & target_mean == 1) {cbind(syn_df, rep(1, n))} else if(summary_stats[summary_stats[,'variable'] == name_ind, 'variance'] == 0) {cbind(syn_df, rep(summary_stats[summary_stats[,'variable'] == name_ind, 'mean'], n))} else{
    start <- if(name_ind %in% numeric_var_names){
      c(mvrnorm(n, mu = 0, Sigma = 1, empirical = T))
    } else if(target_mean > 0 & target_mean < 1){
      sample(c(rep(1, round(n * target_mean)), rep(0, n - round(n * target_mean))), n)
    } 
    
    vals <- lsqnonlin_2(fn, start,
                        options = list(tolx = 1e-10, tolg = 1e-10, maxeval = 100))
    
    cbind(syn_df, vals$x)
  }
}