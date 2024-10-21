# Prepare functions to compute central moments
mv_moment_3_4_bypair <- function(a,b,n){
  a2b <- sum((a-mean(a))^2 * (b-mean(b)))/n
  ab2 <- sum((a-mean(a)) * (b-mean(b))^2)/n
  ab3 <- sum((a-mean(a)) * (b-mean(b))^3)/n
  a2b2 <- sum((a-mean(a))^2 * (b-mean(b))^2)/n
  a3b <- sum((a-mean(a))^3 * (b-mean(b)))/n
  return(list(a2b=a2b, ab2=ab2, ab3=ab3, a2b2=a2b2, a3b=a3b))
}
mv_moment_3_4_by3 <- function(a,b,c,n){
  abc <- sum((a-mean(a)) * (b-mean(b)) * (c-mean(c)))/n
  a2bc <- sum((a-mean(a))^2 * (b-mean(b)) * (c-mean(c)))/n
  ab2c <- sum((a-mean(a)) * (b-mean(b))^2 * (c-mean(c)))/n
  abc2 <- sum((a-mean(a)) * (b-mean(b)) * (c-mean(c))^2)/n
  return(list(abc=abc, a2bc=a2bc, ab2c=ab2c, abc2=abc2))
}
mv_moment_4 <- function(a,b,c,d,n){
  abcd <- sum((a-mean(a)) * (b-mean(b)) * (c-mean(c)) * (d-mean(d)))/n
  abcd
}

fn_compute_summary <- function(data, grouping_name, numeric_var_names){
  data <- as.data.frame(cbind(model.matrix(~-1+., data[,!(names(data) == grouping_name)]),
                data[, grouping_name]))
  names(data)[length(names(data))] <- grouping_name
  wxyz <- combn(names(data)[!(names(data) == grouping_name)], 4) 
  xyz <- combn(names(data)[!(names(data) == grouping_name)], 3) 
  xy <- combn(names(data)[!(names(data) == grouping_name)], 2) 
  
  # Prepare summary statistics (data provider task) ------------------
  
  # Break up data per group
  group_data_design_df <- data %>% split(f = as.factor(.[,grouping_name]))
  m <- length(group_data_design_df)
  n_i <- data %>% count(.data[[grouping_name]]) %>% dplyr::select(n)
  
  summary_stats <- lapply(1:m, function(group_num){
    data.frame(variable = names(data)[names(data) != grouping_name],
               type = ifelse(names(data)[names(data) != grouping_name] %in% numeric_var_names, 'num', 'bin'),
               n = n_i[group_num,],
               mean = apply(group_data_design_df[[group_num]][, names(data) != grouping_name], 2, mean, na.rm = TRUE),
               variance = apply(group_data_design_df[[group_num]][, names(data) != grouping_name], 2, var),
               target_3_moment = apply(group_data_design_df[[group_num]][, names(data) != grouping_name], 2, function(x) sum((x - mean(x))^3)/n_i[group_num,]),
               target_4_moment = apply(group_data_design_df[[group_num]][, names(data) != grouping_name], 2, function(x) sum((x - mean(x))^4)/n_i[group_num,]),
               row.names = NULL)
  })
  
  var_cov_mat <- lapply(1:m, function(group_num) cov(group_data_design_df[[group_num]][, names(data) != grouping_name]))
  
  mv_moment_3_4_bypair_df <- lapply(1:m, function(group_num){
    data.frame(vars = apply(xy, 2, function(x) paste0(t(x)[, 1], "_", t(x)[, 2])),
               a2b = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(group_data_design_df[[group_num]][, xy[1,x]], group_data_design_df[[group_num]][, xy[2,x]], n_i[group_num,])$a2b),
               ab2 = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(group_data_design_df[[group_num]][, xy[1,x]], group_data_design_df[[group_num]][, xy[2,x]], n_i[group_num,])$ab2),
               a3b = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(group_data_design_df[[group_num]][, xy[1,x]], group_data_design_df[[group_num]][, xy[2,x]], n_i[group_num,])$a3b),
               a2b2 = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(group_data_design_df[[group_num]][, xy[1,x]], group_data_design_df[[group_num]][, xy[2,x]], n_i[group_num,])$a2b2),
               ab3 = sapply(1:dim(xy)[2], function(x) mv_moment_3_4_bypair(group_data_design_df[[group_num]][, xy[1,x]], group_data_design_df[[group_num]][, xy[2,x]], n_i[group_num,])$ab3)
    )
  })
  
  mv_moment_3_4_by3_df <- lapply(1:m, function(group_num){
    data.frame(vars = apply(xyz, 2, function(x) paste0(t(x)[, 1], "_", t(x)[, 2],"_", t(x)[, 3])),
               abc = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(group_data_design_df[[group_num]][, xyz[1,x]], group_data_design_df[[group_num]][, xyz[2,x]], group_data_design_df[[group_num]][, xyz[3,x]], n_i[group_num,])$abc),
               a2bc = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(group_data_design_df[[group_num]][, xyz[1,x]], group_data_design_df[[group_num]][, xyz[2,x]], group_data_design_df[[group_num]][, xyz[3,x]], n_i[group_num,])$a2bc),
               ab2c = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(group_data_design_df[[group_num]][, xyz[1,x]], group_data_design_df[[group_num]][, xyz[2,x]], group_data_design_df[[group_num]][, xyz[3,x]], n_i[group_num,])$ab2c),
               abc2 = sapply(1:dim(xyz)[2], function(x) mv_moment_3_4_by3(group_data_design_df[[group_num]][, xyz[1,x]], group_data_design_df[[group_num]][, xyz[2,x]], group_data_design_df[[group_num]][, xyz[3,x]], n_i[group_num,])$abc2))
  })
  
  mv_moment_4_df <- lapply(1:m, function(group_num){
    data.frame(vars = apply(wxyz, 2, function(x) paste0(t(x)[, 1], "_", t(x)[, 2],"_", t(x)[, 3], "_", t(x)[, 4])),
               abcd = sapply(1:dim(wxyz)[2], function(x) mv_moment_4(group_data_design_df[[group_num]][, wxyz[1,x]], group_data_design_df[[group_num]][, wxyz[2,x]], group_data_design_df[[group_num]][, wxyz[3,x]], group_data_design_df[[group_num]][, wxyz[4,x]], n_i[group_num,]))
    )
  })
  
  # Compute central moments of standardized numeric variables (data analyst task) ------------------
  summary_stats <- lapply(1:m, function(group_num){
    cbind(summary_stats[[group_num]],
          std_mean = apply(summary_stats[[group_num]][,c('type','mean')], 1, function(x) ifelse(x[1] == 'num', 0, as.numeric(x[2]))),
          std_var = apply(summary_stats[[group_num]][,c('type','variance')], 1, function(x) ifelse(x[1] == 'num' & as.numeric(x[2]) > 0, 1, as.numeric(x[2]))),
          std_target_3_moment = apply(summary_stats[[group_num]][,c('type','variance','target_3_moment')], 1, function(x) ifelse(x[1] == 'num' & as.numeric(x[2]) > 0, as.numeric(x[3])/sqrt(as.numeric(x[2]))^3, as.numeric(x[3]))),
          std_target_4_moment = apply(summary_stats[[group_num]][,c('type','variance','target_4_moment')], 1, function(x) ifelse(x[1] == 'num' & as.numeric(x[2]) > 0, as.numeric(x[3])/sqrt(as.numeric(x[2]))^4, as.numeric(x[3]))))
  })
  
  for(numeric_var_name in numeric_var_names){
    var_cov_mat <- lapply(1:m, function(group_num){
      sd_orig <- sqrt(var_cov_mat[[group_num]][numeric_var_name, numeric_var_name])
      var_cov_mat[[group_num]][numeric_var_name, ] <- ifelse(rep(sd_orig, dim(var_cov_mat[[group_num]])[2]) > 0, var_cov_mat[[group_num]][numeric_var_name, ]/sd_orig, 0)
      var_cov_mat[[group_num]][, numeric_var_name] <- ifelse(rep(sd_orig, dim(var_cov_mat[[group_num]])[1]) > 0, var_cov_mat[[group_num]][, numeric_var_name]/sd_orig, 0)
      var_cov_mat[[group_num]][numeric_var_name, numeric_var_name] <- ifelse(sd_orig > 0, 1, 0)
      var_cov_mat[[group_num]]
    })
  }
  
  for(numeric_var_name in numeric_var_names){
    mv_moment_3_4_bypair_df <- lapply(1:m, function(group_num){
      var_orig <- summary_stats[[group_num]][summary_stats[[group_num]][,'variable'] == numeric_var_name, 'variance']
      mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'a2b'] <- ifelse(rep(var_orig, sum(xy[1, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'a2b']/var_orig, 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'a2b'] <- ifelse(rep(var_orig, sum(xy[2, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'a2b']/sqrt(var_orig), 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'ab2'] <- ifelse(rep(var_orig, sum(xy[1, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'ab2']/sqrt(var_orig), 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'ab2'] <- ifelse(rep(var_orig, sum(xy[2, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'ab2']/var_orig, 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'a3b'] <- ifelse(rep(var_orig, sum(xy[1, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'a3b']/sqrt(var_orig)^3, 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'a3b'] <- ifelse(rep(var_orig, sum(xy[2, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'a3b']/sqrt(var_orig), 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'ab3'] <- ifelse(rep(var_orig, sum(xy[1, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'ab3']/sqrt(var_orig), 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'ab3'] <- ifelse(rep(var_orig, sum(xy[2, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'ab3']/sqrt(var_orig)^3, 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'a2b2'] <- ifelse(rep(var_orig, sum(xy[1, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[1, ] == numeric_var_name, 'a2b2']/var_orig, 0)
      mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'a2b2'] <- ifelse(rep(var_orig, sum(xy[2, ] == numeric_var_name)) > 0, mv_moment_3_4_bypair_df[[group_num]][xy[2, ] == numeric_var_name, 'a2b2']/var_orig, 0)
      mv_moment_3_4_bypair_df[[group_num]]
    })
  }
  
  for(numeric_var_name in numeric_var_names){
    mv_moment_3_4_by3_df <- lapply(1:m, function(group_num){
      var_orig <- summary_stats[[group_num]][summary_stats[[group_num]][,'variable'] == numeric_var_name, 'variance']
      mv_moment_3_4_by3_df[[group_num]][grepl(numeric_var_name, mv_moment_3_4_by3_df[[group_num]][,'vars']), 'abc'] <- ifelse(rep(var_orig, sum(grepl(numeric_var_name, mv_moment_3_4_by3_df[[group_num]][,'vars']))) > 0, mv_moment_3_4_by3_df[[group_num]][grepl(numeric_var_name, mv_moment_3_4_by3_df[[group_num]][,'vars']), 'abc']/sqrt(var_orig), 0)
      mv_moment_3_4_by3_df[[group_num]][xyz[1, ] == numeric_var_name, c('ab2c', 'abc2')] <- if(var_orig > 0){mv_moment_3_4_by3_df[[group_num]][xyz[1, ] == numeric_var_name, c('ab2c', 'abc2')]/sqrt(var_orig)} else{matrix(0, ncol = 2, nrow = sum(xyz[1, ] == numeric_var_name))} 
      mv_moment_3_4_by3_df[[group_num]][xyz[1, ] == numeric_var_name, 'a2bc'] <- ifelse(rep(var_orig, sum(xyz[1, ] == numeric_var_name)) > 0, mv_moment_3_4_by3_df[[group_num]][xyz[1, ] == numeric_var_name, 'a2bc']/var_orig, 0)
      mv_moment_3_4_by3_df[[group_num]][xyz[2, ] == numeric_var_name, c('a2bc', 'abc2')] <- if(var_orig > 0){mv_moment_3_4_by3_df[[group_num]][xyz[2, ] == numeric_var_name, c('a2bc', 'abc2')]/sqrt(var_orig)} else{matrix(0, ncol = 2, nrow = sum(xyz[2, ] == numeric_var_name))} 
      mv_moment_3_4_by3_df[[group_num]][xyz[2, ] == numeric_var_name, 'ab2c'] <- ifelse(rep(var_orig, sum(xyz[2, ] == numeric_var_name)) > 0, mv_moment_3_4_by3_df[[group_num]][xyz[2, ] == numeric_var_name, 'ab2c']/var_orig, 0)
      mv_moment_3_4_by3_df[[group_num]][xyz[3, ] == numeric_var_name, c('a2bc', 'ab2c')] <- if(var_orig > 0){mv_moment_3_4_by3_df[[group_num]][xyz[3, ] == numeric_var_name, c('a2bc', 'ab2c')]/sqrt(var_orig)} else{matrix(0, ncol = 2, nrow = sum(xyz[3, ] == numeric_var_name))}
      mv_moment_3_4_by3_df[[group_num]][xyz[3, ] == numeric_var_name, 'abc2'] <- ifelse(rep(var_orig, sum(xyz[3, ] == numeric_var_name)) > 0, mv_moment_3_4_by3_df[[group_num]][xyz[3, ] == numeric_var_name, 'abc2']/var_orig, 0)
      mv_moment_3_4_by3_df[[group_num]]
    })
  }
  
  for(numeric_var_name in numeric_var_names){
    mv_moment_4_df <- lapply(1:m, function(group_num){
      var_orig <- summary_stats[[group_num]][summary_stats[[group_num]][,'variable'] == numeric_var_name, 'variance']
      mv_moment_4_df[[group_num]][grepl(numeric_var_name, mv_moment_4_df[[group_num]][,'vars']), 'abcd'] <- ifelse(rep(var_orig, sum(grepl(numeric_var_name, mv_moment_4_df[[group_num]][,'vars']))) > 0, mv_moment_4_df[[group_num]][grepl(numeric_var_name, mv_moment_4_df[[group_num]][,'vars']), 'abcd']/sqrt(var_orig), 0)
      mv_moment_4_df[[group_num]]
    }) 
  }
  
  return(list(summary_stats, var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df, mv_moment_4_df))
  
  
  
}