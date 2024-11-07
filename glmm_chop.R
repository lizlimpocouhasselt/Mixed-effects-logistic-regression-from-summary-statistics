#-----------------------------------------
# MIXED EFFECTS LOGISTIC REGRESSION
# CHOP DATA DEMO
#-----------------------------------------



# Empty the environment
rm(list=ls(all=TRUE))

# Load libraries / functions
library(medicaldata)
library(skimr)
library(dplyr)
library(lme4)
library(stringr)
library(pracma)
library(MASS)
library(ggplot2)
library(reshape)
library(cowplot)
source('fn_compute_summary.R')
source('lsqnonlin_2.R')
source('gen_pseudo.R')
source('fn_pseudodata_2ndmom.R')
source('fn_pseudodata_3rdmom.R')
source('fn_pseudodata_4thmom.R')





#---------------- DATA PROVIDER TASK ----------------

# I. Load and preprocess data
##Include only clinics with:
## a. complete cases
## b. valid values
## c. more than 1 patient record
data('covid_testing')
data <- covid_testing %>%
  filter(complete.cases(.), 
         result %in% c('negative', 'positive'),
         patient_class %in% c('recurring outpatient', 
                              'outpatient', 'emergency', 'inpatient')) %>%
  mutate(patient_class = str_replace_all(patient_class, "recurring outpatient", "outpatient")) %>%
  mutate(patient_class = factor(patient_class, levels = c("inpatient", "emergency", "outpatient"))) %>%
  dplyr::select(result, pan_day, age, gender, clinic_name, demo_group, payor_group, patient_class, drive_thru_ind) %>%
  group_by(clinic_name) %>% filter(n() > 1) %>% as_tibble()

# Standardize numeric variables ------
data <- data %>%
  mutate(across(where(is.numeric), ~ scale(.)[, 1], .names = "std_{.col}"))

# Identify variable type
numeric_var_names <- data %>% dplyr::select(where(is.numeric)) %>% names()




# II. Compute summary statistics
summary_info <- fn_compute_summary(data, 'clinic_name', numeric_var_names)
summary_stats <- summary_info[[1]]
var_cov_mat <- summary_info[[2]]
mv_moment_3_4_bypair_df <- summary_info[[3]]
mv_moment_3_4_by3_df <- summary_info[[4]]
mv_moment_4_df <- summary_info[[5]]

# Merge all dataframes into one
summary_stats_one <- do.call(rbind, summary_stats)
row.names(summary_stats_one) <- NULL
summary_stats_one$clinic_name <- rep(names(summary_stats), 
                                     each = nrow(summary_stats[[1]]))
var_cov_mat_one <- as.data.frame(do.call(rbind, var_cov_mat))
var_cov_mat_one$clinic_name <- rep(names(var_cov_mat), 
                                   each = nrow(var_cov_mat[[1]]))
mv_moment_3_4_bypair_df_one <- do.call(rbind, mv_moment_3_4_bypair_df)
row.names(mv_moment_3_4_bypair_df_one) <- NULL
mv_moment_3_4_bypair_df_one$clinic_name <- rep(names(mv_moment_3_4_bypair_df), 
                                     each = nrow(mv_moment_3_4_bypair_df[[1]]))
mv_moment_3_4_by3_df_one <- do.call(rbind, mv_moment_3_4_by3_df)
row.names(mv_moment_3_4_by3_df_one) <- NULL
mv_moment_3_4_by3_df_one$clinic_name <- rep(names(mv_moment_3_4_by3_df), 
                                               each = nrow(mv_moment_3_4_by3_df[[1]]))
mv_moment_4_df_one <- do.call(rbind, mv_moment_4_df)
row.names(mv_moment_4_df_one) <- NULL
mv_moment_4_df_one$clinic_name <- rep(names(mv_moment_4_df), 
                                            each = nrow(mv_moment_4_df[[1]]))

# Save summary statistics and preprocessed data
write.csv(data, file = "actual_data_preprocessed_chop.csv")
write.csv(summary_stats_one, file = "summary_stats_chop.csv")
write.csv(var_cov_mat_one, file = "var_cov_mat_chop.csv")
write.csv(mv_moment_3_4_bypair_df_one, file = "mv_moment_3_4_bypair_df_chop.csv")
write.csv(mv_moment_3_4_by3_df_one, file = "mv_moment_3_4_by3_df_chop.csv")
write.csv(mv_moment_4_df_one, file = "mv_moment_4_df_chop.csv")



#---------------- DATA ANALYST TASK ----------------

# I. Generate pseudo-data
## A. Load summary data
id_summary_stats <- "1QvRKaP6APPyoI4Nt5AlVFUxdlhiuP-y5"
summary_stats_one <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_summary_stats))
summary_stats <- summary_stats_one %>% 
  dplyr::select(-1) %>% 
  split(f = as.factor(.$clinic_name)) %>%
  lapply(function(df){
    df[-ncol(df)]
  })
id_var_cov_mat <- "1QuJp4NYJzP83L-_4a5alpIZ8vasUV3sW"
var_cov_mat_one <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_var_cov_mat))
var_cov_mat <- var_cov_mat_one %>% 
  dplyr::select(-1) %>%
  split(f = as.factor(.$clinic_name)) %>%
  lapply(function(df) {
    rownames(df) <- colnames(df)[-ncol(df)]
    return(as.matrix(df[-ncol(df)]))
  })
id_mv_moment_3_4_bypair_df <- "1QxD5ncIfskJcQ9SWHW6W9yeRrvYRFtyZ"
mv_moment_3_4_bypair_df_one <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_mv_moment_3_4_bypair_df))
mv_moment_3_4_bypair_df <- mv_moment_3_4_bypair_df_one %>% 
  dplyr::select(-1) %>% 
  split(f = as.factor(.$clinic_name)) %>%
  lapply(function(df){
    df[-ncol(df)]
  })
id_mv_moment_3_4_by3_df <- "1R18kceggqu4YNO7I_xSHrRRigT1JrUGC"
mv_moment_3_4_by3_df_one <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_mv_moment_3_4_by3_df))
mv_moment_3_4_by3_df <- mv_moment_3_4_by3_df_one %>% 
  dplyr::select(-1) %>% 
  split(f = as.factor(.$clinic_name)) %>%
  lapply(function(df){
    df[-ncol(df)]
  })
id_mv_moment_4_df <- "1R3WDwBqO6vn261mH52Jo3VEGUGENW826"
mv_moment_4_df_one <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_mv_moment_4_df))
mv_moment_4_df <- mv_moment_4_df_one %>% 
  dplyr::select(-1) %>% 
  split(f = as.factor(.$clinic_name)) %>%
  lapply(function(df){
    df[-ncol(df)]
  })

## B. Specify model (optional)
formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind + std_pan_day + std_age

## C. Specify the variable names
y_name <- all.vars(formula)[1]
names_ind_vars <- all.vars(formula)[-1]

## D. Generate pseudo-data
set.seed(121314)
pseudodata_2ndmom <- fn_pseudodata_2ndmom_glmm(y_name, names_ind_vars, numeric_var_names, summary_stats, var_cov_mat)
pseudodata_3rdmom <- fn_pseudodata_3rdmom_glmm(y_name, names_ind_vars, numeric_var_names, summary_stats, var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df, mv_moment_4_df)
pseudodata_4thmom <- fn_pseudodata_4thmom_glmm(y_name, names_ind_vars, numeric_var_names, summary_stats, var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df, mv_moment_4_df)

## E. Compare summary statistics between actual and pseudo-data
### 1. Compute summary statistics of pseudo-data
summary_info_ps2 <- fn_compute_summary(do.call(rbind, pseudodata_2ndmom), 'level2', numeric_var_names[4:5])
summary_info_ps3 <- fn_compute_summary(do.call(rbind, pseudodata_3rdmom), 'level2', numeric_var_names[4:5])
summary_info_ps4 <- fn_compute_summary(do.call(rbind, pseudodata_4thmom), 'level2', numeric_var_names[4:5])

### 2. Compute absolute difference between pseudo- and actual data
cols <- c('mean', 'variance', 'target_3_moment', 'target_4_moment')
#### a. "3 laboratory"
clinic <- 2
diff_sumstat_ps2 <- abs(summary_info_ps2[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_2ndmom))) %>% .[,cols])
diff_sumstat_ps2$variable <- summary_info_ps2[[1]][[clinic]][,'variable']
diff_sumstat_long_ps2.2 <- melt(diff_sumstat_ps2)
names(diff_sumstat_long_ps2.2)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps2.2$sumstat <- factor(diff_sumstat_long_ps2.2$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps3 <- abs(summary_info_ps3[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_3rdmom))) %>% .[,cols])
diff_sumstat_ps3$variable <- summary_info_ps3[[1]][[clinic]][,'variable']
diff_sumstat_long_ps3.2 <- melt(diff_sumstat_ps3)
names(diff_sumstat_long_ps3.2)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps3.2$sumstat <- factor(diff_sumstat_long_ps3.2$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps4 <- abs(summary_info_ps4[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_4thmom))) %>% .[,cols])
diff_sumstat_ps4$variable <- summary_info_ps4[[1]][[clinic]][,'variable']
diff_sumstat_long_ps4.2 <- melt(diff_sumstat_ps4)
names(diff_sumstat_long_ps4.2)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps4.2$sumstat <- factor(diff_sumstat_long_ps4.2$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

#### b. "inpatient ward d"
clinic <- 21
diff_sumstat_ps2 <- abs(summary_info_ps2[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_2ndmom))) %>% .[,cols])
diff_sumstat_ps2$variable <- summary_info_ps2[[1]][[clinic]][,'variable']
diff_sumstat_long_ps2.21 <- melt(diff_sumstat_ps2)
names(diff_sumstat_long_ps2.21)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps2.21$sumstat <- factor(diff_sumstat_long_ps2.21$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps3 <- abs(summary_info_ps3[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_3rdmom))) %>% .[,cols])
diff_sumstat_ps3$variable <- summary_info_ps3[[1]][[clinic]][,'variable']
diff_sumstat_long_ps3.21 <- melt(diff_sumstat_ps3)
names(diff_sumstat_long_ps3.21)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps3.21$sumstat <- factor(diff_sumstat_long_ps3.21$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps4 <- abs(summary_info_ps4[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_4thmom))) %>% .[,cols])
diff_sumstat_ps4$variable <- summary_info_ps4[[1]][[clinic]][,'variable']
diff_sumstat_long_ps4.21 <- melt(diff_sumstat_ps4)
names(diff_sumstat_long_ps4.21)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps4.21$sumstat <- factor(diff_sumstat_long_ps4.21$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

#### c. "inpatient ward e"
clinic <- 22
diff_sumstat_ps2 <- abs(summary_info_ps2[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_2ndmom))) %>% .[,cols])
diff_sumstat_ps2$variable <- summary_info_ps2[[1]][[clinic]][,'variable']
diff_sumstat_long_ps2.22 <- melt(diff_sumstat_ps2)
names(diff_sumstat_long_ps2.22)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps2.22$sumstat <- factor(diff_sumstat_long_ps2.22$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps3 <- abs(summary_info_ps3[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_3rdmom))) %>% .[,cols])
diff_sumstat_ps3$variable <- summary_info_ps3[[1]][[clinic]][,'variable']
diff_sumstat_long_ps3.22 <- melt(diff_sumstat_ps3)
names(diff_sumstat_long_ps3.22)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps3.22$sumstat <- factor(diff_sumstat_long_ps3.22$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps4 <- abs(summary_info_ps4[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_4thmom))) %>% .[,cols])
diff_sumstat_ps4$variable <- summary_info_ps4[[1]][[clinic]][,'variable']
diff_sumstat_long_ps4.22 <- melt(diff_sumstat_ps4)
names(diff_sumstat_long_ps4.22)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps4.22$sumstat <- factor(diff_sumstat_long_ps4.22$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

#### d. "inpatient ward h"
clinic <- 25
diff_sumstat_ps2 <- abs(summary_info_ps2[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_2ndmom))) %>% .[,cols])
diff_sumstat_ps2$variable <- summary_info_ps2[[1]][[clinic]][,'variable']
diff_sumstat_long_ps2.25 <- melt(diff_sumstat_ps2)
names(diff_sumstat_long_ps2.25)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps2.25$sumstat <- factor(diff_sumstat_long_ps2.25$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps3 <- abs(summary_info_ps3[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_3rdmom))) %>% .[,cols])
diff_sumstat_ps3$variable <- summary_info_ps3[[1]][[clinic]][,'variable']
diff_sumstat_long_ps3.25 <- melt(diff_sumstat_ps3)
names(diff_sumstat_long_ps3.25)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps3.25$sumstat <- factor(diff_sumstat_long_ps3.25$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps4 <- abs(summary_info_ps4[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_4thmom))) %>% .[,cols])
diff_sumstat_ps4$variable <- summary_info_ps4[[1]][[clinic]][,'variable']
diff_sumstat_long_ps4.25 <- melt(diff_sumstat_ps4)
names(diff_sumstat_long_ps4.25)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps4.25$sumstat <- factor(diff_sumstat_long_ps4.25$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

#### e. "inpatient ward a"
clinic <- 18
diff_sumstat_ps2 <- abs(summary_info_ps2[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_2ndmom))) %>% .[,cols])
diff_sumstat_ps2$variable <- summary_info_ps2[[1]][[clinic]][,'variable']
diff_sumstat_long_ps2.18 <- melt(diff_sumstat_ps2)
names(diff_sumstat_long_ps2.18)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps2.18$sumstat <- factor(diff_sumstat_long_ps2.18$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps3 <- abs(summary_info_ps3[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_3rdmom))) %>% .[,cols])
diff_sumstat_ps3$variable <- summary_info_ps3[[1]][[clinic]][,'variable']
diff_sumstat_long_ps3.18 <- melt(diff_sumstat_ps3)
names(diff_sumstat_long_ps3.18)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps3.18$sumstat <- factor(diff_sumstat_long_ps3.18$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

diff_sumstat_ps4 <- abs(summary_info_ps4[[1]][[clinic]][,cols] - summary_stats[[clinic]] %>% filter(variable %in% names(do.call(rbind, pseudodata_4thmom))) %>% .[,cols])
diff_sumstat_ps4$variable <- summary_info_ps4[[1]][[clinic]][,'variable']
diff_sumstat_long_ps4.18 <- melt(diff_sumstat_ps4)
names(diff_sumstat_long_ps4.18)[2:3] <- c('sumstat','diff')
diff_sumstat_long_ps4.18$sumstat <- factor(diff_sumstat_long_ps4.18$sumstat, levels = c('mean', 'variance', 'target_3_moment', 'target_4_moment'))

### 3. Plot heatmaps
global.min <- min(diff_sumstat_long_ps2.2$diff,
                  diff_sumstat_long_ps3.2$diff,
                  diff_sumstat_long_ps4.2$diff,
                  diff_sumstat_long_ps2.18$diff,
                  diff_sumstat_long_ps3.18$diff,
                  diff_sumstat_long_ps4.18$diff,
                  diff_sumstat_long_ps2.21$diff,
                  diff_sumstat_long_ps3.21$diff,
                  diff_sumstat_long_ps4.21$diff,
                  diff_sumstat_long_ps2.22$diff,
                  diff_sumstat_long_ps3.22$diff,
                  diff_sumstat_long_ps4.22$diff,
                  diff_sumstat_long_ps2.25$diff,
                  diff_sumstat_long_ps3.25$diff,
                  diff_sumstat_long_ps4.25$diff)
global.max <- max(diff_sumstat_long_ps2.2$diff,
                  diff_sumstat_long_ps3.2$diff,
                  diff_sumstat_long_ps4.2$diff,
                  diff_sumstat_long_ps2.18$diff,
                  diff_sumstat_long_ps3.18$diff,
                  diff_sumstat_long_ps4.18$diff,
                  diff_sumstat_long_ps2.21$diff,
                  diff_sumstat_long_ps3.21$diff,
                  diff_sumstat_long_ps4.21$diff,
                  diff_sumstat_long_ps2.22$diff,
                  diff_sumstat_long_ps3.22$diff,
                  diff_sumstat_long_ps4.22$diff,
                  diff_sumstat_long_ps2.25$diff,
                  diff_sumstat_long_ps3.25$diff,
                  diff_sumstat_long_ps4.25$diff)
diff_plot_ps2.2 <- ggplot(diff_sumstat_long_ps2.2, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + scale_x_discrete(labels = c(expression(bar(x)),
                                expression(s^2),
                                expression(bar(mu)^3),
                                expression(bar(mu)^4))) +
  ggtitle('ps2') + theme(plot.title = element_text(hjust=0.5))
diff_plot_ps2.18 <- ggplot(diff_sumstat_long_ps2.18, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + scale_x_discrete(labels = c(expression(bar(x)),
                                                                                                 expression(s^2),
                                                                                                 expression(bar(mu)^3),
                                                                                                 expression(bar(mu)^4)))
diff_plot_ps2.21 <- ggplot(diff_sumstat_long_ps2.21, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + scale_x_discrete(labels = c(expression(bar(x)),expression(s^2),expression(bar(mu)^3),expression(bar(mu)^4)))
diff_plot_ps2.22 <- ggplot(diff_sumstat_long_ps2.22, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + scale_x_discrete(labels = c(expression(bar(x)),expression(s^2),expression(bar(mu)^3),expression(bar(mu)^4)))
diff_plot_ps2.25 <- ggplot(diff_sumstat_long_ps2.25, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + scale_x_discrete(labels = c(expression(bar(x)),
                                                                                                 expression(s^2),
                                                                                                 expression(bar(mu)^3),
                                                                                                 expression(bar(mu)^4)))

diff_plot_ps3.2 <- ggplot(diff_sumstat_long_ps3.2, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4))) +
  ggtitle('ps3') + theme(plot.title = element_text(hjust=0.5))
diff_plot_ps3.18 <- ggplot(diff_sumstat_long_ps3.18, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))
diff_plot_ps3.21 <- ggplot(diff_sumstat_long_ps3.21, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))
diff_plot_ps3.22 <- ggplot(diff_sumstat_long_ps3.22, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))
diff_plot_ps3.25 <- ggplot(diff_sumstat_long_ps3.25, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))

diff_plot_ps4.2 <- ggplot(diff_sumstat_long_ps4.2, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4))) +
  ggtitle('ps4') + theme(plot.title = element_text(hjust=0.5))
diff_plot_ps4.18 <- ggplot(diff_sumstat_long_ps4.18, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))
diff_plot_ps4.21 <- ggplot(diff_sumstat_long_ps4.21, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))
diff_plot_ps4.22 <- ggplot(diff_sumstat_long_ps4.22, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))
diff_plot_ps4.25 <- ggplot(diff_sumstat_long_ps4.25, aes(x=sumstat, y=variable, fill=diff)) + geom_tile() + xlab("") + ylab("") +
  scale_fill_gradient(limits = c(global.min, global.max)) +
  geom_text(aes(label = round(diff,3)), color = "white", size = 4) + theme(axis.text.y=element_blank(),
                                                                           axis.ticks.y=element_blank()) +
  scale_x_discrete(labels = c(expression(bar(x)),
                              expression(s^2),
                              expression(bar(mu)^3),
                              expression(bar(mu)^4)))

pg.all <- plot_grid(
  diff_plot_ps2.2 + theme(legend.position = "none"),
  diff_plot_ps3.2 + theme(legend.position = "none"),
  diff_plot_ps4.2 + theme(legend.position = "none"),
  diff_plot_ps2.21 + theme(legend.position = "none"),
  diff_plot_ps3.21 + theme(legend.position = "none"),
  diff_plot_ps4.21 + theme(legend.position = "none"),
  diff_plot_ps2.22 + theme(legend.position = "none"),
  diff_plot_ps3.22 + theme(legend.position = "none"),
  diff_plot_ps4.22 + theme(legend.position = "none"),
  diff_plot_ps2.25 + theme(legend.position = "none"),
  diff_plot_ps3.25 + theme(legend.position = "none"),
  diff_plot_ps4.25 + theme(legend.position = "none"),
  diff_plot_ps2.18 + theme(legend.position = "none"),
  diff_plot_ps3.18 + theme(legend.position = "none"),
  diff_plot_ps4.18 + theme(legend.position = "none"),
  ncol = 3, nrow = 5, byrow = T, rel_widths = c(1,0.7,0.7))
labels <- c("n=2",
            "n=35",
            "n=62",
            "n=105",
            "n=208")
label_plot <- ggplot() +
  geom_text(aes(x = 1, y = rev(c(1.75,2.5,3.2,4,4.75)), label = labels), hjust = 0) +  # Reverse y to align with rows
  theme_void() +  # Remove axes, gridlines, etc.
  theme(plot.margin = margin(t = 5, r = 2.5, b = 60, l = 2.5))

plot_grid(label_plot, pg.all, get_legend(diff_plot_ps2.2), ncol = 3,
          rel_widths = c(0.075,1,0.075))

# II. Estimate a mixed effects logistic regression model from pseudo-data
glmm_pseudo_2nd <- glmer(formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind + std_pan_day + std_age + (1|level2), data = do.call(rbind, pseudodata_2ndmom), family = binomial)
glmm_pseudo_3rd <- glmer(formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind + std_pan_day + std_age + (1|level2), data = do.call(rbind, pseudodata_3rdmom), family = binomial)
glmm_pseudo_4th <- glmer(formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind + std_pan_day + std_age + (1|level2), data = do.call(rbind, pseudodata_4thmom), family = binomial)





#---------------- COMPARISON WITH ACTUAL DATA ----------------

# Estimate a mixed effects logistic regression model from actual data
id_data <- "1QtTwfyKIvuPvg6uP1ZmZWp5vRIA8eHDS"
pooled_actual_data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_data))
glmm_actual <- glmer(ifelse(result == 'negative', 0, 1) ~ gender + patient_class + drive_thru_ind + scale(pan_day) + scale(age) + (1|clinic_name), data, family = binomial)




summary(glmm_pseudo_2nd)
summary(glmm_pseudo_3rd)
summary(glmm_pseudo_4th)
summary(glmm_actual)

AIC(glmm_pseudo_2nd)
AIC(glmm_pseudo_3rd)
AIC(glmm_pseudo_4th)
AIC(glmm_actual)

confint(glmm_pseudo_2nd, devmatchtol = 1e-04)
ci.ps3 <- confint(glmm_pseudo_3rd, devmatchtol = 1e-04)
confint(glmm_pseudo_4th, devmatchtol = 1e-04)
ci.actual <- confint(glmm_actual, devmatchtol = 1e-04)
