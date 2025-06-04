#-----------------------------------------
# DATA ANALYST TASK
# GENERATE PSEUDO-DATA AND ESTIMATE MODEL
#-----------------------------------------

library(pracma)
library(MASS)
library(lme4)
source(file.path('DEMO', 'scripts_and_functions', 'lsqnonlin_2.R'))
source(file.path('DEMO', 'scripts_and_functions', 'gen_pseudo.R'))
source(file.path('DEMO', 'scripts_and_functions', 'fn_pseudodata_2ndmom.R'))
source(file.path('DEMO', 'scripts_and_functions', 'fn_pseudodata_3rdmom.R'))
source(file.path('DEMO', 'scripts_and_functions', 'fn_pseudodata_4thmom.R'))

# I. Load summary data
summary_info <- readRDS(file.path("DEMO", "intermediate_results", "summary_info.rds"))
mean_cov <- summary_info[[1]]
var_cov_mat <- summary_info[[2]]
mv_moment_3_4_bypair_df <- summary_info[[3]]
mv_moment_3_4_by3_df <- summary_info[[4]]
mv_moment_4_df <- summary_info[[5]]

# II. Generate pseudo-data
## A. Specify model (optional)
formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient +
 drive_thru_ind1 + pan_day + age

## B. Specify the variable names
y_name <- all.vars(formula)[1]
names_ind_vars <- all.vars(formula)[-1]
numeric_var_names <- c('pan_day', 'age')

## C. Generate pseudo-data
# In the interest of time, the results of running these codes are saved in the 'ps' folder
set.seed(121314)
pseudodata_2ndmom <- fn_pseudodata_2ndmom_glmm(y_name, names_ind_vars, numeric_var_names, mean_cov, var_cov_mat)
pseudodata_3rdmom <- fn_pseudodata_3rdmom_glmm(y_name, names_ind_vars, numeric_var_names, mean_cov, var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df)
pseudodata_4thmom <- fn_pseudodata_4thmom_glmm(y_name, names_ind_vars, numeric_var_names, mean_cov, var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df, mv_moment_4_df)


# III. Estimate a mixed effects logistic regression model from pseudo-data
glmm_pseudo_2nd <- glmer(formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind1 + scale(pan_day) + scale(age) + (1|level2), data = do.call(rbind, pseudodata_2ndmom), family = binomial)

glmm_pseudo_3rd <- glmer(formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind1 + scale(pan_day) + scale(age) + (1|level2), data = do.call(rbind, pseudodata_3rdmom), family = binomial)

glmm_pseudo_4th <- glmer(formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind1 + scale(pan_day) + scale(age) + (1|level2), data = do.call(rbind, pseudodata_4thmom), family = binomial)

ps2.ci <- confint(glmm_pseudo_2nd, devmatchtol = 1e-04)
ps3.ci <- confint(glmm_pseudo_3rd, devmatchtol = 1e-04)
ps4.ci <- confint(glmm_pseudo_4th, devmatchtol = 1e-04)

saveRDS(glmm_pseudo_2nd, file = file.path("DEMO", "intermediate_results", "glmm_ps2.rds"))
saveRDS(glmm_pseudo_3rd, file = file.path("DEMO", "intermediate_results", "glmm_ps3.rds"))
saveRDS(glmm_pseudo_4th, file = file.path("DEMO", "intermediate_results", "glmm_ps4.rds"))
saveRDS(ps2.ci, file = file.path("DEMO", "intermediate_results", "ci_ps2.rds"))
saveRDS(ps3.ci, file = file.path("DEMO", "intermediate_results", "ci_ps3.rds"))
saveRDS(ps4.ci, file = file.path("DEMO", "intermediate_results", "ci_ps4.rds"))
