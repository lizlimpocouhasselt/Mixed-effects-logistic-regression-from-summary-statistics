# Mixed effects logistic regression from summary statistics

## Description
This README demonstrates (via R) the concepts explained in our paper entitled [Federated mixed effects logistic regression based on one-time shared summary statistics](http://arxiv.org/abs/2411.04002), although these can be implemented in any other statistical softwares.

We present how a data analyst, upon receiving summary data from a data provider (e.g. clinic), can estimate a mixed effects binary logistic regression model without having access to individual observations for privacy reasons. For possible data preparation steps on the side of the data provider, code is also available in glmm_chop.R.

## Strategy
I. Generate pseudo-data

II. Estimate the model using pseudo-data

## Demo code
To generate pseudo-data, we utilize the following functions:
```r
source('lsqnonlin_2.R')
source('gen_pseudo.R')
source('fn_pseudodata_3rdmom.R')
``` 
We also need the following R packages: 
```r
library(medicaldata)
library(skimr)
library(dplyr)
library(lme4)
library(stringr)
library(pracma)
library(MASS)
library(reshape)
```
For this demo, we use the publicly available data from the Children's Hospital of Pennsylvania (CHOP) which can be accessed from the R package [medicaldata](https://cran.r-project.org/web/packages/medicaldata/medicaldata.pdf). It contains deidentified patient information and COVID-19 test results from 88 clinics, although we only utilized 57 clinics with a total of 6,330 patients for this demo after filtering out incomplete observations, observations with invalid values, and clinics with only 1 patient record. From these, we computed summary statistics (polynomial-approximate sufficient statistics) per clinic. To mimic a real-world scenario in this demo, we assume that the data analyst has received only the following summary statistics: 
```r
# Load summary data
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

``` 

Next, we specify the model formula we wish to use:
```r
# A. Specify model (optional)
formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind + std_pan_day + std_age
```

Then we generate pseudo-data matching up to the 3rd sample moments of the actual data for the specified variables. We consider up to the 3rd order only for this demo based on the results of the simulation study.
```r
# B. Specify the variable names
y_name <- all.vars(formula)[1]
names_ind_vars <- all.vars(formula)[-1]
numeric_var_names <- c('drive_thru_ind','std_pan_day','std_age')

# C. Generate pseudo-data ------
set.seed(121314)
pseudodata_3rdmom <- fn_pseudodata_3rdmom_glmm(y_name, names_ind_vars, numeric_var_names, summary_stats, var_cov_mat, mv_moment_3_4_bypair_df, mv_moment_3_4_by3_df, mv_moment_4_df)
```

With the pseudo-data, we can estimate a binary logistic regression model with random-intercept per clinic and compare the output to that produced from actual data:

```r
glmm_pseudo_3rd <- glmer(formula <- resultpositive ~ gendermale + patient_classemergency + patient_classoutpatient + drive_thru_ind + std_pan_day + std_age + (1|level2), data = do.call(rbind, pseudodata_3rdmom), family = binomial)
summary(glmm_pseudo_3rd)

# Compare with model based on actual [pre-processed] data
id_data <- "1QtTwfyKIvuPvg6uP1ZmZWp5vRIA8eHDS"
data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_data))
data$patient_class <- factor(data$patient_class, levels = c("inpatient", "emergency", "outpatient"))
glmm_actual <- glmer(ifelse(result == 'negative', 0, 1) ~ gender + patient_class + drive_thru_ind + scale(pan_day) + scale(age) + (1|clinic_name), data, family = binomial)
summary(glmm_actual)

# Compute AIC and confidence intervals
AIC(glmm_pseudo_3rd)
AIC(glmm_actual)
confint(glmm_pseudo_3rd, devmatchtol = 1e-04)
confint(glmm_actual, devmatchtol = 1e-04)

```




