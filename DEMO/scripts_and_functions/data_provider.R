#-----------------------------------------
# DATA PROVIDER TASK
# COMPUTE SUMMARY STATISTICS
#-----------------------------------------

# Load libraries / functions
library(medicaldata)
library(dplyr)
library(stringr)
library(matrixStats)
source(file.path('DEMO', 'scripts_and_functions', 'fn_compute_summary.R'))


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
  mutate(patient_class = factor(patient_class,
   levels = c("inpatient", "emergency", "outpatient"))) %>%
  mutate(drive_thru_ind = as.factor(drive_thru_ind)) %>%
  dplyr::select(result, gender, patient_class, drive_thru_ind, pan_day, age, clinic_name) %>%
  group_by(clinic_name) %>% filter(n() > 1) %>% as_tibble()
saveRDS(data, file = file.path("DEMO", "intermediate_results", "preprocessed_data.rds"))

# Identify variable type
numeric_var_names <- data %>% dplyr::select(where(is.numeric)) %>% names()


# II. Compute summary statistics
summary_info <- fn_compute_summary(data, 'clinic_name', numeric_var_names)
saveRDS(summary_info, file = file.path("DEMO", "intermediate_results", "summary_info.rds"))
