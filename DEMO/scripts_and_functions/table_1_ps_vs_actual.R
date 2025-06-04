#-----------------------------------------
# TABLE 1: Comparison of estimates when using
#    pseudo-data vs actual data
#-----------------------------------------

# Load libraries / functions
library(lme4)
library(reporter)
source(file.path('DEMO', 'scripts_and_functions', 'fn_table_1_ps_vs_actual.R'))

# Load pseudo-data (K = 3)
load(file.path("DEMO", "ps", "pseudodata_3rdmom.RData"))

# Retrieve point and interval estimates
glmm_pseudo_3rd <- readRDS(file.path("DEMO", "intermediate_results", "glmm_ps3.rds"))
ps3.ci <- readRDS(file.path("DEMO", "intermediate_results", "ci_ps3.rds"))

#---------------- COMPARISON WITH ACTUAL DATA ----------------
# Load actual data
data <- readRDS(file.path("DEMO", "intermediate_results", "preprocessed_data.rds"))

# Estimate a mixed effects logistic regression model from actual data
glmm_actual <- glmer(ifelse(result == 'negative', 0, 1) ~ gender + patient_class + drive_thru_ind +
 scale(pan_day) + scale(age) + (1|clinic_name), data, family = binomial)
actual.ci <- confint(glmm_actual, devmatchtol = 1e-04)
 
#---------------- TABLE 1 ----------------
ps3.point <- summary(glmm_pseudo_3rd)$coefficients
actual.point <- summary(glmm_actual)$coefficients
ps3.sigma <- attr(VarCorr(glmm_pseudo_3rd)$level2, "stddev")
actual.sigma <- attr(VarCorr(glmm_actual)$clinic_name, "stddev")
table_1a <- data.frame(Term = c('(Intercept)', 'Gendermale', 'Patient class (emergency)',
                               'Patient class (outpatient)', 'Drive thru ind',
                               'Std. Pandemic Day', 'Std. Age', symbol("sigma")),
                      `pseudo-data (3rd)` = c(mapply(fmt, ps3.point[, 1],
                                                   ps3.point[, 2],
                                                   sapply(ps3.point[, 4], get_stars)),
                                              sprintf("%.3f", ps3.sigma)),
                      `95% CI` = sprintf("(%.4f, %.4f)", ps3.ci[c(2:8, 1), 1],
                         ps3.ci[c(2:8, 1), 2]),
                      `actual data` = c(mapply(fmt, actual.point[, 1],
                                             actual.point[, 2],
                                             sapply(actual.point[, 4], get_stars)),
                                        sprintf("%.3f", actual.sigma)),
                      `95% CI actual` = sprintf("(%.4f, %.4f)", actual.ci[c(2:8, 1), 1],
                         actual.ci[c(2:8, 1), 2]),
                      check.names = FALSE, row.names = NULL)
# Add extra rows for the scaled residuals and other info
table_1b <- data.frame(
  Term = c("Scaled residuals:", "  Min", "  Q1", "  Median", "  Q3", "  Max",
           "AIC", "BIC", "N", "number of clinics", ""),
  `pseudo-data (3rd)` = c("", 
                          sapply(quantile(residuals(glmm_pseudo_3rd, "pearson", scaled = T)),
                                 fn_table_1b),
                          sapply(summary(glmm_pseudo_3rd)$AIC[1:2], fn_table_1b),
                          nrow(data), 
                          length(pseudodata_3rdmom), 
                          ""),
  `95% CI` = c("", "", "", "", "", "", "", "", "", "", ""),
  `actual data` = c("", 
                    sapply(quantile(residuals(glmm_actual, "pearson", scaled = T)),
                           fn_table_1b),
                    sapply(summary(glmm_pseudo_3rd)$AIC[1:2], fn_table_1b),
                    nrow(data), 
                    length(unique(data$clinic_name)), 
                    ""),
  `95% CI actual` = c("", "", "", "", "", "", "", "", "", "", ""),
  check.names = FALSE
)
full_table <- rbind(table_1a, table_1b)
# Define column widths (adjust based on your content)
column_widths <- c(30, 22, 22, 22, 22)
# Format the header
header1 <- sprintf(paste0("%-", column_widths[1], "s %-", column_widths[2] + column_widths[3] + 20,
 "s %-", column_widths[4] + column_widths[5] + 20, "s"), "", "pseudo-data (3rd)", "actual data")
header2_parts <- c(sprintf(paste0("%-", column_widths[1], "s"), ""),
                   sprintf(paste0("%-", column_widths[2], "s %-", column_widths[3], "s"), 
                    "Est(std. err.)", "95% CI"),
                   sprintf(paste0("%-", column_widths[4], "s %-", column_widths[5], "s"),
                    "Est(std. err.)", "95% CI"))
header2 <- paste(header2_parts, collapse = "  ")

separator <- paste(rep("-", nchar(header2)), collapse = "")

# Format the data rows
formatted_rows <- apply(full_table, 1, function(row) {
  sprintf(paste0("%-", column_widths, "s"), row)
})
formatted_table <- apply(formatted_rows, 2, paste, collapse = "  ")

# Add the significance notes at the end
significance_notes <- "\nSignificance levels: ***p<0.001; **p<0.01; *p<0.05"

# Write to the ASCII file
output_file <- file.path("DEMO", "tables", "table_1_ps_vs_actual.txt")
writeLines(c(header1, header2, separator, formatted_table, significance_notes),
           con = output_file)
