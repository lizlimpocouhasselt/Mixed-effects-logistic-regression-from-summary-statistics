#--------------------------------------------------
# FIGURE 8: DIFFERENCE IN UNIVARIATE MOMENTS
#           PSEUDO- VS ACTUAL DATA
#--------------------------------------------------

# Load libraries / functions
library(medicaldata)
library(dplyr)
library(stringr)
library(matrixStats)
library(reshape2)
library(ggplot2)
library(cowplot)
library(patchwork)
source(file.path("DEMO", "scripts_and_functions", "fn_compute_summary.R"))

# Load and pre-process actual data
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

# Load pseudodata
load(file.path("DEMO", "ps", "pseudodata_2ndmom.RData"))
load(file.path("DEMO", "ps", "pseudodata_3rdmom.RData"))
load(file.path("DEMO", "ps", "pseudodata_4thmom.RData"))

## Compare summary univariate statistics between actual and pseudo-data
### 1. Compute summary statistics
numeric_var_names <- c('pan_day', 'age')
summary_info_actual <- fn_compute_summary(data, 'clinic_name', numeric_var_names)
summary_info_ps2 <- fn_compute_summary(do.call(rbind, pseudodata_2ndmom),
 'level2', numeric_var_names)
summary_info_ps3 <- fn_compute_summary(do.call(rbind, pseudodata_3rdmom),
 'level2', numeric_var_names)
summary_info_ps4 <- fn_compute_summary(do.call(rbind, pseudodata_4thmom),
 'level2', numeric_var_names)
cols <- c('std_mean', 'std_var', 'std_target_3_moment', 'std_target_4_moment')


### 2. Compute absolute difference between pseudo- and actual data and plot in heatmaps
clinic <- data.frame(name = c("3 laboratory", "inpatient ward d", "inpatient ward e",
 "inpatient ward h", "inpatient ward a"),
                     number = c(2, 21, 22, 25, 18))
all_diffs <- lapply(clinic$number, function(num){
  lapply(c('ps2', 'ps3', 'ps4'), function(type){
    ps <- get(paste0("summary_info_", type))[[1]][[num]][, c('variable', cols)]
    actual <- summary_info_actual[[1]][[num]][, c('variable', cols)] %>%
      filter(variable %in% ps$variable)
    diff_vals <- abs(ps[, -1] - actual[, -1])
    as.vector(as.matrix(diff_vals))
  })
}) |> unlist()
global.min <- min(all_diffs, na.rm = TRUE)
global.max <- max(all_diffs, na.rm = TRUE)

all.plots <- lapply(clinic$number, function(num){
  clinic.name <- clinic$name[match(num, clinic$number)]
  numeric_var_names <- c('drive_thru_ind', 'pan_day', 'age')
  actual <- summary_info_actual[[1]][[num]][, c('variable', cols)]
  lapply(c('ps2', 'ps3', 'ps4'), function(type){
    ### 1. Compute summary statistics
    ps <- get(paste0("summary_info_", type))[[1]][[num]][, c('variable', cols)]
    actual <- actual %>% filter(variable %in% ps$variable)
    diff_sumstat_ps <- abs(ps[, -1] - actual[, -1])
    diff_sumstat_ps$variable <- ps$variable
    diff_sumstat_long_ps <- melt(diff_sumstat_ps)
    names(diff_sumstat_long_ps)[-1] <- c('sumstat','diff')
    diff_sumstat_long_ps$sumstat <- factor(diff_sumstat_long_ps$sumstat,
     levels = c('std_mean', 'std_var', 'std_target_3_moment', 'std_target_4_moment'))
    ggplot(diff_sumstat_long_ps, 
           aes(x = sumstat, y = variable, fill = diff)) + 
      geom_tile() + xlab("") + ylab("") +
      scale_fill_gradient(limits = c(global.min, global.max)) +
      geom_text(aes(label = round(diff, 3)), color = "white", size = 4) +
      scale_x_discrete(labels = c(expression(bar(x)),
                                  expression(s^2),
                                  expression(bar(mu)^3),
                                  expression(bar(mu)^4))) +
      ggtitle(type) +
      theme(plot.margin = margin(t = -5, r = 2.5, b = -10, l = 2.5))
  })
}) |> unlist(recursive = FALSE)
pg.all <- plot_grid(wrap_plots(all.plots, ncol = 3, guides = "collect") &
                      theme(legend.position = "right"))

labels <- c("n=2", "n=35", "n=62", "n=105", "n=208")
label_plot <- ggplot() +
  geom_text(aes(x = 1, y = rev(c(1.75,2.5,3.2,4,4.75)), label = labels),
            hjust = 0) +  # Reverse y to align with rows
  theme_void() +  # Remove axes, gridlines, etc.
  theme(plot.margin = margin(t = 5, r = 2.5, b = 60, l = 2.5))
postscript(file.path("Figures", "outputs", "fig8_sumstat_ps.eps"))
plot_grid(label_plot, pg.all, get_legend(pg.all), ncol = 3,
          rel_widths = c(0.075,1,0.075))
dev.off()
