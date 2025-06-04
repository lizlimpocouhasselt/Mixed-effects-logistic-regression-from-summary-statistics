#-------------------------------------------------------------------
# APPENDIX FIGURE 2: CONFIDENCE INTERVALS (n = 200)
#-------------------------------------------------------------------

# Load packages
library(ggplot2)
library(cowplot)

# Call function
source(file.path("Figures", "scripts_and_functions", "fn_figure_3_confint.R"))

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = rep(200, 3))

# Plot confidence intervals
nsim = 1000
lookup_df <- data.frame(num = 1:6,
                        name = I(list(expression(sigma[u]),
                                      expression(beta[0]),
                                      expression(beta[1]),
                                      expression(beta[2]),
                                      expression(beta[32]),
                                      expression(beta[33]))),
                        true = c(1.078, -4.20753, 0.33348,
                                 -0.25252, 1.20088, 0.53604))

ci_allpars <- lapply(1:6, function(parnum){
  ci_plots <- lapply(1:nrow(settings), function(setting){
    m <- settings$m[setting]
    uniform_cluster_size <- settings$uniform_cluster_size[setting]
    ci.df <- fn_confint(nsim, m, uniform_cluster_size, parnum)
    ci.df$iter <- rep(1:nsim, 4)
    parname <- lookup_df$name[match(parnum, lookup_df$num)]
    partrue <- lookup_df$true[match(parnum, lookup_df$num)]
    ggplot(ci.df, aes(x = iter, y = point)) + 
      ylab(parname[[1]]) +
      geom_line(aes(color = dat)) +
      ggtitle(paste0("m = ", m, "; n = ", uniform_cluster_size)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = dat, color = dat), alpha = 0.1) +
      geom_hline(yintercept = partrue, col = "red") 
  })
  lims <- unlist(lapply(ci_plots, function(plot){
    layer_scales(plot)$y$range$range
  }))
  legend <- get_legend(ci_plots[[1]] + theme(legend.position = "right"))
  ci_plots <- lapply(ci_plots, function(plot) plot + ylim(range(lims)) +
                       theme(legend.position = "none"))
  pg.par <- plot_grid(plotlist = ci_plots, ncol = nrow(settings))
  plot_grid(pg.par, legend, rel_widths = c(1, 0.075))
})
postscript(file.path("Appendix Figures", "outputs", "Afig2_ci_all_n_200.eps"))
plot_grid(plotlist = ci_allpars, ncol = 1)
dev.off()