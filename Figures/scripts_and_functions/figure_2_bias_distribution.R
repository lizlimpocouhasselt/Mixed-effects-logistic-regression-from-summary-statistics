#-------------------------------------------------------------------
# FIGURE 2: BIAS DISTRIBUTION OF ESTIMATES
#-------------------------------------------------------------------

# Load packages
library(reshape2)
library(ggplot2)
library(cowplot)

# Call function/s
source(file.path("Figures", "scripts_and_functions", "fn_figure_2_bias_distribution.R"))

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))

# Plot biases
nsim <- 1000
bias_plots <- lapply(1:nrow(settings), function(setting){
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  bias.df <- fn_bias(nsim, m, uniform_cluster_size)
  ggplot(bias.df, aes(x = factor(pars, levels = c("b0",
                                                  "b1",
                                                  "b2",
                                                  "b32",
                                                  "b33",
                                                  "sig.u")), 
                      y = bias, fill = dat)) + 
    geom_boxplot() +
    geom_hline(yintercept = 0, col = "red") +
    scale_x_discrete(labels = c("b0" = expression(beta[0]),
                                "b1" = expression(beta[1]),
                                "b2" = expression(beta[2]),
                                "b32" = expression(beta[32]),
                                "b33" = expression(beta[33]),
                                "sig.u" = expression(sigma[u]))) +
    xlab("") +
    ggtitle(paste0("m = ", m, "; n = ", uniform_cluster_size)) +
    theme(plot.title = element_text(hjust = 0.5))
})
postscript(file.path("Figures", "outputs", "fig2_bias.eps"), onefile = F)
plot_grid(plotlist = bias_plots, ncol = 1)
dev.off()
