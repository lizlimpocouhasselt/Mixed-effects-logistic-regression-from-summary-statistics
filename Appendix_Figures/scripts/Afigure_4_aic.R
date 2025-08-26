#-----------------------------------------------------------------------
# APPENDIX FIGURE COMPARING AIC VALUES BETWEEN PSEUDO- AND ACTUAL DATA
#-----------------------------------------------------------------------

# Load libraries
library(ggplot2)
library(cowplot)
library(reshape2)

# Tabulate settings
settings <- data.frame(
  setting = 1:6,
  m = rep(c(30, 50, 100), 2),
  uniform_cluster_size = c(100, 60, 30, rep(200, 3))
)

# Plot AIC value comparison
nsim <- 1000
aic_plots <- lapply(1:nrow(settings), function(setting) {
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  aic.mat <- sapply(1:nsim, function(iter) {
    load(sprintf(file.path("SIMULATION", "intermediate_results", "orig_setting",
     "aic", "aic_vals_%04d_%04d_%04d.RData"), iter, m, uniform_cluster_size))
    aic_vals
  })
  aic.df <- as.data.frame(t(aic.mat))
  names(aic.df) <- c("sim", "ps2", "ps3", "ps4")
  aic.df <- aic.df[order(aic.df$sim), ]
  aic.df <- data.frame(
    sim = rep(aic.df$sim, 3),
    ps = c(aic.df$ps2, aic.df$ps3, aic.df$ps4),
    type = rep(c("ps2", "ps3", "ps4"), each = nsim)
  )
  ggplot(aic.df, aes(x = sim, y = ps, color = type)) +
    geom_point(shape = 1) +
    geom_abline(intercept = 0, color = "red") +
    ggtitle(paste0("m = ", m, ", n = ", uniform_cluster_size)) +
    theme(plot.title = element_text(hjust = 0.5))
})
postscript(file.path("Appendix Figures", "outputs", "Afig4_aic.eps"))
plot_grid(plotlist = aic_plots, ncol = 2, byrow = F)
dev.off()
