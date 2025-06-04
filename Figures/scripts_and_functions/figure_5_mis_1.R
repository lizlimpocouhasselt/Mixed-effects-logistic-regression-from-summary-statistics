#-------------------------------------------------------------------
# FIGURE 5: BIAS DISTRIBUTION AND COVERAGE UNDER VARIABLE OMISSION
#-------------------------------------------------------------------

# Load packages
library(ggplot2)
library(cowplot)
library(reshape2)



# Call function/s
source(file.path("Figures", "scripts_and_functions", "fn_figure_5_mis_1.R"))

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))
settings$name <- factor(paste("m =", settings$m, 
                              ", n =", settings$uniform_cluster_size),
                        levels = paste("m =", settings$m, 
                                       ", n =", settings$uniform_cluster_size))
settings$name <- paste("m =", settings$m, 
                       ", n =", settings$uniform_cluster_size)

# Tabulate true parameter values
lookup_df <- data.frame(num = 1:5,
                        name = c("sig.u", "b0", "b1",
                                 "b32", "b33"),
                        symbol = I(list(expression(sigma[u]),
                                        expression(beta[0]),
                                        expression(beta[1]),
                                        expression(beta[32]),
                                        expression(beta[33]))),
                        true = c(1.078, -4.20753, 0.33348,
                                 1.20088, 0.53604))

# Plot biases
nsim <- 1000
bias_plots <- lapply(1:nrow(settings), function(setting){
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  bias.df <- fn_bias(nsim, m, uniform_cluster_size)
  ggplot(bias.df, aes(x = factor(pars, levels = c("b0",
                                                  "b1",
                                                  "b32",
                                                  "b33",
                                                  "sig.u")), 
                      y = bias, fill = dat)) + 
    geom_boxplot() +
    geom_hline(yintercept = 0, col = "red") +
    scale_x_discrete(labels = c("b0" = expression(beta[0]),
                                "b1" = expression(beta[1]),
                                "b32" = expression(beta[32]),
                                "b33" = expression(beta[33]),
                                "sig.u" = expression(sigma[u]))) +
    xlab("") +
    ggtitle(paste0("m = ", m, "; n = ", uniform_cluster_size)) +
    theme(plot.title = element_text(hjust = 0.5))
})
postscript(file.path("Figures", "outputs", "fig5a_bias_mis_1.eps"))
plot_grid(plotlist = bias_plots, ncol = 1)
dev.off()


# Compute coverage and plot them
nsim = 1000
coverage.all <- lapply(1:nrow(lookup_df), function(parnum){
  coverage.ls <- lapply(1:nrow(settings), function(setting){
    m <- settings$m[setting]
    uniform_cluster_size <- settings$uniform_cluster_size[setting]
    df <- data.frame(
      coverage = 100 *
        fn_coverage(nsim, m, uniform_cluster_size, parnum, lookup_df),
      setting = rep(paste("m =", m,", n =", uniform_cluster_size), 4))
    df$type = row.names(df); row.names(df) <- NULL
    df
  })
  coverage.df <- do.call(rbind, coverage.ls)
  parname <- lookup_df$name[match(parnum, lookup_df$num)]
  parsym <- lookup_df$symbol[match(parnum, lookup_df$num)]
  coverage.df$par <- rep(parname, nrow(coverage.df))
  ggplot(coverage.df, aes(x = factor(setting, 
                                     levels = settings$name), 
                          y = coverage, group = type)) +
    geom_line(aes(color = type)) + geom_point(aes(color = type)) + 
    ylab("coverage (%)") + xlab("") +
    geom_hline(yintercept = 95, color = "red") +
    geom_hline(yintercept = c(93.6,96.4), color = "red", lty = 2) +
    ggtitle(parsym[[1]]) + theme(plot.title = element_text(hjust=0.5))
})
legend <- get_legend(coverage.all[[1]] + theme(legend.position = "right"))
coverage.all <- lapply(coverage.all, function(plot) plot +
                         theme(legend.position = "none"))
pg.par <- plot_grid(plotlist = coverage.all, ncol = 2)
postscript(file.path("Figures", "outputs", "fig5b_coverage_mis_1.eps"))
plot_grid(pg.par, legend, rel_widths = c(1, 0.075))
dev.off()

