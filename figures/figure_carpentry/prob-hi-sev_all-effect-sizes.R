# Visualize all parameter values 
library(ggplot2)
library(brms)
library(tidyr)

if (!file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm.rds"))) {
  source(here::here("analyses/probability-of-high-severity_build-models.R"))
}

load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm.rds"))

if(!file.exists(here::here("data/data_output/all-fire-samples_texture_configured.rds"))) {
  source(here::here("data/data_carpentry/configure_fire-samples.R"))
}

# This .rds file has the R object name `ss_burned`
load(here::here("data/data_output/burned-fire-samples_texture_configured.rds"))

# What is the mean fm100_s for non-extreme and extreme conditions?
fm100_s_means <- 
  ss_burned %>% 
  as.data.frame() %>% 
  group_by(extreme80_fm100) %>% 
  summarize(mean_fm100 = mean(fm100_s))


get_beta_coef <- function(m, fm100_s_means) {
  # Output will be a distribution for Intercept, heterogeneity in normal conditions, heterogeneity in extreme conditions, fm100 in normal conditions, fm100 in extreme conditions, preFire_ndvi, topo_roughness, and pahl
  samps <- posterior_samples(m)
  prior_samps <- prior_samples(m)
  
  betas <- samps[, 1:11]
  betas_prior <- samps[, 1:11]
  
  het_normal <- c(0, 1, 0, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 0), mean_fm100)), 0, 0, 0, 0, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 0), mean_fm100)), 0, 0)
  
  het_xtreme <- c(0, 1, 1, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                  , 0, 0, 0, 1, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                  , pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                  , pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100)))
  
  fm100_xtreme <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0)
  
  # coefficients
  
  b_intercept <- betas$b_Intercept
  b_het_normal <- apply(betas, 1, function(x) x %*% het_normal)
  b_het_xtreme <- apply(betas, 1, function(x) x %*% het_xtreme)
  b_fm100_normal <- betas$b_fm100_s
  b_fm100_xtreme <- apply(betas, 1, function(x) x %*% fm100_xtreme)
  b_preFire_ndvi <- betas$b_preFire_ndvi_s
  b_topo_roughness <- dplyr::select(betas, starts_with("b_topo_roughness")) %>% pull()
  b_pahl <- betas$b_pahl_s
  
  b_intercept_prior <- betas_prior$b_Intercept
  b_het_normal_prior <- apply(betas_prior, 1, function(x) x %*% het_normal)
  b_het_xtreme_prior <- apply(betas_prior, 1, function(x) x %*% het_xtreme)
  b_fm100_normal_prior <- betas_prior$b_fm100_s
  b_fm100_xtreme_prior <- apply(betas_prior, 1, function(x) x %*% fm100_xtreme)
  b_preFire_ndvi_prior <- betas_prior$b_preFire_ndvi_s
  b_topo_roughness_prior <- dplyr::select(betas_prior, starts_with("b_topo_roughness")) %>% pull()
  b_pahl_prior <- betas_prior$b_pahl_s
  
  data.frame(b_intercept, b_het_normal, b_het_xtreme, b_fm100_normal, b_fm100_xtreme, b_preFire_ndvi, b_topo_roughness, b_pahl, b_intercept_prior, b_het_normal_prior, b_het_xtreme_prior, b_fm100_normal_prior, b_fm100_xtreme_prior, b_preFire_ndvi_prior, b_topo_roughness_prior, b_pahl_prior)
}

betas_1 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm, fm100_s_means)
betas_2 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm, fm100_s_means)
betas_3 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm, fm100_s_means)
betas_4 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm, fm100_s_means)

# This function creates "halfeye" plots, which make a dot and interval plot for each beta coefficient estimate of interest (with the line representing a
# 95% credible interval based on simple symmetric quantiles) and then the density plot of the distribution on top of the interval. This allows a
# "quick view" of the important interval, but still displays all the data. 
# The 6 different parameters are actually their own plots, with the mfrow= argument in par() set to stack them all on top of each other (and the mar=
# argument set to leave no gaps between these 6 plots on the top or bottom)

# I "explode" the first plot to annotate it, but the next few parameters are plotted similarly
# I then "explode" the plots that have an interaction with "extreme fuel moisture condition or not" to annotate those, since they are a little different
# The main difference is that I plot the beta estimate for each condition on the same line in different colors

plot_halfeye <- function(betas_radius, pt_cex = 1, suppress_legend = TRUE, suppress_lab = TRUE, suppress_title = TRUE, space_for_legend = FALSE, point_line_repel = -0.1) {
  
  # The intercept
  plot(density(betas_radius$b_intercept), # standard density plot of the samples from the posterior distribution of the intercept
       xlim = range(betas_radius), # to keep all 6 parameter plots on the same x scale, I use the min and max of the posterior samples for *all* the parameters as the x axis limit
       ylim = c(max(density(betas_radius$b_intercept)$y) * point_line_repel, max(density(betas_radius$b_intercept)$y)), # y limits go to the max for this particular parameter's
       # maximum probability density, and as low as the maximum probability density times the `point_line_repel` value, which is negative and specifies where the point will go
       # (below the density plot) representing the mean. `point_line_repel` is a multiplier, so -0.1 means it will go 10% of the max probability density below the density curve
       xlab = NA, # all labeling is done outside of the plot calls using mtext()
       ylab = NA, 
       main = NA, 
       bty = "n", # No box around the plot
       xaxt = "n", # all axis generation is done outside the plot() call using axis()
       yaxt = "n")
  
  points(x = mean(betas_radius$b_intercept), # add a point representing the mean of the posterior distribution
         y = max(density(betas_radius$b_intercept)$y) * point_line_repel, # the point goes below the density plot using the `point_line_repel` multiplier
         cex = pt_cex, # the point size. Usually have to keep it small to see it!
         pch = 19) # a closed circle
  
  lines(x = quantile(betas_radius$b_intercept, probs = c(0.025, 0.975)), # the lines along the axis representing the 95% (symmetrical) CI
        y = rep(max(density(betas_radius$b_intercept)$y) * point_line_repel, times = 2), # the line stays below the density plot at the y position specified by the 
        # `point_line_repel` multiplier
        lwd = 2) # make the line thicker
  
  abline(v = 0, lty = 2) # add a vertical dashed line through 0
  
  # Prefire NDVI
  plot(density(betas_radius$b_preFire_ndvi), 
       xlim = range(betas_radius), 
       ylim = c(max(density(betas_radius$b_preFire_ndvi)$y) * point_line_repel, max(density(betas_radius$b_preFire_ndvi)$y)), 
       xlab = NA, 
       ylab = NA, 
       main = NA, 
       bty = "n", 
       xaxt = "n", 
       yaxt = "n")  
  
  points(x = mean(betas_radius$b_preFire_ndvi), 
         y = max(density(betas_radius$b_preFire_ndvi)$y) * point_line_repel, 
         cex = pt_cex, 
         pch = 19)
  
  lines(x = quantile(betas_radius$b_preFire_ndvi, probs = c(0.025, 0.975)), 
        y = rep(max(density(betas_radius$b_preFire_ndvi)$y) * point_line_repel, times = 2), 
        lwd = 2)
  
  abline(v = 0, lty = 2)
  
  # Potential annual heat load
  plot(density(betas_radius$b_pahl), 
       xlim = range(betas_radius), 
       ylim = c(max(density(betas_radius$b_pahl)$y) * point_line_repel, max(density(betas_radius$b_pahl)$y)), 
       xlab = NA, 
       ylab = NA, 
       main = NA, 
       bty = "n", 
       xaxt = "n", 
       yaxt = "n")  
  
  points(x = mean(betas_radius$b_pahl), 
         y = max(density(betas_radius$b_pahl)$y) * point_line_repel, 
         cex = pt_cex, pch = 19)
  
  lines(x = quantile(betas_radius$b_pahl, probs = c(0.025, 0.975)), 
        y = rep(max(density(betas_radius$b_pahl)$y) * point_line_repel, times = 2), 
        lwd = 2)
  
  abline(v = 0, lty = 2)
  
  # Topographic roughness
  plot(density(betas_radius$b_topo_roughness), 
       xlim = range(betas_radius), 
       ylim = c(max(density(betas_radius$b_topo_roughness)$y) * point_line_repel, max(density(betas_radius$b_topo_roughness)$y)), 
       xlab = NA, 
       ylab = NA, 
       main = NA, 
       bty = "n", 
       xaxt = "n", 
       yaxt = "n")  
  
  points(x = mean(betas_radius$b_topo_roughness), 
         y = max(density(betas_radius$b_topo_roughness)$y) * point_line_repel, 
         cex = pt_cex, 
         pch = 19)
  
  lines(x = quantile(betas_radius$b_topo_roughness, probs = c(0.025, 0.975)), 
        y = rep(max(density(betas_radius$b_topo_roughness)$y) * point_line_repel, times = 2), 
        lwd = 2)
  
  abline(v = 0, lty = 2)
  
  # Effect of fm100_s in both extreme and normal fuel moisture conditions (plotted on the same line)
  plot(density(betas_radius$b_fm100_normal), 
       xlim = range(betas_radius), 
       ylim = c(max(density(betas_radius$b_fm100_normal)$y) * point_line_repel, max(density(betas_radius$b_fm100_normal)$y)), 
       xlab = NA, 
       ylab = NA, 
       main = NA, 
       bty = "n", 
       xaxt = "n", 
       yaxt = "n", 
       col = "blue")
  
  points(x = mean(betas_radius$b_fm100_normal), 
         y = max(density(betas_radius$b_fm100_normal)$y) * point_line_repel, 
         cex = pt_cex, 
         pch = 19, 
         col = "blue")
  
  lines(x = quantile(betas_radius$b_fm100_normal, probs = c(0.025, 0.975)), 
        y = rep(max(density(betas_radius$b_fm100_normal)$y) * point_line_repel, times = 2), 
        lwd = 2, 
        col = "blue")
  
  lines(density(betas_radius$b_fm100_xtreme), col = "red")
  
  points(x = mean(betas_radius$b_fm100_xtreme), 
         y = max(density(betas_radius$b_fm100_xtreme)$y) * point_line_repel, 
         cex = pt_cex, 
         pch = 19, 
         col = "red")
  
  lines(x = quantile(betas_radius$b_fm100_xtreme, probs = c(0.025, 0.975)), 
        y = rep(max(density(betas_radius$b_fm100_xtreme)$y) * point_line_repel, times = 2), 
        lwd = 2, 
        col = "red")
  
  abline(v = 0, lty = 2)
  
  old_mar <- par()$mar
  old_mgp <- par()$mgp
  
  if (!space_for_legend) {
    par(mar = c(3, 1, 0, 1), mgp = old_mgp + 3)
  }
  
  # Heterogeneity
  plot(density(betas_radius$b_het_normal), 
       xlim = range(betas_radius), 
       ylim = c(max(density(betas_radius$b_het_normal)$y) * point_line_repel, max(density(betas_radius$b_het_normal)$y)), 
       xlab = NA, 
       ylab = NA, 
       main = NA, 
       bty = "n", 
       yaxt = "n", 
       col = "blue", 
       xaxt = "n")
  
  points(x = mean(betas_radius$b_het_normal), 
         y = max(density(betas_radius$b_het_normal)$y) * point_line_repel, 
         cex = pt_cex, 
         pch = 19, 
         col = "blue")
  
  lines(x = quantile(betas_radius$b_het_normal, probs = c(0.025, 0.975)), 
        y = rep(max(density(betas_radius$b_het_normal)$y) * point_line_repel, times = 2), 
        lwd = 2, 
        col = "blue")
  
  lines(density(betas_radius$b_het_xtreme), col = "red")
  
  points(x = mean(betas_radius$b_het_xtreme), 
         y = max(density(betas_radius$b_het_xtreme)$y) * point_line_repel, 
         cex = pt_cex, 
         pch = 19, 
         col = "red")
  
  lines(x = quantile(betas_radius$b_het_xtreme, probs = c(0.025, 0.975)), 
        y = rep(max(density(betas_radius$b_het_xtreme)$y) * point_line_repel, times = 2), 
        lwd = 2, 
        col = "red")
  
  abline(v = 0, lty = 2)

  axis(side = 1, line = 2.5, at = -3:1, cex.axis = 0.8)
  
  if (!suppress_legend) {  
    legend("bottomleft", 
           legend = c("Normal fuel moisture", "Extreme fuel moisture"), 
           col = c("blue", "red"), 
           lty = 1, 
           lwd = 2, 
           bty = "n", 
           horiz = TRUE, 
           xpd = NA, 
           xjust = 0.5, 
           inset = c(-0.2, -0.5))
  }
  
  par(mar = old_mar, mgp = old_mgp)
  
  if (!suppress_lab) {
    mtext(side = 1, "Effect size (log-odds scale)", outer = TRUE, line = 5.5)
    
    mtext(side = 2, text = "Intercept", outer = TRUE, at = 1 - 1/12, las = 1)
    mtext(side = 2, text = "Prefire NDVI", outer = TRUE, at = 5/6 - 1/12, las = 1)
    mtext(side = 2, text = "Potential annual\nheat load", outer = TRUE, at = 4/6 - 1/12, las = 1)
    mtext(side = 2, text = "Topographic\nroughness", outer = TRUE, at = 3/6 - 1/12, las = 1)
    mtext(side = 2, text = "100 hour\nfuel moisture", outer = TRUE, at = 2/6 - 1/12, las = 1)
    mtext(side = 2, text = "Heterogeneity", outer = TRUE, at = 1/6 - 1/12, las = 1)
  }
  
  if (!suppress_title) {
    mtext(side = 3, text = "90m x 90m\nneighborhood", outer = TRUE, at = 1/4 - 1/8, line = 1)
    mtext(side = 3, text = "150m x 150m\nneighborhood", outer = TRUE, at = 2/4 - 1/8, line = 1)
    mtext(side = 3, text = "210m x 210m\nneighborhood", outer = TRUE, at = 3/4 - 1/8, line = 1)
    mtext(side = 3, text = "270m x 270m\nneighborhood", outer = TRUE, at = 4/4 - 1/8, line = 1)
  }
}


pdf("figures/prob-hi-sev_all-effect-sizes.pdf", width = 17.3 / 2.54, height = 17.3 / 2.54 * (3/4))
plot_layout <- matrix(c(1:24), nrow = 6, ncol = 4)
layout(plot_layout)
par(mar = c(0, 0.1, 0, 0.1), xpd = FALSE, oma = c(8, 10, 5, 0))

# par(mfrow = c(6, 1), xpd = TRUE, mar = c(0, 6, 0, 1), oma = c(4.5, 1, 0, 0))
# par(mfrow = c(24, 1), xpd = TRUE, mar = c(0, 6, 0, 1), oma = c(4.5, 1, 0, 0))

plot_halfeye(betas_1, space_for_legend = TRUE, pt_cex = 0.5)
plot_halfeye(betas_2, space_for_legend = TRUE, suppress_legend = FALSE, pt_cex = 0.5)
plot_halfeye(betas_3, space_for_legend = TRUE, pt_cex = 0.5)
plot_halfeye(betas_4, , space_for_legend = TRUE, suppress_lab = FALSE, suppress_title = FALSE, pt_cex = 0.5)

dev.off()