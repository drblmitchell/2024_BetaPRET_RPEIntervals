library(ggplot2)
library(patchwork)

plt_theme <- theme(
  axis.ticks = element_blank(),
  axis.text = element_text(size = 8, colour = "black"),
  axis.title = element_text(size = 8, colour = "black")
)

# HEART RATE
hr_residuals <- residuals(lmm_hr_reduced)
hr_fitted <- fitted(lmm_hr_reduced)

## Residuals plot
plt_hr1 <- data.frame(
  fitted = hr_fitted,
  residuals = hr_residuals
) %>% 
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(80, 180),
    breaks = seq(80, 180, by = 20),
    name = "Fitted"
  ) +
  scale_y_continuous(
    name = "Residuals"
  ) +
  theme(plot.margin = margin(r = 5))

## Q-Q normality plot
plt_hr2 <- qqnorm(hr_residuals, plot.it = FALSE) %>% 
  data.frame(
    theoretical = .$x,
    sample = .$y
  ) %>% 
  ggplot(aes(x = theoretical, y = sample)) +
  geom_point(size = 1) +
  geom_abline(
    slope = sd(hr_residuals),
    intercept = mean(hr_residuals),
    linetype = "dashed"
  ) +
  labs(
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme(plot.margin = margin(l = 5))

## Combined plot
plt_hr <- (plt_hr1 + plt_theme) +
(plt_hr2 + plt_theme) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.margin = margin())
  ) & theme(plot.tag = element_text(size = 8))

# Uncomment to save
# ggsave("./R/out/fig_assumptions_hr.pdf", plot = plt_hr, width = 159, height = 75, units = "mm")


# %HRpeak
hrpeak_residuals <- residuals(lmm_pcthrpeak_reduced)
hrpeak_fitted <- fitted(lmm_pcthrpeak_reduced)

## Residuals plot
plt_hrpeak1 <- data.frame(
  fitted = hrpeak_fitted,
  residuals = hrpeak_residuals
) %>% 
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(50, 102),
    name = "Fitted"
  ) +
  scale_y_continuous(
    name = "Residuals"
  ) +
  theme(plot.margin = margin(r = 5))

## Q-Q normality plot
plt_hrpeak2 <- qqnorm(hrpeak_residuals, plot.it = FALSE) %>% 
  data.frame(
    theoretical = .$x,
    sample = .$y
  ) %>% 
  ggplot(aes(x = theoretical, y = sample)) +
  geom_point(size = 1) +
  geom_abline(
    slope = sd(hrpeak_residuals),
    intercept = mean(hrpeak_residuals),
    linetype = "dashed"
  ) +
  labs(
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme(plot.margin = margin(l = 5))

## Combined plot
plt_hrpeak <- (plt_hrpeak1 + plt_theme) +
  (plt_hrpeak2 + plt_theme) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.margin = margin())
  ) & theme(plot.tag = element_text(size = 8))

# Uncomment to save
# ggsave("./R/out/fig_assumptions_hrpeak.pdf", plot = plt_hrpeak, width = 159, height = 75, units = "mm")


# OXYGEN UPTAKE
vo2_residuals <- residuals(lmm_vo2kg_reduced)
vo2_fitted <- fitted(lmm_vo2kg_reduced)

## Residuals plot
plt_vo21 <- data.frame(
  fitted = vo2_fitted,
  residuals = vo2_residuals
) %>% 
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(15,45),
    # breaks = seq(80, 180, by = 20),
    name = "Fitted"
  ) +
  scale_y_continuous(
    name = "Residuals"
  ) +
  theme(plot.margin = margin(r = 5))

## Q-Q normality plot
plt_vo22 <- qqnorm(vo2_residuals, plot.it = FALSE) %>% 
  data.frame(
    theoretical = .$x,
    sample = .$y
  ) %>% 
  ggplot(aes(x = theoretical, y = sample)) +
  geom_point(size = 1) +
  geom_abline(
    slope = sd(vo2_residuals),
    intercept = mean(vo2_residuals),
    linetype = "dashed"
  ) +
  labs(
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme(plot.margin = margin(l = 5))

## Combined plot
plt_vo2 <- (plt_vo21 + plt_theme) +
  (plt_vo22 + plt_theme) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.margin = margin())
  ) & theme(plot.tag = element_text(size = 8))

# Uncomment to save
# ggsave("./R/out/fig_assumptions_vo2.pdf", plot = plt_vo2, width = 159, height = 75, units = "mm")


# %VO2peak
vo2peak_residuals <- residuals(lmm_pctvo2kgpeak_reduced)
vo2peak_fitted <- fitted(lmm_pctvo2kgpeak_reduced)

## Residuals plot
plt_vo2peak1 <- data.frame(
  fitted = vo2peak_fitted,
  residuals = vo2peak_residuals
) %>% 
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    name = "Fitted"
  ) +
  scale_y_continuous(
    name = "Residuals"
  ) +
  theme(plot.margin = margin(r = 5))

## Q-Q normality plot
plt_vo2peak2 <- qqnorm(vo2peak_residuals, plot.it = FALSE) %>% 
  data.frame(
    theoretical = .$x,
    sample = .$y
  ) %>% 
  ggplot(aes(x = theoretical, y = sample)) +
  geom_point(size = 1) +
  geom_abline(
    slope = sd(vo2peak_residuals),
    intercept = mean(vo2peak_residuals),
    linetype = "dashed"
  ) +
  labs(
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme(plot.margin = margin(l = 5))

## Combined plot
plt_vo2peak <- (plt_vo2peak1 + plt_theme) +
  (plt_vo2peak2 + plt_theme) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.margin = margin())
  ) & theme(plot.tag = element_text(size = 8))

# Uncomment to save
# ggsave("./R/out/fig_assumptions_vo2peak.pdf", plot = plt_vo2peak, width = 159, height = 75, units = "mm")


## Work rate
mets_residuals <- residuals(lmm_mets_reduced)
mets_fitted <- fitted(lmm_mets_reduced)

## Residuals plot
plt_mets1 <- data.frame(
  fitted = mets_fitted,
  residuals = mets_residuals
) %>% 
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(6, 14),
    name = "Fitted"
  ) +
  scale_y_continuous(
    name = "Residuals"
  ) +
  theme(plot.margin = margin(r = 5))

## Q-Q normality plot
plt_mets2 <- qqnorm(mets_residuals, plot.it = FALSE) %>% 
  data.frame(
    theoretical = .$x,
    sample = .$y
  ) %>% 
  ggplot(aes(x = theoretical, y = sample)) +
  geom_point(size = 1) +
  geom_abline(
    slope = sd(mets_residuals),
    intercept = mean(mets_residuals),
    linetype = "dashed"
  ) +
  labs(
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme(plot.margin = margin(l = 5))

## Combined plot
plt_mets <- (plt_mets1 + plt_theme) +
  (plt_mets2 + plt_theme) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.margin = margin())
  ) & theme(plot.tag = element_text(size = 8))

# Uncomment to save
# ggsave("./R/out/fig_assumptions_mets.pdf", plot = plt_mets, width = 159, height = 75, units = "mm")


# %WRpeak
metspeak_residuals <- residuals(lmm_pctmetspeak_reduced)
metspeak_fitted <- fitted(lmm_pctmetspeak_reduced)

## Residuals plot
plt_metspeak1 <- data.frame(
  fitted = metspeak_fitted,
  residuals = metspeak_residuals
) %>% 
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(40, 75),
    name = "Fitted"
  ) +
  scale_y_continuous(
    name = "Residuals"
  ) +
  theme(plot.margin = margin(r = 5))

## Q-Q normality plot
plt_metspeak2 <- qqnorm(metspeak_residuals, plot.it = FALSE) %>% 
  data.frame(
    theoretical = .$x,
    sample = .$y
  ) %>% 
  ggplot(aes(x = theoretical, y = sample)) +
  geom_point(size = 1) +
  geom_abline(
    slope = sd(metspeak_residuals),
    intercept = mean(metspeak_residuals),
    linetype = "dashed"
  ) +
  labs(
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme(plot.margin = margin(l = 5))

## Combined plot
plt_metspeak <- (plt_metspeak1 + plt_theme) +
  (plt_metspeak2 + plt_theme) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.margin = margin())
  ) & theme(plot.tag = element_text(size = 8))

# Uncomment to save
# ggsave("./R/out/fig_assumptions_metspeak.pdf", plot = plt_metspeak, width = 159, height = 75, units = "mm")
