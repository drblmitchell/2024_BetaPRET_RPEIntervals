library(ggplot2)
library(patchwork)

# Load intervals data from Analyses.Rmd

intervals$condition = factor(intervals$condition, c("bblockade", "control"))

#### Function definitions ###

# Calculate predicted values and SE
calculate_predictions <- function(model) {
  # Grid of condition x intensity x bout
  new_data <- expand.grid(
    condition = c(0, 1),
    intensity = c(13, 15),
    bout_rpe = c(1, 2, 3)
  ) %>% 
    mutate(
      condition = factor(condition, c(1, 0), c("bblockade", "control")),
      intensity = factor(intensity, c(13, 15), c("rpe13", "rpe15"))
    )
  
  # Generate predictions with SE
  predictions <- predict(model, newdata = new_data, re.form = NA, se.fit = TRUE)

  return(
    new_data %>% 
      mutate(
        predicted = predictions$fit,
        se = predictions$se.fit
      )
  )  
}

#### Custom settings ####

point_size = 2
stroke_size = 0.8
grey_col = "grey30"

custom_theme <- theme_classic() + theme(
  legend.position = "none",
  axis.title = element_text(size = 9),
  axis.text = element_text(size = 9, colour = "black"),
  axis.ticks = element_line(colour = "black")
)

remove_x_axis <- theme(
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.title.x = element_blank()
)

set_scales <- function() {
  list(
    scale_colour_manual(values = c("rpe13" = grey_col, "rpe15" = "black")),
    scale_fill_manual(values = c("control.rpe13" = "white",
                                 "control.rpe15" = "white",
                                 "bblockade.rpe13" = grey_col,
                                 "bblockade.rpe15" = "black"))
  )
}

### Plots ###

# Heart rate plot
hr_1 <- calculate_predictions(lmm_hr_reduced) %>% 
  ggplot(aes(x = bout_rpe,
             y = predicted,
             colour = intensity,
             linetype = condition,
             fill = interaction(condition, intensity))) +
  stat_summary(aes(x = bout_rpe, y = hr),
               data = intervals,
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.05,
               linewidth = 0.5,
               linetype = "solid") +
  geom_line() +
  stat_summary(aes(x = bout_rpe, y = hr),
               data = intervals,
               fun = mean,
               geom = "point",
               size = point_size,
               shape = 21,
               stroke = stroke_size) +
  coord_cartesian(ylim = c(60, 180)) +
  scale_y_continuous(breaks = seq(60, 180, by = 40), expand = c(0, 0)) +
  scale_x_continuous(breaks = 1:3) +
  ylab(bquote("Heart rate (beat·min"^{-1} * ")")) +
  set_scales() + custom_theme


# %HRpeak plot
hr_2 <- calculate_predictions(lmm_pcthrpeak_reduced) %>% 
  ggplot(aes(x = bout_rpe,
             y = predicted,
             colour = intensity,
             linetype = condition,
             fill = interaction(condition, intensity))) +
  stat_summary(aes(x = bout_rpe, y = pct_hr_peak),
               data = intervals,
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.05,
               linewidth = 0.5,
               linetype = "solid") +
  geom_line() +
  stat_summary(aes(x = bout_rpe, y = pct_hr_peak),
               data = intervals,
               fun = mean,
               geom = "point",
               size = point_size,
               shape = 21,
               stroke = stroke_size) +
  coord_cartesian(ylim = c(40, 100)) +
  scale_y_continuous(breaks = seq(40, 100, by = 20), expand = c(0, 0)) +
  scale_x_continuous(breaks = 1:3) +
  xlab("Interval") +
  ylab(bquote("%HR"[{peak}])) +
  set_scales() + custom_theme


# Oxygen uptake plot
vo2_1 <- calculate_predictions(lmm_vo2kg_reduced) %>% 
  ggplot(aes(x = bout_rpe,
             y = predicted,
             colour = intensity,
             linetype = condition,
             fill = interaction(condition, intensity))) +
  stat_summary(aes(x = bout_rpe, y = vo2kg),
               data = intervals,
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.05,
               linewidth = 0.5,
               linetype = "solid") +
  geom_line() +
  stat_summary(aes(x = bout_rpe, y = vo2kg),
               data = intervals,
               fun = mean,
               geom = "point",
               size = point_size,
               shape = 21,
               stroke = stroke_size) +
  coord_cartesian(ylim = c(20, 40)) +
  scale_y_continuous(breaks = seq(20, 40, by = 5), expand = c(0, 0)) +
  scale_x_continuous(breaks = 1:3) +
  ylab(bquote("VO"[2] * " (mL·kg"^{-1} * "·min"^{-1} * ")")) +
  set_scales() + custom_theme


# %VO2peak plot
vo2_2 <- calculate_predictions(lmm_pctvo2kgpeak_reduced) %>% 
  ggplot(aes(x = bout_rpe,
             y = predicted,
             colour = intensity,
             linetype = condition,
             fill = interaction(condition, intensity))) +
  stat_summary(aes(x = bout_rpe, y = pct_vo2kg_peak),
               data = intervals,
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.05,
               linewidth = 0.5,
               linetype = "solid") +
  geom_line() +
  stat_summary(aes(x = bout_rpe, y = pct_vo2kg_peak),
               data = intervals,
               fun = mean,
               geom = "point",
               size = point_size,
               shape = 21,
               stroke = stroke_size) +
  coord_cartesian(ylim = c(40, 100)) +
  scale_y_continuous(breaks = seq(40, 100, by = 20), expand = c(0, 0)) +
  scale_x_continuous(breaks = 1:3) +
  xlab("Interval") +
  ylab(bquote("%VO"[2][p][e][a][k])) +
  set_scales() + custom_theme


# Work rate (METs) plot
wr_1 <- calculate_predictions(lmm_mets_reduced) %>% 
  ggplot(aes(x = bout_rpe,
             y = predicted,
             colour = intensity,
             linetype = condition,
             fill = interaction(condition, intensity))) +
  stat_summary(aes(x = bout_rpe, y = mets),
               data = intervals,
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.05,
               linewidth = 0.5,
               linetype = "solid") +
  geom_line() +
  stat_summary(aes(x = bout_rpe, y = mets),
               data = intervals,
               fun = mean,
               geom = "point",
               size = point_size,
               shape = 21,
               stroke = stroke_size) +
  coord_cartesian(ylim = c(7, 13)) +
  scale_y_continuous(breaks = seq(7, 13, by = 2), expand = c(0, 0)) +
  scale_x_continuous(breaks = 1:3) +
  xlab("Interval") +
  ylab("Work rate (METs)") +
  set_scales() + custom_theme


# %WRpeak plot
wr_2 <- calculate_predictions(lmm_pctmetspeak_reduced) %>% 
  ggplot(aes(x = bout_rpe,
             y = predicted,
             colour = intensity,
             linetype = condition,
             fill = interaction(condition, intensity))) +
  stat_summary(aes(x = bout_rpe, y = pct_mets_peak),
               data = intervals,
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.05,
               linewidth = 0.5,
               linetype = "solid") +
  geom_line() +
  stat_summary(aes(x = bout_rpe, y = pct_mets_peak),
               data = intervals,
               fun = mean,
               geom = "point",
               size = point_size,
               shape = 21,
               stroke = stroke_size) +
  coord_cartesian(ylim = c(40, 100)) +
  scale_y_continuous(breaks = seq(40, 100, by = 20), expand = c(0, 0)) +
  scale_x_continuous(breaks = 1:3) +
  xlab("Interval") +
  ylab(bquote("%WR"[p][e][a][k])) +
  set_scales() + custom_theme +
  theme(plot.margin = margin(t = 5))


### Combined plots ###
plt <- (hr_1 + remove_x_axis + theme(plot.margin = margin(b = 10, r = 5))) + 
  (hr_2 + remove_x_axis + theme(plot.margin = margin(b = 10))) + 
  (vo2_1 + remove_x_axis + theme(plot.margin = margin(b = 10, r = 5))) + 
  (vo2_2 + remove_x_axis + theme(plot.margin = margin(b = 10))) + 
  (wr_1 + theme(plot.margin = margin())) + 
  (wr_2 + theme(plot.margin = margin())) +
  plot_layout(ncol = 2) + 
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(plot.margin = margin())
  ) &
  theme(plot.tag = element_text(size = 9))

# Uncomment to save
# ggsave("./R/out/fig_mixed_models.pdf", plt, width = 172, units = "mm")
