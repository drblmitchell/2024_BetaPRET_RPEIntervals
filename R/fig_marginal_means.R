library(ggplot2)
library(patchwork)

# Load intervals data from Analyses.Rmd

# Calculate summary means across bout_rpe
intervals_mean <- intervals |> 
  group_by(pid, condition, intensity) |> 
  summarise(
    mean_hr = mean(hr),
    mean_hrvt = mean(pct_hr_vt),
    mean_hrpeak = mean(pct_hr_peak),
    mean_vo2 = mean(vo2kg),
    mean_vo2vt = mean(pct_vo2kg_vt),
    mean_vo2peak = mean(pct_vo2kg_peak),
    mean_wr = mean(mets),
    mean_wrpeak = mean(pct_mets_peak),
    .groups = "drop"
  )

grey = "black"

# Define custom theme
custom_theme <- theme_classic() + theme(
  axis.title = element_text(size = 9),
  axis.title.x = element_blank(),
  axis.text = element_text(size = 9, colour = "black"),
  axis.ticks = element_line(colour = "black"),
  axis.ticks.x = element_blank(),
  ggh4x.axis.nestline.x = element_blank()
)

# Define theme to hide x-axis
remove_x_axis <- theme(
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.title.x = element_blank()
)

# Define function to apply scales
set_scales <- function() {
  list(
    guides(x = ggh4x::guide_axis_nested(delim = ".")),
    scale_x_discrete(labels = c("control.rpe13" = "Control.RPE 13",
                                "bblock.rpe13" = "B-blockade.RPE 13",
                                "control.rpe15" = "Control.RPE 15",
                                "bblock.rpe15" = "B-blockade.RPE 15"),
                     expand = expansion(add = 0.4))
  )
}

# Define function to nudge summary points
nudge_points = position_nudge(
  x = ifelse(
        grepl("control", levels(interaction(
          intervals_mean$condition,
          intervals_mean$intensity
        ))),
        -0.2, 0.2
      )
)



### Plots ###

# Heart rate plot
hr_1 <- intervals_mean |> 
  ggplot(aes(interaction(condition, intensity), mean_hr)) +
  geom_point(size = 1, colour = grey) +
  geom_line(aes(group = interaction(pid, intensity)), colour = grey) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    position = nudge_points
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1,
    position = nudge_points
  ) +
  coord_cartesian(ylim = c(50, 200)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab(bquote("Heart rate (beat·min"^{-1} * ")")) +
  set_scales() + custom_theme


# %HRpeak plot
hr_2 <- intervals_mean |> 
  ggplot(aes(interaction(condition, intensity), mean_hrpeak)) +
  geom_point(size = 1, colour = grey) +
  geom_line(aes(group = interaction(pid, intensity)), colour = grey) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    position = nudge_points
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1,
    position = nudge_points
  ) +
  coord_cartesian(ylim = c(20, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab(bquote("%HR"[p][e][a][k])) +
  set_scales() + custom_theme


# Oxygen uptake plot
vo2_1 <- intervals_mean |> 
  ggplot(aes(interaction(condition, intensity), mean_vo2)) +
  geom_point(size = 1, colour = grey) +
  geom_line(aes(group = interaction(pid, intensity)), colour = grey) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    position = nudge_points
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1,
    position = nudge_points
  ) +
  coord_cartesian(ylim = c(10, 50)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab(bquote("VO"[2] * " (mL·kg"^{-1} * "·min"^{-1} * ")")) +
  set_scales() + custom_theme


# %VO2peak plot
vo2_2 <- intervals_mean |> 
  ggplot(aes(interaction(condition, intensity), mean_vo2peak)) +
  geom_point(size = 1, colour = grey) +
  geom_line(aes(group = interaction(pid, intensity)), colour = grey) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    position = nudge_points
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1,
    position = nudge_points
  ) +
  coord_cartesian(ylim = c(20, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab(bquote("%VO"[2][p][e][a][k])) +
  set_scales() + custom_theme


# Work rate (METs) plot
wr_1 <- intervals_mean |> 
  ggplot(aes(interaction(condition, intensity), mean_wr)) +
  geom_point(size = 1, colour = grey) +
  geom_line(aes(group = interaction(pid, intensity)), colour = grey) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    position = nudge_points
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1,
    position = nudge_points
  ) +
  coord_cartesian(ylim = c(5, 15)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Work rate (METs)") +
  set_scales() + custom_theme


# %WRpeak plot
wr_2 <- intervals_mean |> 
  ggplot(aes(interaction(condition, intensity), mean_wrpeak)) +
  geom_point(size = 1, colour = grey) +
  geom_line(aes(group = interaction(pid, intensity)), colour = grey) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    position = nudge_points
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.1,
    position = nudge_points
  ) +
  coord_cartesian(ylim = c(20, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab(bquote("%WR"[p][e][a][k])) +
  set_scales() + custom_theme


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
# ggsave("./R/out/fig_marginal_means.pdf", plt, width = 170, height = 171, units = "mm")
