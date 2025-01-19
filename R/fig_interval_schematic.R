library(ggplot2)

data <- data.frame(
  time = c(0, 0, 3, 3, 5, 5, 8, 8, 10, 10, 13, 13, 15, 15, 18, 18, 20, 20, 23, 23, 25, 25, 28, 28),
  rpe = c(6, 13, 13, 8, 8, 15, 15, 8, 8, 13, 13, 8, 8, 15, 15, 8, 8, 13, 13, 8, 8, 15, 15, 6)
)

data |> 
  ggplot(aes(x = time, y = rpe)) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(6, 20, by = 2),
    limits = c(6, 20),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(0, 28, by = 4),
    expand = expansion(mult = 0.125)
  ) +
  xlab("Time (min)") +
  ylab("RPE") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8, colour = "black"),
    axis.ticks = element_line(colour = "black"),
    plot.margin = margin()
  )

# Uncomment to save
# ggsave("./R/out/fig_interval_schematic.pdf", width = 85, height = 60, units = "mm")
