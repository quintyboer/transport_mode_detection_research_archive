
### load libraries -----------------------------------------------------------------------------------------------------------

require(ggplot2)


### function for saving plots ------------------------------------------------------------------------------------------------

# function to save a plot with standardised setting
# input: ggplot figure object and string with filename (including path)
# output: plot is saved to specified location
save_plot <- function(plot, # ggplot
                      filename) { # string with full pathname
  ggsave(plot,
         filename = filename,
         bg = "transparent",
         width = 6,
         height = 4,
         dpi = 300)
}


### define colorblind-friendly color scheme ------------------------------------

# color scheme from Paul Tol (2021)
color_scheme_bright <- c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB")
color_scheme_contrast <- c("#004488", "#BB5566", "#DDAA33", "#000000")


### boxplot of trip duration by transport mode -------------------------------------------------------------------------------

# generate plot with the prediction data (events_pred), which has the duration_mins variable
descriptives_duration_plot <- ggplot(data = events_pred) +
  geom_boxplot(aes(x = label_track, y = duration_mins, fill = label_track),
               outlier.size = 0.2, fatten = 0.75) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Transport mode", y = "Trip duration in minutes") +
  scale_fill_manual(values = color_scheme_bright)

# save duration boxplot
save_plot(descriptives_duration_plot,
          filename =  "output/fig2_descriptives_duration.png")


### boxtplot of median speed by transport mode -------------------------------------------------------------------------------

# generate plot with gps features data (filtered for events_pred), which has the med_speed_kmh variable
descriptives_speed_plot <- ggplot(data = events_gps[event_id %in% events_pred$event_id]) +
  geom_boxplot(aes(x = label_track, y = med_speed_kmh, fill = label_track),
               outlier.size = 0.2, fatten = 0.75) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Transport mode", y = "Median speed (km/h)") +
  ylim(0, 150) +
  scale_fill_manual(values = color_scheme_bright)

# save speed boxplot
save_plot(descriptives_speed_plot,
          filename = "output/fig3_descriptives_speed.png")

