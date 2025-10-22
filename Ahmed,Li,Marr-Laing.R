# C-Section Risk in Canada: The Pandemic Effect
# R Script for Data Visualization
# Authors: Maria Ahmed, Zilin Li, Kate Marr-Laing
# October 2025

# Loading libraries 
library(systemfonts)
library(tidyverse)
library(gganimate)
library(ggplot2)

# Loading data
csec_data <-read.csv("Plot_Data(Ahmed,Li,Marr-Laing).csv")

# Data cleaning and calculations of rate change
csec_data <- csec_data %>%
  arrange(csec_data) %>%
  mutate(csec_rate_change = c(0, diff(primary_csec_rate) / primary_csec_rate[1])) %>%
  mutate(hosp_rate_change = c(0, diff(standardized_hosp_rate_per_100000) / standardized_hosp_rate_per_100000[1])) %>%
  mutate(birth_rate_change = c(0, diff(birth_rate) / birth_rate[1]))

csec_data <- csec_data %>%
  select(c("fiscal_year", "csec_rate_change", "hosp_rate_change", "birth_rate_change"))

csec_long <- reshape2::melt(csec_data, id.vars = "fiscal_year")
csec_long <- csec_long %>%
  mutate(variable = as.factor(variable)) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(start_year = as.numeric(substring(fiscal_year, 1, 4))) 

csec_long <- csec_long %>%
  mutate(start_year_date = ymd(paste0(start_year, "-01-01")))

# defining order and colours for visualization
rate_order <- c("csec_rate_change", "hosp_rate_change", "birth_rate_change")
rate_colors <- c("hosp_rate_change" = "#0461CF", "birth_rate_change" = "#08AA53", "csec_rate_change" = "#873E8A")
text_label <- "All values standardized to\n2017-2018 pre-pandemic rates"

point_data <- csec_long %>%
  filter(start_year != "2017") 

# Creating plot for visualization
plot <- ggplot(csec_long, aes(x = start_year, y = value, color = variable, shape = variable, group = variable)) +
  geom_line(aes(x = start_year, y = value, color = variable, group = variable), linewidth = 1.6) +
  geom_point(data = point_data, 
             aes(x = start_year, y = value, color = variable, group = variable), size = 4.5) +
  theme_bw() +
  scale_x_continuous(breaks = csec_long$start_year, labels = csec_long$fiscal_year) +
  ylab("Change in Rate") +
  xlab("Fiscal Year") +
  theme(axis.text = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title.x = element_text(margin = margin(t=20), size = 32, face = "bold", hjust = 0.598),
        axis.title.y = element_text(margin = margin(r = 20), size = 32, face = "bold"),
        axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Calibri"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10),  # top, right, bottom, left
        legend.position = "inside",
        legend.justification = c(0.033, 1),
        legend.text = element_text(size = 23),
        legend.key.size = unit(2.1, 'lines'),
        legend.key.width = unit(3, "cm"),  
        legend.spacing.y = unit(2, "cm"),
        legend.title = element_blank(),
        axis.text.x = element_text(face = ifelse(levels(factor(csec_long$fiscal_year)) 
                                                 == "2020-2021", "bold", "plain")),) +
  geom_segment(aes(x = 2017, y = -0.055, yend = 0.01), color = "black", linewidth = 1.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black", size = 1.5) +
  geom_text(aes(x = 2017, y = -0.05, label = text_label), hjust = -0.05, color = "black", size = 6) + 
  theme(text = element_text(family = "Calibri")) +
  scale_color_manual(labels = expression(bold("Primary C-section rate") ~ "per 100 births", 
                                         bold("Hospitalization rate") ~ "per 100,000 people", 
                                         bold("Birth rate") ~ "per 1000 people"),
                     values = rate_colors) +
  scale_shape_manual(values = c("csec_rate_change" = 16,   # Circle
                                "hosp_rate_change" = 15,   # Square
                                "birth_rate_change" = 17)) +   # Triangle
  guides(color = guide_legend(override.aes = list(shape = c(16, 15, 17), size = 4.5, fill = NA)),
         shape = "none")

#Animating plot 
animated_plot <- plot + transition_reveal(start_year)
animate(animated_plot, width=1900, height=950, fps = 20, renderer=gifski_renderer(loop=TRUE), res = 120) 

