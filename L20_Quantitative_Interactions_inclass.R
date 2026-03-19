# ==============================================================================
# MA376 Lesson 20: Quantitative Interactions
# ==============================================================================
#
# YOUR TASK: Find the best model for your line's downtime data.
#
# DELIVERABLES (email to instructor before class ends):
#   1. Your fitted model equation
#   2. One table (summary stats, ANOVA comparison, etc.)
#   3. Up to two visualizations
#
# A random OTHER pair will explain your model + viz next class.
# Best explanation = bonus point for all 4 cadets.
#
# ==============================================================================

library(tidyverse)
library(GGally)

# ==============================================================================
# BUILDING A BETTER PLOT: Iterative Refinement
# ==============================================================================
# The push-up interaction model: y = 20 + 4*Sleep + 0.3*Warmup + 0.2*Sleep*Warmup
# We'll take a basic plot through 5 versions --- each adds ONE idea.

predict_pushups <- function(sleep, warmup) {
  20 + 4 * sleep + 0.3 * warmup + 0.2 * sleep * warmup
}

grid <- expand.grid(Sleep = seq(4, 10, by = 0.5), Warmup = c(5, 10, 15))
grid$Pushups <- predict_pushups(grid$Sleep, grid$Warmup)
label_df <- grid %>% filter(Sleep == max(Sleep))

# ---- V1: The default --------------------------------------------------------
ggplot(grid, aes(x = Sleep, y = Pushups, color = factor(Warmup))) +
  geom_line()
# Gray background, legend off to the side, no labels. A reader has to work.

# ---- V2: Declutter -----------------------------------------------------------
ggplot(grid, aes(x = Sleep, y = Pushups, color = factor(Warmup))) +
  geom_line(linewidth = 1.1) +
  labs(x = "Sleep (hours)", y = "Predicted Push-ups", color = "Warmup (min)") +
  theme_minimal()
# Better. But the legend still forces the eye to bounce back and forth.

# ---- V3: Direct labels, kill the legend --------------------------------------
ggplot(grid, aes(x = Sleep, y = Pushups, color = factor(Warmup))) +
  geom_line(linewidth = 1.1) +
  geom_text(data = label_df,
            aes(label = paste0(Warmup, " min")),
            hjust = -0.1, fontface = "bold", size = 3.5) +
  labs(x = "Sleep (hours)", y = "Predicted Push-ups") +
  coord_cartesian(xlim = c(4, 11.2)) +
  theme_minimal() +
  theme(legend.position = "none")
# Eye goes straight from line to label. Zero legend lookup.

# ---- V4: Gray out context, highlight the story -------------------------------
# The story: "More warmup amplifies the benefit of sleep."
# Gray the middle line. Color only the comparison that matters.

ggplot(grid, aes(x = Sleep, y = Pushups, group = factor(Warmup))) +
  geom_line(data = grid %>% filter(Warmup == 10),
            color = "gray75", linewidth = 1) +
  geom_line(data = grid %>% filter(Warmup != 10),
            aes(color = factor(Warmup)), linewidth = 1.3) +
  scale_color_manual(values = c("5" = "#4393C3", "15" = "#D6604D")) +
  geom_text(data = label_df %>% filter(Warmup != 10),
            aes(label = paste0(Warmup, " min"), color = factor(Warmup)),
            hjust = -0.1, fontface = "bold", size = 3.5) +
  geom_text(data = label_df %>% filter(Warmup == 10),
            aes(label = "10 min"),
            hjust = -0.1, color = "gray60", size = 3.2) +
  labs(x = "Sleep (hours)", y = "Predicted Push-ups") +
  coord_cartesian(xlim = c(4, 11.5)) +
  theme_minimal() +
  theme(legend.position = "none")
# Your eye goes straight to the comparison. The 10-min line is context, not noise.

# ---- V5: Annotate the insight ------------------------------------------------
# State the takeaway. The reader shouldn't have to figure it out.

ggplot(grid, aes(x = Sleep, y = Pushups, group = factor(Warmup))) +
  geom_line(data = grid %>% filter(Warmup == 10),
            color = "gray75", linewidth = 1) +
  geom_line(data = grid %>% filter(Warmup != 10),
            aes(color = factor(Warmup)), linewidth = 1.3) +
  scale_color_manual(values = c("5" = "#4393C3", "15" = "#D6604D")) +
  geom_text(data = label_df %>% filter(Warmup != 10),
            aes(label = paste0(Warmup, " min"), color = factor(Warmup)),
            hjust = -0.1, fontface = "bold", size = 3.5) +
  geom_text(data = label_df %>% filter(Warmup == 10),
            aes(label = "10 min"),
            hjust = -0.1, color = "gray60", size = 3.2) +
  annotate("segment", x = 8, xend = 8, y = 61.5, yend = 80.5,
           arrow = arrow(ends = "both", length = unit(0.08, "inches")),
           color = "gray30") +
  annotate("text", x = 8.3, y = 71, size = 3, color = "gray30", hjust = 0,
           label = "+14 push-ups\nwith 15-min warmup") +
  annotate("text", x = 5, y = 82, size = 3.8, fontface = "bold", hjust = 0,
           label = "Warming up amplifies\nthe benefit of sleep") +
  labs(x = "Sleep (hours)", y = "Predicted Push-ups") +
  coord_cartesian(xlim = c(4, 11.5)) +
  theme_minimal() +
  theme(legend.position = "none")
# This plot tells a story. A reader gets the point in 3 seconds.
# That's the goal for your competition submission.


# ==============================================================================
# COMPETITION: Production Line Downtime
# ==============================================================================

# --- STEP 1: Load your line's data -------------------------------------------
# Replace "alpha" with your assigned line:
#   alpha | bravo | charlie | delta | echo | foxtrot | golf | hotel

my_line_name <- "alpha"  # <-- CHANGE THIS

url <- paste0("https://raw.githubusercontent.com/defense031/MA376-L20/master/data/line_",
              my_line_name, ".csv")
my_line <- read_csv(url)
head(my_line)

# --- STEP 2: Explore ---------------------------------------------------------
ggpairs(my_line)

# --- STEP 3: Fit models ------------------------------------------------------
# Start additive. Then add the interaction you suspect. Compare with anova().

model_add <- lm(Downtime_hrs ~ Temp_C + Pressure_psi, data = my_line)
model_int <- lm(Downtime_hrs ~ Temp_C * Pressure_psi, data = my_line)

summary(model_int)
anova(model_add, model_int)

# Full additive model (all 6 predictors)
model_full <- lm(Downtime_hrs ~ Temp_C + Pressure_psi + Speed_uph +
                   Humidity_pct + Material_Score + Cycle_Time_min,
                 data = my_line)
summary(model_full)

# --- STEP 4: Visualize the interaction ----------------------------------------
# Bin one variable into Low/Med/High. Plot the other on x-axis.
# Non-parallel slopes = interaction. Apply V3-V5 techniques above.

my_line <- my_line %>%
  mutate(Pressure_group = cut(Pressure_psi,
                               breaks = quantile(Pressure_psi, c(0, 0.33, 0.67, 1)),
                               labels = c("Low", "Med", "High"),
                               include.lowest = TRUE))

ggplot(my_line, aes(x = Temp_C, y = Downtime_hrs, color = Pressure_group)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Temperature (C)", y = "Downtime (hrs/wk)", color = "Pressure") +
  theme_minimal()

# --- STEP 5: 3D scatter (optional) -------------------------------------------
# install.packages("plotly")  # run once if needed
# library(plotly)
# plot_ly(my_line, x = ~Temp_C, y = ~Pressure_psi, z = ~Downtime_hrs,
#         type = "scatter3d", mode = "markers",
#         marker = list(size = 4, color = ~Downtime_hrs,
#                       colorscale = "Viridis"))
