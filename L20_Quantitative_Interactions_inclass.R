# ==============================================================================
# MA376 Lesson 20: Production Line Competition
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
# STEP 1: Load your line's data
# ==============================================================================
# Replace "alpha" with your assigned line:
#   alpha | bravo | charlie | delta | echo | foxtrot | golf | hotel

my_line_name <- "alpha"  # <-- CHANGE THIS

url <- paste0("https://raw.githubusercontent.com/defense031/MA376-L20/master/data/line_",
              my_line_name, ".csv")
my_line <- read_csv(url)
head(my_line)

# ==============================================================================
# STEP 2: Explore
# ==============================================================================
# Pairs plot: which variables relate most to Downtime_hrs?
ggpairs(my_line)

# Heat-map scatter of two promising predictors
ggplot(my_line, aes(x = Temp_C, y = Pressure_psi, color = Downtime_hrs)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  theme_minimal()

# ==============================================================================
# STEP 3: Fit models
# ==============================================================================
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

# ==============================================================================
# STEP 4: Visualize the interaction
# ==============================================================================
# Bin one variable into Low/Med/High. Plot the other on x-axis.
# Non-parallel slopes = interaction.

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

# ==============================================================================
# STEP 5: 3D scatter (optional, looks cool)
# ==============================================================================
# install.packages("plotly")  # run once if needed
# library(plotly)
# plot_ly(my_line, x = ~Temp_C, y = ~Pressure_psi, z = ~Downtime_hrs,
#         type = "scatter3d", mode = "markers",
#         marker = list(size = 4, color = ~Downtime_hrs,
#                       colorscale = "Viridis"))
