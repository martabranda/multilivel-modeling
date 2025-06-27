# ==============================================================================
# SETUP AND DATA LOADING
# ==============================================================================

# Load all required packages efficiently
require("pacman")
p_load("dplyr",      # Data manipulation
       "ggplot2",    # Visualization
       "lme4",       # Mixed models
       "lmerTest",   # P-values for mixed models
       "lavaan",     # SEM/CFA
       "lubridate",  # Date/time manipulation
       "padr",       # Time series padding
       "semTools",   # SEM tools
       "sjPlot")     # Model visualization

# Load conscientiousness longitudinal data
dt <- `conscientiousness_long.(2)`

# ==============================================================================
# DESCRIPTIVE ANALYSIS
# ==============================================================================

# Between-person variability: mean conscientiousness per person
person_means <- aggregate(conscientious ~ ID, dt, FUN = function(x) mean(x, na.rm = TRUE))
hist(person_means$conscientious,
     main = "Distribution of Person Means - Conscientiousness",
     xlab = "Mean Conscientiousness Score",
     col = "lightblue")

# Within-person variability: SD per person
person_sd <- aggregate(conscientious ~ ID, dt, FUN = function(x) sd(x, na.rm = TRUE))
hist(person_sd$conscientious,
     main = "Distribution of Within-Person Variability",
     xlab = "Standard Deviation of Conscientiousness",
     col = "lightcoral")

# ==============================================================================
# INTRACLASS CORRELATION (ICC)
# ==============================================================================

# Null model to partition variance
fit.ICC <- lmer(conscientious ~ 1 + (1|ID), data = dt, REML = FALSE)
random.var.c <- as.data.frame(VarCorr(fit.ICC))

# Calculate ICC
between_var <- random.var.c$vcov[1]
within_var <- random.var.c$vcov[2]
ICC <- between_var / (between_var + within_var)

cat("Variance Components:\n")
cat("Between-person variance:", round(between_var, 3), "\n")
cat("Within-person variance:", round(within_var, 3), "\n")
cat("ICC:", round(ICC, 3), "\n")
cat("Interpretation:", round(ICC*100, 1), "% of variance is between-person\n\n")

# ==============================================================================
# MULTILEVEL CFA - RELIABILITY ANALYSIS
# ==============================================================================

# Two-level CFA model for conscientiousness construct
model1 <- 'level: 1
  C =~ conscientious + diligent + disciplined 
level: 2
  C =~ conscientious + diligent + disciplined'

# Fit multilevel CFA
fit1 <- lavaan::cfa(model1, dt, cluster = "ID")
summary(fit1)

# Calculate reliability at both levels
reliability_results <- semTools::reliability(fit1)
print(reliability_results)

# ==============================================================================
# COMPOSITE SCORE CREATION
# ==============================================================================

# Create composite conscientiousness score
dt$C <- rowMeans(dt[, c("conscientious", "diligent", "disciplined")], na.rm = TRUE)

# ==============================================================================
# TIME TREND ANALYSIS
# ==============================================================================

# Model with random intercepts and slopes for time
fit_time <- lmer(C ~ day + (day||ID), data = dt, REML = FALSE)
summary(fit_time)

# Visualize individual trajectories (subset for clarity)
dt_subgroup <- dt %>% 
  filter(ID %in% c("ID2", "ID4", "ID5"))

p_trajectories <- ggplot(dt_subgroup, aes(x = day, y = C, color = ID)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = 0.3) +
  theme_bw() +
  labs(title = "Individual Trajectories of Conscientiousness",
       x = "Day",
       y = "Conscientiousness Composite Score") +
  theme(legend.position = "bottom")

print(p_trajectories)

# ==============================================================================
# WITHIN-PERSON ASSOCIATIONS
# ==============================================================================

# Random intercept and slope model: Effect of laziness on conscientiousness
fit.c <- lmer(C ~ lazy + (lazy|ID), data = dt, REML = FALSE)
summary(fit.c)

cat("\nWithin-person association interpretation:\n")
cat("For a 1-unit increase in laziness (within-person),\n")
cat("conscientiousness decreases by", round(fixef(fit.c)["lazy"], 3), "units\n\n")

# ==============================================================================
# LAGGED ANALYSIS PREPARATION
# =========================