library(lme4)      
library(psych)     
library(ggplot2)   
library(dplyr)    
library(lavaan)
library(ggeffects)

# ==============================================================================
# MULTILEVEL CFA FOR RELIABILITY
# ==============================================================================

# Two-level CFA model for unconscientiousness construct
mdl2 <- 'level: 1
 irul =~  imprecise + reckless + unreliable + lazy 
level: 2
 irul =~ imprecise + reckless + unreliable + lazy'

fit_2 <- lavaan::cfa(mdl2, 
                     data = dt_lag, 
                     cluster = "ID")
summary(fit_2)

# ==============================================================================
# COMPOSITE SCORE CREATION
# ==============================================================================

# Create unconscientiousness composite (UNC)
dt_lag$UNC <- rowMeans(dt_lag[, c("imprecise", "reckless", "unreliable", "lazy")], 
                       na.rm = TRUE)
summary(dt_lag$UNC)

# ==============================================================================
# WITHIN/BETWEEN-PERSON DECOMPOSITION
# ==============================================================================

# Decompose UNC into within and between components
dt_lag$UNC_between <- ave(dt_lag$UNC, dt_lag$ID, FUN = function(x) mean(x, na.rm = TRUE))
UNC_between_wide <- aggregate(UNC ~ ID, dt_lag, FUN = function(x) mean(x, na.rm = TRUE))
UNC_grand_mean <- mean(UNC_between_wide$UNC)
dt_lag$UNC_within <- dt_lag$UNC - dt_lag$UNC_between
dt_lag$UNC_between_centered <- dt_lag$UNC_between - UNC_grand_mean

# Decompose irresponsible variable similarly
dt_lag$irr_between <- ave(dt_lag$irresponsible, dt_lag$ID, FUN = function(x) mean(x, na.rm = TRUE))
irr_between_wide <- aggregate(irresponsible ~ ID, dt_lag, FUN = function(x) mean(x, na.rm = TRUE))
irr_grand_mean <- mean(irr_between_wide$irresponsible)
dt_lag$irr_within <- dt_lag$irresponsible - dt_lag$irr_between
dt_lag$irr_between_centered <- dt_lag$irr_between - irr_grand_mean

# ==============================================================================
# LAGGED VARIABLES CREATION
# ==============================================================================

dt_lag <- dt_lag %>%
  group_by(ID) %>%
  mutate(
    time = row_number(),
    UNC_within_lag1 = lag(UNC_within, 1),
    irr_within_lag1 = lag(irr_within, 1)
  ) %>%
  ungroup()

# ==============================================================================
# LAGGED ASSOCIATION MODEL
# ==============================================================================

# Predict UNC from lagged irresponsible with controls
model_UNC <- lmer(UNC ~ irr_within_lag1 + UNC_within_lag1 + time + 
                    irr_between_centered + (1|ID),
                  data = dt_lag,
                  REML = TRUE)
summary(model_UNC)

# ==============================================================================
# CONTEMPORANEOUS ASSOCIATION WITH AGE MODERATION
# ==============================================================================

# Center age variable
dt_lag$age_centered <- scale(dt_lag$age, scale = FALSE)

# Model with interaction
model_UNC_age <- lmer(UNC ~ irresponsible + age_centered + irresponsible:age_centered + 
                        time + irr_between_centered + (1|ID),
                      data = dt_lag,
                      REML = TRUE)
summary(model_UNC_age)

# ==============================================================================
# INTERACTION VISUALIZATION
# ==============================================================================

# Generate predictions for interaction
interact_df <- ggeffects::ggpredict(model_UNC_age, 
                                    terms = c("irresponsible", "age_centered[-1,0,1]"))

# Create interaction plot
ggplot(interact_df, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  scale_color_viridis_d(name = "Age (Centered)", 
                        labels = c("-1 SD", "Mean", "+1 SD")) +
  scale_fill_viridis_d(guide = "none") +
  labs(x = "Irresponsible", 
       y = "Predicted Unconscientiousness",
       title = "Age Moderates the Irresponsible-Unconscientiousness Association",
       caption = "Shaded areas represent 95% confidence intervals") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))