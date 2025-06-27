# Load required packages
library(haven)       # SPSS file handling
library(tidyverse)   # Data manipulation and visualization
library(lme4)        # Linear mixed models
library(lmerTest)    # P-values for mixed models
library(emmeans)     # Estimated marginal means
library(multcomp)    # Multiple comparisons
library(effectsize)  # Effect size calculations
library(ggplot2)     # Advanced plotting

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

# Convert effectiveness ratings to numeric
EffPromGiu <- EffPromGiu %>%
  mutate(across(starts_with("Q27_efficaciapromsal_"), 
                ~as.numeric(as.character(.))))

# Reshape to long format for repeated measures analysis
data_long <- EffPromGiu %>%
  pivot_longer(
    cols = starts_with("Q27_efficaciapromsal_"),
    names_to = "Item",
    values_to = "Valutazione"
  )

# ==============================================================================
# MIXED EFFECTS MODEL
# ==============================================================================

# Random intercept model: accounts for within-subject correlation
model <- lmer(Valutazione ~ Item + (1|ID), data = data_long)

# Model summary and ANOVA
summary(model)
anova(model)

# ==============================================================================
# ESTIMATED MARGINAL MEANS
# ==============================================================================

# Calculate EMMs for each health promotion strategy
emm_results <- emmeans(model, "Item")
print(emm_results)

# Sort strategies by effectiveness
emm_results %>%
  as.data.frame() %>%
  arrange(desc(emmean)) %>%
  print(n = 20)

# ==============================================================================
# POST-HOC COMPARISONS
# ==============================================================================

# Pairwise comparisons with Bonferroni correction
post_hoc <- pairs(emm_results, adjust = "bonferroni")

# Display all comparisons
summary(post_hoc, infer = TRUE)

# Show only significant differences
significant_pairs <- summary(post_hoc, infer = TRUE) %>% 
  filter(p.value < 0.05)
print(significant_pairs)

# Export results
write.csv(summary(post_hoc, infer = TRUE), "post_hoc_results.csv", row.names = FALSE)

# Alternative corrections for comparison
post_hoc_fdr <- pairs(emm_results, adjust = "fdr")    # False Discovery Rate
post_hoc_tukey <- pairs(emm_results, adjust = "tukey") # Tukey HSD

# ==============================================================================
# VARIANCE COMPONENTS AND EFFECT SIZES
# ==============================================================================

# Variance components
var_components <- VarCorr(model)
print(var_components)

# Calculate ICC manually
var_id <- as.numeric(VarCorr(model)$ID[1])
var_res <- attr(VarCorr(model), "sc")^2
ICC <- var_id / (var_id + var_res)
cat("Intraclass Correlation Coefficient (ICC):", round(ICC, 3), "\n")
cat("Interpretation: ", round(ICC*100, 1), "% of variance is due to between-subject differences\n")

# Effect size (eta squared)
eta_sq <- eta_squared(anova(model))
print(eta_sq)

# ==============================================================================
# VISUALIZATION
# ==============================================================================

# Basic plot with confidence intervals
p1 <- plot(emm_results) + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Health Promotion Strategies Effectiveness",
       x = "Strategy")

# Enhanced visualization with sorted strategies
p2 <- emm_results %>%
  as.data.frame() %>%
  ggplot(aes(x = reorder(Item, emmean), y = emmean)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, color = "darkblue", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  geom_hline(yintercept = 4, linetype = "dashed", alpha = 0.5, color = "red") +
  labs(x = "Health Promotion Strategy", 
       y = "Perceived Effectiveness (1-7)", 
       title = "Effectiveness of Health Promotion Strategies",
       subtitle = "Ordered by mean effectiveness with 95% CIs",
       caption = "Red line indicates scale midpoint") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11))

# Display plot
print(p2)

# Save plot
ggsave("health_promotion_effectiveness.png", p2, width = 10, height = 8, dpi = 300)

# ==============================================================================
# ANALYSIS SUMMARY
# ==============================================================================

cat("\n========== ANALYSIS SUMMARY ==========\n")
cat("Number of strategies evaluated:", length(unique(data_long$Item)), "\n")
cat("Number of participants:", length(unique(data_long$ID)), "\n")
cat("Total observations:", nrow(data_long), "\n")
cat("\nModel Type: Linear Mixed Model with random intercepts\n")
cat("Multiple comparisons correction: Bonferroni\n")
cat("Number of significant pairwise differences:", nrow(significant_pairs), "\n")