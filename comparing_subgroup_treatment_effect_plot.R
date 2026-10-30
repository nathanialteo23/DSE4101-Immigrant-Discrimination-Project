library(tidyverse)

df <- tribble(
  ~Moderator, ~Subgroup, ~Method, ~Estimate, ~Lower, ~Upper,
  "Language skills", "Absent", "Regression", -4.06, -5.70, -2.42,
  "Language skills", "Present", "Regression", -5.50, -8.78, -2.21,
  "Extracurricular activities", "Absent", "Regression", -6.28, -8.83, -3.72,
  "Extracurricular activities", "Present", "Regression", -3.20, -5.18, -1.23,
  "Same-firm experience", "Absent", "Regression", -5.81, -8.32, -3.30,
  "Same-firm experience", "Present", "Regression", -2.88, -5.94, 0.19,
  
  "Language skills", "Absent", "Causal forest", -4.14, -5.88, -2.40,
  "Language skills", "Present", "Causal forest", -5.42, -9.02, -1.82,
  "Extracurricular activities", "Absent", "Causal forest", -6.03, -8.75, -3.32,
  "Extracurricular activities", "Present", "Causal forest", -3.43, -5.53, -1.34,
  "Same-firm experience", "Absent", "Causal forest", -5.68, -8.35, -3.01,
  "Same-firm experience", "Present", "Causal forest", -2.96, -6.28, 0.35
)


library(ggplot2)
library(dplyr)

# Use the df created earlier containing both methods
df$Moderator <- factor(
  df$Moderator,
  levels = c("Language skills",
             "Extracurricular activities",
             "Same-firm experience")
)

df$Subgroup <- factor(
  df$Subgroup,
  levels = c("Absent", "Present"),
  labels = c("0", "1")
)

df$Method <- factor(
  df$Method,
  levels = c("Regression", "Causal forest")
)

# Stagger the estimates and confidence intervals
dodge <- position_dodge(width = 0.45)

p <- ggplot(
  df,
  aes(
    x = Subgroup,
    y = Estimate,
    color = Method,
    group = Method
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    position = dodge,
    width = 0.12,
    linewidth = 1
  ) +
  geom_point(
    position = dodge,
    size = 3.5
  ) +
  facet_wrap(~ Moderator, nrow = 1) +
  scale_color_manual(
    values = c(
      "Regression" = "#E69F00",
      "Causal forest" = "#0072B2"
    )
  ) +
  labs(
    title = "Comparing Subgroup Treatment Effects",
    subtitle = "Interaction regression vs causal forest (95% confidence intervals)",
    x = "Covariate value",
    y = "Name effect (percentage points)",
    color = "Method"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      size = 22, face = "bold"
    ),
    plot.subtitle = element_text(
      size = 15, face = "bold"
    ),
    strip.text = element_text(
      size = 16, face = "bold"
    ),
    axis.title = element_text(
      size = 16, face = "bold"
    ),
    axis.text = element_text(
      size = 14, face = "bold"
    ),
    legend.title = element_text(
      size = 14, face = "bold"
    ),
    legend.text = element_text(
      size = 13, face = "bold"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  )

print(p)
