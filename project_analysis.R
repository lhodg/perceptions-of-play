library(tidyverse)
library(rlang)
library(ggpubr)
library(rstatix)

# Read CSV file to R
raw_data <- read_csv("cleaned_data.csv")

# format data for analysis
edited_data <- raw_data %>%
  rowid_to_column("subject_id") %>%
  rename(
    subject_role ="P or a T?"
  ) %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = -c(subject_id, subject_role),
    names_to = c("play_type", "skill"),
    names_pattern = "(ic|pp).*(social|emotional|communication|academic)",
    values_to = "response"
  )

# View descriptive statistics
edited_data %>%
  group_by(subject_role, play_type) %>%
  get_summary_stats(response, show = c("mean", "sd", "min", "max"))

wide_data <- edited_data %>%
  pivot_wider(
    id_cols = c(subject_role,
                subject_id,
                play_type),
    names_from = skill,
    values_from = response
  )

# Summary ANOVA of all data

anova_1a_parents <- anova_test(
  data = edited_data,
  dv = response,
  wid = subject_id,
  within = c(play_type, skill)
)
summary(anova_1a_parents)

# Create composite score
composite_data <- wide_data %>%
  mutate(
    composite = (social + emotional + communication) / 3
  )

# This form better for t-tests (above is better for plotting)
composite_data_wide <- composite_data %>%
  pivot_wider(
    id_cols = c(subject_id, subject_role),
    names_from = play_type,
    values_from = social:composite
  )

# -------------------------
# Hypothesis 1: both teachers and parents view pretend play as more
# important for social/emotional/communication

# Plot
composite_data %>%
  unite(
    subject_and_play,
    c(subject_role, play_type)
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(
      subject_and_play,
      composite
    )
  )

# tests for teachers ---------
composite_data_teachers <- composite_data_wide %>%
  filter(
    subject_role == "T"
  )

# paired t-test (assumptions violated, but robust)
t.test(
  Pair(composite_pp, composite_ic) ~ 1,
  data = composite_data_teachers,
  alternative = "greater"
)

# fewer assumptions. Can't calculate exact p-value in this analysis
wilcox.test(
  Pair(composite_pp, composite_ic) ~ 1,
  data = composite_data_teachers,
  alternative = "greater"
)

# same tests for parents ------
composite_data_parents <- composite_data_wide %>%
  filter(
    subject_role == "P"
  )

t.test(
  Pair(composite_pp, composite_ic) ~ 1,
  data = composite_data_teachers,
  alternative = "greater"
)

# t.test(
#   x = composite_data_teachers$composite_pp,
#   y = composite_data_teachers$composite_ic,
#   alternative = "greater",
#   paired = TRUE
# )

wilcox.test(
  Pair(composite_pp, composite_ic) ~ 1,
  data = composite_data_teachers,
  alternative = "greater"
)

# ---------------------------
# Hypothesis 2: teachers will view pretend play as more important for development
# compared with parents

composite_data %>%
  filter(
    play_type == "pp"
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(
      subject_role,
      composite
    )
  )

t.test(
  composite_pp ~ subject_role,
  data = composite_data_wide,
  alternative = "less"
)

# t.test(
#   composite_data_wide$composite_pp[composite_data_wide$subject_role == "P"],
#   composite_data_wide$composite_pp[composite_data_wide$subject_role == "T"],
#   alternative = "less"
# )

wilcox.test(
  composite_pp ~ subject_role,
  data = composite_data_wide,
  alternative = "less"
)

# -------------------------
# Hypothesis 3: no difference between parents and teachers in beliefs on
# imaginary play for school readiness

composite_data %>%
  filter(
    play_type == "ic"
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(
      subject_role,
      academic
    )
  )

t.test(
  academic_ic ~ subject_role,
  data = composite_data_wide,
  alternative = "two.sided"
)

wilcox.test(
  academic_ic ~ subject_role,
  data = composite_data_wide,
  alternative = "two.sided"
)

# ------------------------
# Hypothesis 4: teachers likely to view pretend play as more important than
# parents for academic readiness

composite_data %>%
  filter(
    play_type == "pp"
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(
      subject_role,
      academic
    )
  )

t.test(
  academic_pp ~ subject_role,
  data = composite_data_wide,
  alternative = "less"
)

wilcox.test(
  academic_pp ~ subject_role,
  data = composite_data_wide,
  alternative = "less"
)
