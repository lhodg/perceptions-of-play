library(tidyverse)
library(rlang)
library(ggpubr)
library(rstatix)
library(stats)

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

# Pivot data wide
wide_data <- edited_data %>%
  pivot_wider(
    id_cols = c(subject_role,
                subject_id,
                play_type),
    names_from = skill,
    values_from = response
  )

# Create composite score
composite_data <- wide_data %>%
  mutate(
    composite = (social + emotional + communication) / 3
  )

# View descriptive statistics
edited_data %>%
  group_by(subject_role, play_type) %>%
  get_summary_stats(response, show = c("mean", "sd", "min", "max","se"))

composite_data %>%
  group_by(subject_role, play_type) %>%
  get_summary_stats(composite, show = c("mean", "sd"))

composite_data %>%
  group_by(subject_role, play_type) %>%
  get_summary_stats(academic, show = c("mean", "sd"))

edited_data %>%
  group_by (subject_role) %>%
  get_summary_stats(response, show = c("mean", "sd"))

# -------------------------
# Hypothesis 1: both teachers and parents view pretend play as more
# important for social/emotional/communication

composite_data_long <- composite_data %>%
  select(
    subject_id,
    subject_role,
    play_type,
    developmental = composite,
    academic
  ) %>%
  pivot_longer(
    cols = c(developmental, academic),
    names_to = "skill",
    values_to = "response"
  )

anova_summary <- composite_data_long %>%
  group_by(
    skill
  ) %>%
  anova_test(
    dv = response,
    wid = c(subject_id),
    between = subject_role,
    within = play_type
  )

anova_summary_df <- get_anova_table(anova_summary)
anova_summary_df

anova_summary2 <- composite_data_long %>%
  group_by(
    subject_role,
    skill
  ) %>%
  anova_test(
    dv = response,
    wid = c(subject_id),
    within = play_type
  )

anova_summary_df2 <- get_anova_table(anova_summary2)
anova_summary_df2

t_test <- composite_data_long %>%
  filter(
    skill != "academic"
  ) %>%
  group_by(
    subject_role
  ) %>%
  pairwise_t_test(
    response ~ play_type,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    alternative = "less",
    detailed = TRUE
  )

t_test

wilcox.test(
  Pair(composite_pp, composite_ic) ~ 1,
  data = composite_data_teachers,
  alternative = "greater"
)

wilcox.test(
  Pair(composite_pp, composite_ic) ~ 1,
  data = composite_data_parents,
  alternative = "greater"
)

# ---------------------------
# Hypothesis 2: teachers will view pretend play as more important for development
# compared with parents
anova_summary3 <- composite_data_long %>%
  filter(skill != "academic", play_type != "ic") %>%
  group_by(
    play_type,
    skill
  ) %>%
  anova_test(
    dv = response,
    wid = c(subject_id),
    between = subject_role
  )

anova_summary_df3 <- get_anova_table(anova_summary3)
anova_summary_df3

# -------------------------
# Hypothesis 3: no difference between parents and teachers in beliefs on
# imaginary play for school readiness

anova_summary4 <- composite_data_long %>%
  filter(
    skill == "academic"
  ) %>%
  group_by(
    play_type,
    skill
  ) %>%
  anova_test(
    dv = response,
    wid = c(subject_id),
    between = subject_role
  )

anova_summary_df4 <- get_anova_table(anova_summary4)
anova_summary_df4

# ------------------------
# Hypothesis 4: teachers likely to view pretend play as more important than
# parents for academic readiness

anova_summary5 <- composite_data_long %>%
  filter(skill != "developmental", play_type != "ic") %>%
  group_by(
    play_type,
    skill
  ) %>%
  anova_test(
    dv = response,
    wid = c(subject_id),
    between = subject_role
  )
get_anova_table(anova_summary5)
