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

# ---------------------------------

# Hypothesis 1 & 2

data_1a <- edited_data %>%
  filter(skill != "academic")

data_1a_parents <- data_1a %>%
  filter(subject_role == "P")

data_1a_teachers <- data_1a %>%
  filter(subject_role == "T")

data_3 <- edited_data %>%
  list.filter (skill != "social") |

?list.filter()

anova_1a_parents <- anova_test(
  data = edited_data,
  dv = response,
  wid = subject_id,
  within = c(play_type, skill)
)
summary(anova_1a_parents)

polr_1a_parents <- MASS::polr(
  as.factor(response) ~ play_type,
  data = data_1a_parents
)
summary(polr_1a_parents)

lm_1a_parents <- lm(response ~ 0 + play_type, data_1a_parents)
summary(lm_1a_parents)

get_anova_table(lm_1a_parents)


anova_1a <- anova_test(
  data = edited_data %>% filter(skill != "academic"),
  dv = response,
  wid = subject_id,
  between = subject_role,
  within = c(play_type, skill)

  anova_1a <- anova_test(
    data = edited_data %>% filter(skill != "academic"),
    dv = response,
    wid = subject_id,
    between = subject_role,
    within = c(play_type, skill)

)
get_anova_table(anova_1a)


  anova_1a <- anova_test(
    data = edited_data %>% filter(subject_role != "T"),
    dv = response,
    wid = subject_id,
    between = subject_role,
    within = c(play_type, skill)
)
get_anova_table(anova_1a)


ggplot(edited_data, aes(subject_role, response, fill=skill))+
  stat_summary(fun=mean, geom="bar")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+
  facet_wrap(~ play_type)+
  labs(x="Role", y="Mean rating")+
  theme(legend.position="none")
