library(here)
source('scripts/data_cleaning.R')
req(sesame_transf)

# Summary statistics
sum_stats <- sesame_transf |>
  select(
    improve_body,
    improve_let,
    improve_form,
    improve_numb,
    improve_relat,
    improve_clasf
  ) |>
  pivot_longer(cols = everything(),
               names_to = "Test",
               values_to = "Improvement") |>
  group_by(Test) |>
  summarize(
    Min = min(Improvement),
    Q1 = quantile(Improvement, 0.25),
    Median = median(Improvement),
    Mean = mean(Improvement),
    Q3 = quantile(Improvement, 0.75),
    Max = max(Improvement)
  ) |>
  kable(digits = 2)

# Mean improvement by viewing category
impr_view_cat <- sesame_transf |>
  group_by(viewcat) |>
  summarize(
    body = mean(improve_body),
    letter = mean(improve_let),
    form = mean(improve_form),
    number = mean(improve_numb),
    relation = mean(improve_relat),
    classify = mean(improve_clasf)
  ) |>
  kable(
    digits = 2,
    caption = "Table 1: Mean Improvement by Viewing Category"
  )

# Boxplot distribution of all improvement scores
impr_dist <- sesame_transf |>
  select(
    improve_body, 
    improve_let, 
    improve_form, 
    improve_numb, 
    improve_relat, 
    improve_clasf
  ) |>
  pivot_longer(cols = everything()) |>
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Figure 1: Distributions of All Improvement Scores",
       x = "Skill Area",
       y = "Score Improvement")

# Histogram of peabody distribution
hist_peabody <- ggplot(sesame_transf, aes(peabody)) +
  geom_histogram(
    bins = 25,
    fill = "thistle3",
    color = "dimgrey",
    alpha = 0.8
  ) +
  labs(
    title = "Figure 2: Distribution of Peabody Vocabulary Scores",
    x = "Peabody Score",
    y = "Count"
  )

# Histogram of improvement score for letters test
hist_let <- ggplot(sesame_transf, aes(improve_let)) +
  geom_histogram(
    bins = 25, 
    fill = "lightskyblue3",
    color = "dimgrey"
  ) +
  labs(title = "Distribution of Letter Score Improvement",
       x = "Improvement (Post - Pre)",
       y = "Count")

# Histogram of improvement score for numbers test
hist_numb <- ggplot(sesame_transf, aes(improve_numb)) +
  geom_histogram(
    bins = 25, 
    fill = "lightskyblue3",
    color = "dimgrey"
  ) +
  labs(title = "Distribution of Numbers Score Improvement",
       x = "Improvement (Post - Pre)",
       y = "Count")

# Interaction between viewcat and setting on improve_let
viewcat_intr_improve_let <- ggplot(sesame_transf, aes(
  x = factor(viewcat),
  y = improve_let,
  fill = factor(setting)
)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("1" = "thistle3", "2" = "lightskyblue3"),
    labels = c("1" = "Home", "2" = "School")
  ) + 
  labs(
    title = "Figure 3: Interaction of Viewing Category and Educational Setting on Letter Improvement",
    x = "Viewing Category",
    y = "Letter Improvement",
    fill = "Educational Setting"
  )