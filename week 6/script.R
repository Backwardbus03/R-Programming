library(palmerpenguins)
library(tidyverse)
library(e1071)
library(car)
library(effsize)

data("penguins")
df <- penguins %>% drop_na(body_mass_g, sex, flipper_length_mm)

overall_stats <- df %>%
  summarise(
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Min = min(body_mass_g),
    Max = max(body_mass_g),
    Variance = var(body_mass_g),
    SD = sd(body_mass_g),
    Q1 = quantile(body_mass_g, 0.25),
    Q3 = quantile(body_mass_g, 0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )
print("Overall Descriptive Statistics for Body Mass:")
print(overall_stats)

species_stats <- df %>%
  group_by(species) %>%
  summarise(
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Min = min(body_mass_g),
    Max = max(body_mass_g),
    Variance = var(body_mass_g),
    SD = sd(body_mass_g),
    Q1 = quantile(body_mass_g, 0.25),
    Q3 = quantile(body_mass_g, 0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )
print("Species-wise Descriptive Statistics for Body Mass:")
print(species_stats)

ggplot(df, aes(x = body_mass_g)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Histogram of Body Mass", x = "Body Mass (g)", y = "Frequency")

ggplot(df, aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Body Mass by Species", x = "Body Mass (g)", y = "Density")

males <- df %>% filter(sex == "male") %>% pull(body_mass_g)
females <- df %>% filter(sex == "female") %>% pull(body_mass_g)

print("Shapiro-Wilk Test for Males:")
shapiro.test(males)
print("Shapiro-Wilk Test for Females:")
shapiro.test(females)

ttest_res <- t.test(body_mass_g ~ sex, data = df)
print("Independent Two-Sample t-test:")
print(ttest_res)

cohen_res <- cohen.d(body_mass_g ~ sex, data = df)
print("Cohen's d Effect Size:")
print(cohen_res)

print("Levene's Test for Homogeneity of Variance:")
leveneTest(body_mass_g ~ species, data = df)

anova_1way <- aov(body_mass_g ~ species, data = df)
print("One-Way ANOVA Results:")
summary(anova_1way)

print("Tukey's HSD Post-hoc Test:")
TukeyHSD(anova_1way)

kruskal_res <- kruskal.test(body_mass_g ~ species, data = df)
print("Kruskal-Wallis Test Results:")
print(kruskal_res)

anova_2way <- aov(body_mass_g ~ species * sex, data = df)
print("Two-Way ANOVA Results (Species and Sex):")
summary(anova_2way)

print("One-Way ANOVA for Flipper Length across Species:")
anova_flipper <- aov(flipper_length_mm ~ species, data = df)
summary(anova_flipper)

print("Tukey HSD for Flipper Length:")
TukeyHSD(anova_flipper)

ggplot(df, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Body Mass Comparison Across Species", x = "Species", y = "Body Mass (g)")

ggplot(df, aes(x = sex, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Body Mass Comparison by Sex", x = "Sex", y = "Body Mass (g)")

ggplot(df, aes(sample = body_mass_g)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~species) +
  theme_minimal() +
  labs(title = "QQ-Plots of Body Mass by Species")

ggplot(df, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Flipper Length Comparison Across Species", x = "Species", y = "Flipper Length (mm)")