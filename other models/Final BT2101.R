# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(nnet) # For multinomial logistic regression
library(MatchIt)
library(cobalt) # For balance diagnostics
library(WeightIt) # For propensity score weighting
library(car)
library(ggeffects) # For non-linearity check
library(sensemakr) # For sensitivity analysis
library(ggplot2) # For visualization

# Step 1: Load and Clean Data
survey <- read_excel("BT2101_Survey_-Responses.xlsx")
survey_data <- survey %>%
  rename(
    age = `What is your age?`,
    gender = `What is your gender?`,
    A_level_UAS = `What is your A level UAS score?`,
    polytechnic_GPA = `What is your final polytechnic GPA?`,
    IB_score = `What is your IB score?`,
    NUS_status = `Are you currently enrolled in NUS? If yes, what is your current year of study?`,
    primary_major = `What is your PRIMARY major?`,
    current_GPA = `What is your current GPA (Pre-S/U)?`,
    Y1_GPA = `What is your Y1 GPA (Pre-S/U)?`,
    study_hours = `On average, how many hours do you typically spend studying for exams each week for all your courses?`,
    non_edu_hours = `Per week, on average, how much time have you committed to non-educational purposes?`,
    learning_style = `What is your preferred learning style?`,
    work_ethic = `How would you rate your work ethic and dedication to studying?`,
    procrastination = `How often do you procrastinate in completing academic tasks?`,
    cognitive_ability = `How would you rate your cognitive ability to grasp complex academic concepts?`,
    self_efficacy = `How would you rate your self-efficacy in completing academic tasks on your own without external help?`,
    academic_stress = `How often do you feel stressed or anxious due to your academic workload?`,
    tech_comfort = `How would you rate your comfort level in using new technologies for academic purposes?`,
    AI_frequency = `How would you rate your frequence of use of AI tools?`,
    AI_subscription = `Have you ever paid for an AI subscription? If so, how long have you been using it?`,
    tech_proficiency = `How would you rate your proficiency in using technology for academic purposes?`,
    AI_nonacademic_use = `How often do you use AI tools outside of academic purposes?`,
    AI_reliance = `Have you relied on AI tools to complete assignments or projects without fully understanding the material?`,
    AI_improvement = `Do you think AI has improved your ability to understand complex topics?`,
    tech_comfort_proficiency = `How would you rate your comfort and proficiency with using technology for academic purposes?`,
    AI_regulations = `From a scale of 1 to 5, how strict do you think were the regulations around AI during your time of study?`
  ) %>%
  mutate(
    AI_frequency = as.ordered(AI_frequency),
    gender = as.factor(gender),
    primary_major = as.factor(primary_major),
    learning_style = as.factor(learning_style),
    AI_subscription = as.factor(AI_subscription)
  )

# Remove rows 13 and 41
survey_data <- survey_data[-c(13, 41), ]

# Step 2: Standardize Pre-University Scores
survey_data <- survey_data %>%
  mutate(
    # Normalize each score type to a scale of 0 to 1
    normalized_poly_gpa = ifelse(!is.na(polytechnic_GPA), polytechnic_GPA / 4.0, NA),
    normalized_a_level_uas = ifelse(!is.na(A_level_UAS), A_level_UAS / 90.0, NA),
    normalized_ib_score = ifelse(!is.na(IB_score), IB_score / 45.0, NA),
    
    # Combine into a single pre-university score (take the non-NA value)
    pre_uni_score = coalesce(normalized_poly_gpa, normalized_a_level_uas, normalized_ib_score)
  )

# Step 3: Generalized Propensity Score Estimation
confounders <- c(
  "age", "gender", "pre_uni_score", "primary_major", "study_hours", "non_edu_hours",
  "work_ethic", "procrastination", "cognitive_ability", "self_efficacy",
  "academic_stress", "tech_comfort", "AI_subscription", "tech_proficiency",
  "AI_nonacademic_use", "AI_regulations", "learning_style"
)

ps_model <- multinom(AI_frequency ~ ., data = survey_data[, c("AI_frequency", confounders)])
treatment_probs <- predict(ps_model, type = "probs")
survey_data <- cbind(survey_data, treatment_probs)

weights <- weightit(
  formula = AI_frequency ~ .,
  data = survey_data[, c("AI_frequency", confounders)],
  method = "ps",
  estimand = "ATE",
  stabilize = TRUE
)

survey_data$weights <- weights$weights

# Step 4: Covariate Balance Check
balance_stats <- bal.tab(
  formula = AI_frequency ~ .,
  data = survey_data[, c("AI_frequency", confounders)],
  weights = survey_data$weights,
  method = "weighting",
  stats = c("m", "ks"),
  quick = FALSE
)

love.plot(balance_stats, threshold = 0.1, abs = TRUE)

# Step 5: Weighted Regression Model (Causal Effect Estimation)
weighted_model <- lm(
  current_GPA ~ AI_frequency + age + gender + study_hours + work_ethic + academic_stress +
    pre_uni_score + primary_major + Y1_GPA + non_edu_hours +
    procrastination + cognitive_ability + self_efficacy +
    tech_comfort + learning_style + AI_subscription + tech_proficiency +
    AI_nonacademic_use + AI_regulations,
  data = survey_data,
  weights = survey_data$weights
)

summary(weighted_model)

# Step 6: Diagnostics and Sensitivity Analysis
vif(weighted_model) # Multicollinearity check

residuals <- resid(weighted_model)
fitted_values <- fitted(weighted_model)

plot(fitted_values, residuals,
     main="Residuals vs Fitted Values",
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0, col="red")

ggplot(data=survey_data, aes(x=AI_frequency, y=weights)) +
  geom_boxplot() +
  labs(title="Distribution of Weights Across AI Usage Levels")

#test for non-linearity of AI Effect on GPA
predicted <- ggpredict(weighted_model, terms="AI_frequency")
ggplot(predicted, aes(x=x, y=predicted)) +
  geom_line(size=1.2, color="blue") +
  geom_point(size=3, color="red") +
  labs(title="Predicted Effect of AI Usage on GPA",
       x="AI Frequency",
       y="Predicted GPA") +
  theme_minimal()
