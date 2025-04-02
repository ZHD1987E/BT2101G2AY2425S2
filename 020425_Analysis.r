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
# Use a general path for broader compatibility
survey_data <- read_excel(file.choose()) # Prompts user to select the Excel file manually

survey_data <- survey_data %>%
  rename(
    age = 'What is your age?',
    gender = 'What is your gender?',
    A_level_UAS = 'What is your A level UAS score? (If applicable)\r\n\r\nIf you are not sure, please click here to calculate your UAS score.',
    polytechnic_GPA = 'What is your final polytechnic GPA? (If applicable)',
    IB_score = 'What is your IB score? (If applicable)',
    NUS_status = 'Are you currently enrolled in NUS? If yes, what is your current year of study? \r\nIf no, did you graduate before 2023?',
    primary_major = 'What is your PRIMARY major?',
    current_GPA = 'What is your current GPA (Pre-S/U)?',
    Y1_GPA = 'What is your Y1 GPA (Pre-S/U)?',
    study_hours = 'On average, how many hours do you typically spend studying for exams each week for all your courses?',
    non_edu_hours = 'Per week, on average, how much time have you committed to non-educational purposes?\r\n\r\nFor example, personal recreation or gaming',
    learning_style = 'What is your preferred learning style?',
    work_ethic = 'How would you rate your work ethic and dedication to studying? \r\n(Scale from 1 to 5)',
    procrastination = 'How often do you procrastinate in completing academic tasks?\r\n',
    cognitive_ability = 'How would you rate your cognitive ability to grasp complex academic concepts?',
    self_efficacy = 'How would you rate your self-efficacy in completing academic tasks on your own without external help? \r\n',
    academic_stress = 'How often do you feel stressed or anxious due to your academic workload?',
    tech_comfort = 'How would you rate your comfort level in using new technologies for academic purposes?\r\n',
    AI_frequency = 'How would you rate your frequence of use of AI tools?',
    AI_subscription = 'Have you ever paid for an AI subscription? If so, how long have you been using it?',
    tech_proficiency = 'How would you rate your proficiency in using technology for academic purposes?',
    AI_nonacademic_use = 'How often do you use AI tools outside of academic purposes?',
    AI_reliance = 'Have you relied on AI tools to complete assignments or projects without fully understanding the material?',
    AI_improvement = 'Do you think AI has improved your ability to understand complex topics?',
    tech_comfort_proficiency = 'How would you rate your comfort and proficiency with using technology for academic purposes? \r\n',
    AI_regulations = 'From a scale of 1 to 5, how strict do you think were the regulations around AI during your time of study?'
  ) %>%
  mutate(
    AI_frequency = as.ordered(AI_frequency),
    gender = as.factor(gender),
    primary_major = as.factor(primary_major),
    AI_subscription = as.factor(AI_subscription),
    learning_style = as.factor(learning_style),
    A_level_UAS = as.numeric(A_level_UAS),
    polytechnic_GPA = as.numeric(polytechnic_GPA),
    IB_score = as.numeric(IB_score)
  )

# Remove rows 13 and 41
survey_data <- survey_data[-c(13, 41), ]

# Step 2: Standardize Pre-University Scores
survey_data <- survey_data %>%
  mutate(
    normalized_poly_gpa = polytechnic_GPA / 4.0,
    normalized_a_level_uas = A_level_UAS / 90.0,
    normalized_ib_score = IB_score / 45.0,
    pre_uni_score = coalesce(normalized_poly_gpa, normalized_a_level_uas, normalized_ib_score)
  ) %>%
  filter(!is.na(pre_uni_score))

survey_data <- survey_data %>%
  mutate(AI_frequency = as.numeric(AI_frequency))

# Step 3: Run Linear Model Without Weighting
model <- lm(
  current_GPA ~ AI_frequency + age + gender + study_hours + work_ethic + academic_stress +
    pre_uni_score + primary_major + Y1_GPA + non_edu_hours +
    procrastination + cognitive_ability + self_efficacy +
    tech_comfort + learning_style + AI_subscription + tech_proficiency +
    AI_nonacademic_use + AI_regulations,
  data = survey_data
)

summary(model)
vif(model)

plot(fitted(model), resid(model),
     main = "Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

predicted <- ggpredict(model, terms = "AI_frequency")
ggplot(predicted, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Predicted Effect of AI Usage on GPA", x = "AI Usage", y = "Predicted GPA") +
  theme_minimal()

# ------------------------------------------------------
# Attempted Propensity Score Weighting (Did Not Work Well)
# ------------------------------------------------------
# Sample Size Justification:
# - Total responses after cleaning: nrow(survey_data)
# - AI_frequency = 1: only 2 responses
# - AI_frequency = 2: 1 response
# → Too few samples in 'Low' usage levels for meaningful balancing

# Distribution check:
table(survey_data$AI_frequency)

# Attempt to group into Low, Medium, High for balance
survey_data <- survey_data %>%
  mutate(AI_group = case_when(
    AI_frequency %in% c(1, 2) ~ "Low",
    AI_frequency == 3 ~ "Medium",
    AI_frequency %in% c(4, 5) ~ "High"
  ))
survey_data$AI_group <- factor(survey_data$AI_group, levels = c("Low", "Medium", "High"))

table(survey_data$AI_group) # Only 3 rows in 'Low'

# Weighting fails because model is too confident
# → Predicted probabilities from multinom are often 1.0 or 0.0

# Example:
# treatment_probs <- predict(multinom(...), type = "probs")
# summary(rowSums(treatment_probs)) shows all 1s

# Weight distribution:
# summary(weights$weights) shows mostly 1s or near 0s
# → Indicates lack of overlap across covariates

# Conclusion:
# - Weighting approaches (both ps and cbps) not usable with current sample
# - Proceeded with simple linear regression for interpretability
# - Limitation clearly acknowledged in reporting
