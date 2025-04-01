library(readxl)
library(dplyr)
library(tidyverse)
library(MatchIt)
library(cobalt)
library(WeightIt)
library(ggeffects)


survey <- read_excel("Downloads/survey.xlsx")
View(survey)

survey_data <- survey[, -1] #remove first column on time

# Data cleaning and variable renaming
survey_data <- survey_data %>%
  rename( # Rename all variables to remove spaces/special characters
    age = `What is your age?`,
    gender = `What is your gender?`,
    A_level_UAS = `What is your A level UAS score? (If applicable)\r\n\r\nIf you are not sure, please click here to calculate your UAS score.`,
    polytechnic_GPA = `What is your final polytechnic GPA? (If applicable)`,
    IB_score = `What is your IB score? (If applicable)`,
    NUS_status = `Are you currently enrolled in NUS? If yes, what is your current year of study? \r\nIf no, did you graduate before 2023?`,
    primary_major = `What is your PRIMARY major?`,
    current_GPA = `What is your current GPA (Pre-S/U)?`,
    Y1_GPA = `What is your Y1 GPA (Pre-S/U)?`,
    study_hours = `On average, how many hours do you typically spend studying for exams each week for all your courses?`,
    non_edu_hours = `Per week, on average, how much time have you committed to non-educational purposes?\r\n\r\nFor example, personal recreation or gaming`,
    learning_style = `What is your preferred learning style?`,
    work_ethic = `How would you rate your work ethic and dedication to studying? \r\n(Scale from 1 to 5)`,
    procrastination = `How often do you procrastinate in completing academic tasks?\r\n`,
    cognitive_ability = `How would you rate your cognitive ability to grasp complex academic concepts?`,
    self_efficacy = `How would you rate your self-efficacy in completing academic tasks on your own without external help? \r\n`,
    academic_stress = `How often do you feel stressed or anxious due to your academic workload?`,
    tech_comfort = `How would you rate your comfort level in using new technologies for academic purposes?\r\n`,
    AI_frequency = `How would you rate your frequence of use of AI tools?`,
    AI_subscription = `Have you ever paid for an AI subscription? If so, how long have you been using it?`,
    tech_proficiency = `How would you rate your proficiency in using technology for academic purposes?`,
    AI_nonacademic_use = `How often do you use AI tools outside of academic purposes?`,
    AI_reliance = `Have you relied on AI tools to complete assignments or projects without fully understanding the material?`,
    AI_improvement = `Do you think AI has improved your ability to understand complex topics?`,
    tech_comfort_proficiency = `How would you rate your comfort and proficiency with using technology for academic purposes? \r\n`,
    AI_regulations = `From a scale of 1 to 5, how strict do you think were the regulations around AI during your time of study?`
  ) %>%
  # Convert to appropriate data types
  mutate(
    # Create binary treatment variable (high vs low AI use)
    AI_frequency = as.ordered(AI_frequency),
    gender = as.factor(gender),  # Convert categorical variables to factors
    primary_major = as.factor(primary_major),
    AI_subscription = as.factor(AI_subscription),
    learning_style = as.factor(learning_style)
  )

confounders <- c(
  "age", "gender", "A_level_UAS", "polytechnic_GPA", "IB_score",
  "primary_major", "study_hours", "non_edu_hours",
  "work_ethic", "procrastination", "cognitive_ability",
  "self_efficacy", "academic_stress", "tech_comfort", "AI_subscription",
  "tech_proficiency", "AI_nonacademic_use", "tech_comfort_proficiency",
  "AI_regulations", "learning_style"
)


#replace NA values with 0
survey_data$A_level_UAS[is.na(survey_data$A_level_UAS)] <- 0
survey_data$polytechnic_GPA[is.na(survey_data$polytechnic_GPA)] <- 0
survey_data$IB_score[is.na(survey_data$IB_score)] <- 0

# Generalized Propensity Score Estimation
# Use multi-nomial logistic regression for ordinal treatment
library(nnet)

#Step 1: Estimate Generalized Propensity Scores (GPS)

ps_model <- multinom(
  AI_frequency ~ .,  # All confounders
  data = survey_data[, c("AI_frequency", confounders)]
)

# Predict probabilities for each AI usage level (1-5)
treatment_probs <- predict(ps_model, type = "probs")
survey_data <- cbind(survey_data, treatment_probs) # This just creates rows of 1 to 5!


# Step 2: Creating weights based on all cofounders and the predicted scores

weights <- weightit(
  AI_frequency ~ .,  # All confounders (This includes the generically named '1', '2' ... '5' rows!!!)
  data = survey_data[, c("AI_frequency", confounders)],
  method = "ps",  # Propensity score weighting
  estimand = "ATE",  # Average Treatment Effect
  stabilize = TRUE # Stabilized weights
)

survey_data$weights <- weights$weights

# Step 3: Check Covariate Balance

# For multi-level treatments, use cobalt's bal.tab()
balance_stats <- bal.tab(
  AI_frequency ~ .,
  data = survey_data[, c("AI_frequency", confounders)],
  weights = weights$weights,
  method = "weighting",
  stats = c("m", "ks"),
  quick = FALSE 
)

# Plot Balance Check
love.plot(
  balance_stats,
  threshold = 0.1,  # Balance threshold
  abs = TRUE,  # Absolute values
  stars = "raw"  # Label raw mean differences
)


# Step 4: Estimate Treatment Effect of AI Usage on GPA

# Weighted regression model to estimate AI effect on current_GPA
weighted_model <- lm(
  current_GPA ~ AI_frequency + age + gender + study_hours + work_ethic + academic_stress
    + A_level_UAS + IB_score + polytechnic_GPA + primary_major + Y1_GPA + non_edu_hours  
    + procrastination + cognitive_ability + self_efficacy + tech_comfort + learning_style
    + AI_subscription + tech_proficiency + AI_nonacademic_use + AI_regulations,
  data = survey_data,
  weights = survey_data$weights
)
# To get the coefficients
summary(weighted_model)

# To visualize the weighted_model
predicted <- ggpredict(weighted_model, terms = "AI_frequency")

ggplot(predicted, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Predicted Effect of AI Usage on GPA",
       x = "AI Frequency",
       y = "Predicted GPA") +
  theme_minimal()


#Detecting multicollinearity
library(car)
vif(weighted_model)

#linear model as a test of linearity
lm_model <- lm(current_GPA ~ AI_frequency + age + study_hours + work_ethic + academic_stress, data = survey_data)
summary(lm_model)
plot(lm_model$residuals)

#residual plot to check for homoscedasticity
residuals <- resid(weighted_model)
fitted_values <- fitted(weighted_model)

plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red") 



# Step 5: Sensitivity Analysis & Robustness Checks
ggplot(data = survey_data, aes(x = AI_frequency, y = weights)) +
  geom_boxplot() +
  labs(title = "Distribution of Weights Across AI Usage Levels")

# Visualize Treatment Probabilities
library(tidyr)
survey_data_long <- survey_data %>%
  pivot_longer(
    cols = c(`2`, `3`, `4`, `5`),
    names_to = "treatment_level",
    values_to = "probability"
  )

# Density plot for the probability of treatment
ggplot(survey_data_long, aes(x = probability, fill = treatment_level)) +
  geom_density(alpha = 0.3, adjust = 2) + 
  coord_cartesian(ylim = c(0, 5)) +  # Adjust Y-axis limits
  labs(title = "Generalized Propensity Score Distribution",
       x = "Probability of Treatment Level",
       y = "Density",
       fill = "AI Frequency") +
  theme_minimal()


