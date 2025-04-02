# Load necessary libraries
# Define the list of packages again
packages <- c(
  "readxl", "dplyr", "tidyverse", "nnet", "MatchIt", "cobalt",
  "WeightIt", "car", "ggeffects", "sensemakr", "ggplot2"
)

# Install any packages that aren't already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

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

# Step 1: Load and Clean Data (Assuming the survey data has ALREADY been loaded)

survey_data <- latestSurvey %>%
  rename(
    age = names(latestSurvey)[2],
    gender = names(latestSurvey)[3],
    A_level_UAS = names(latestSurvey)[4],
    polytechnic_GPA = names(latestSurvey)[5],
    IB_score = names(latestSurvey)[6],
    NUS_status = names(latestSurvey)[7],
    primary_major = names(latestSurvey)[8],
    current_GPA = names(latestSurvey)[9],
    Y1_GPA = names(latestSurvey)[10],
    study_hours = names(latestSurvey)[11],
    non_edu_hours = names(latestSurvey)[12],
    learning_style = names(latestSurvey)[13],
    work_ethic = names(latestSurvey)[14],
    procrastination = names(latestSurvey)[15],
    cognitive_ability = names(latestSurvey)[16],
    self_efficacy = names(latestSurvey)[17],
    academic_stress = names(latestSurvey)[18],
    tech_comfort = names(latestSurvey)[19],
    AI_frequency = names(latestSurvey)[20],
    AI_subscription = names(latestSurvey)[21],
    tech_proficiency = names(latestSurvey)[22],
    AI_nonacademic_use = names(latestSurvey)[23],
    AI_reliance = names(latestSurvey)[24],
    AI_improvement = names(latestSurvey)[25],
    tech_comfort_proficiency = names(latestSurvey)[26],
    AI_regulations = names(latestSurvey)[27]
  ) %>%
  mutate(
    gender = as.factor(gender),
    primary_major = as.factor(primary_major),
    learning_style = as.factor(learning_style),
    AI_subscription = as.factor(AI_subscription)
  )

# (all error rows are removed before importing dataset)


# Rename row 23 NUSHigh CAP
survey_data[23, "IB_score"] <- "39.6"

# Step 2: Standardize Pre-University Scores
survey_data <- survey_data %>%
  mutate(
    polytechnic_GPA = as.numeric(gsub("[^0-9.]", "", polytechnic_GPA)),
    A_level_UAS = as.numeric(gsub("[^0-9.]", "", A_level_UAS)),
    IB_score = as.numeric(gsub("[^0-9.]", "", IB_score)),
    
    normalized_poly_gpa = ifelse(!is.na(polytechnic_GPA), polytechnic_GPA / 4.0, NA),
    normalized_a_level_uas = ifelse(!is.na(A_level_UAS), A_level_UAS / 90.0, NA),
    normalized_ib_score = ifelse(!is.na(IB_score), IB_score / 45.0, NA),
    
    pre_uni_score = coalesce(normalized_poly_gpa, normalized_a_level_uas, normalized_ib_score)
  )

# Substitute empty values in pre-uni column with mean score
survey_data$pre_uni_score[is.na(survey_data$pre_uni_score)] <- 
  mean(survey_data$pre_uni_score, na.rm = TRUE)



# Step 3: Generalized Propensity Score Estimation
confounders <- c(
  "age", "gender", "pre_uni_score", "primary_major", "study_hours", "non_edu_hours",
  "work_ethic", "procrastination", "cognitive_ability", "self_efficacy",
  "academic_stress", "tech_comfort", "AI_subscription", "tech_proficiency",
  "AI_nonacademic_use", "AI_regulations", "learning_style"
)



# Checking weights validity taking into account the estimated probability
# of receiving each treatment level
ps_model <- multinom(AI_frequency ~ ., data = survey_data[, c("AI_frequency", confounders)])
treatment_probs <- predict(ps_model, type = "probs")
survey_data2 <- cbind(survey_data, treatment_probs)

weights <- weightit(
  formula = AI_frequency ~ .,
  data = survey_data2[, c("AI_frequency", confounders)],
  method = "ps",
  estimand = "ATE",
  stabilize = TRUE
)

survey_data$weights <- weights$weights


#### Above works fine, below doesn't work because of the weights issue (too few responses) ####

# Step 4: Covariate Balance Check

# balance_stats <- bal.tab(
#   formula = AI_frequency ~ .,
#   data = survey_data[, c("AI_frequency", confounders)],
#   weights = survey_data$weights,
#   method = "weighting",
#   stats = c("m", "ks"),
#   quick = FALSE
# )
# love.plot(balance_stats, threshold = 0.1, abs = TRUE)

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

predicted <- ggpredict(weighted_model, terms="AI_frequency")
ggplot(predicted, aes(x=x, y=predicted)) +
  geom_line(size=1.2, color="blue") +
  geom_point(size=3, color="red") +
  labs(title="Predicted Effect of AI Usage on GPA",
       x="AI Frequency",
       y="Predicted GPA") +
  theme_minimal()