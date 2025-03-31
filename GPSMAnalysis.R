# Load required libraries
library(tidyverse)
library(readr)


# Load your dataset
df <- read.csv("Final_Extended_GPSM_Dataset.csv")

# Clean up spaces and convert to factor
df$paid_ai <- as.factor(trimws(df$paid_ai))


# Recode 'study_hours' into approximate numeric values
df$study_hours_num <- recode(df$study_hours,
                             "Less than 1 hour" = 0.5,
                             "1-5 hours" = 3,
                             "6-10 hours" = 8,
                             "11-15 hours" = 13,
                             "16-20 hours" = 18,
                             "21+ hours" = 24)

# Handle missing gender (optional assumption: majority = 1)
df$gender[is.na(df$gender)] <- 1

# Convert pre_u_score to numeric safely
df$pre_u_score <- parse_number(df$pre_u_score)

#convert into factors
df$learning_style <- as.factor(df$learning_style)
df$paid_ai <- as.factor(df$paid_ai)
df$major <- as.factor(df$major)
df$year_of_study <- as.factor(df$year_of_study)
df$pre_u_type <- as.factor(df$pre_u_type)

# Fit GPS model (Generalized Propensity Score model)
gps_model <- lm(current_gpa ~ age + gender + study_hours_num +
                  tech_proficiency + stress_level + self_efficacy +
                  ai_regulations + pre_u_score + y1_gpa,
                data = df)

gps_model <- lm(current_gpa ~ 
                  ai_usage_freq + age + gender + study_hours_num + 
                  tech_proficiency + stress_level + self_efficacy + 
                  ai_regulations + pre_u_score + work_ethic + procrastination +
                  cognitive_ability + comfort_with_tech + ai_usage_nonacademic +
                  ai_improved_understanding + used_ai_without_understanding +
                  learning_style + paid_ai + major + year_of_study +
                  pre_u_type + non_academic_hours + y1_gpa,
                data = df)



summary(gps_model)

# Predict GPS and assign to full dataset safely
df$gps <- NA
valid_rows <- as.numeric(rownames(model.frame(gps_model)))
df$gps[valid_rows] <- predict(gps_model)

# Split GPS into 5 strata (quintiles)
df$gps_strata <- ntile(df$gps, 5)

# Summary table: average GPA by AI usage per stratum
result <- df %>%
  group_by(gps_strata, ai_usage_freq) %>%
  summarise(mean_gpa = mean(current_gpa, na.rm = TRUE),
            count = n(), .groups = 'drop')

print(result)

#Visualize
ggplot(result, aes(x = factor(ai_usage_freq), y = mean_gpa, fill = factor(gps_strata))) +
  geom_col(position = "dodge") +
  labs(title = "GPA vs AI Usage Frequency (GPS-Stratified)",
       x = "AI Usage Frequency",
       y = "Mean GPA",
       fill = "GPS Stratum") +
  theme_minimal()