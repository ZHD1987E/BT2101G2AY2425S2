library(dplyr)

 # include variables that might influence both grades (outcome) & ai usage (treatment)
survey_data <- survey %>%
  rename(
    alevel = names(survey)[4],
    polytechnic = names(survey)[5],
    ib = names(survey)[6],
    y1_gpa = names(survey)[10],
    curr_gpa = names(survey)[9], # outcome
    major = names(survey)[8],
    gender = names(survey)[3],
    age = names(survey)[2],
    ai_use = names(survey)[20], # treatment
    work_ethic = names(survey)[14],
    procrastinate = names(survey)[15],
    cognitive = names(survey)[16],
    workload_stress = names(survey)[18],
    hours_studying = names(survey)[11],
    comfort_using_ai = names(survey)[19],
    tech_proficiency = names(survey)[22]
  ) %>%
  select(alevel, polytechnic, ib, y1_gpa, curr_gpa, major, gender, age, ai_use, work_ethic, procrastinate, cognitive,
         workload_stress, hours_studying, comfort_using_ai, tech_proficiency) 


# filter out entries with missing data in crucial rows 

survey_data <- survey_data %>%
  filter(!is.na(ai_use) | !is.na(curr_gpa))

survey_data <- survey_data %>%
  mutate(
    alevel = as.numeric(as.character(alevel)),
    polytechnic = as.numeric(as.character(polytechnic)),
    ib = as.numeric(as.character(ib))
  )



# estimate gpsm 
gps_model <- glm(ai_use ~ y1_gpa + major + gender + age + work_ethic + procrastinate + 
                   cognitive + workload_stress + hours_studying + comfort_using_ai + tech_proficiency,
                      data = survey_data, family = gaussian())


# get ps
survey_data$gps <- predict(gps_model, newdata = survey_data, type = "response", na.action = na.pass)

