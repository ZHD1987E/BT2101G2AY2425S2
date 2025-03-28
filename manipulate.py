import pandas as pd

def clean_data(df):
    # Drop column: 'Timestamp'
    df = df.drop(columns=['Timestamp'])
    # Rename column 'What is your age?' to 'age'
    df = df.rename(columns={'What is your age?': 'age'})
    # Rename column 'What is your gender? ' to 'gender'
    df = df.rename(columns={'What is your gender? ': 'gender'})
    # Rename column 'What is your A level UAS score? (If applicable)\n\nIf you are not sure, please click here to calculate your UAS score.' to 'alevel'
    df = df.rename(columns={'What is your A level UAS score? (If applicable)\n\nIf you are not sure, please click here to calculate your UAS score.': 'alevel'})
    # Rename column 'What is your final polytechnic GPA? (If applicable)' to 'polytechnic'
    df = df.rename(columns={'What is your final polytechnic GPA? (If applicable)': 'polytechnic'})
    # Rename column 'What is your IB score? (If applicable)' to 'ib'
    df = df.rename(columns={'What is your IB score? (If applicable)': 'ib'})
    # Rename column 'Are you currently enrolled in NUS? If yes, what is your current year of study? \nIf no, did you graduate before 2023?' to 'enrol'
    df = df.rename(columns={'Are you currently enrolled in NUS? If yes, what is your current year of study? \nIf no, did you graduate before 2023?': 'enrol'})
    # Rename column 'What is your PRIMARY major?' to 'major'
    df = df.rename(columns={'What is your PRIMARY major?': 'major'})
    # Rename column 'What is your current GPA (Pre-S/U)?' to 'curr_gpa'
    df = df.rename(columns={'What is your current GPA (Pre-S/U)?': 'curr_gpa'})
    # Rename column 'What is your Y1 GPA (Pre-S/U)?' to 'y1_gpa'
    df = df.rename(columns={'What is your Y1 GPA (Pre-S/U)?': 'y1_gpa'})
    # Rename column 'On average, how many hours do you typically spend studying for exams each week for all your courses?' to 'time_exam'
    df = df.rename(columns={'On average, how many hours do you typically spend studying for exams each week for all your courses?': 'time_exam'})
    # Rename column 'Per week, on average, how much time have you committed to non-educational purposes?\n\nFor example, personal recreation or gaming' to 'time_non_educ'
    df = df.rename(columns={'Per week, on average, how much time have you committed to non-educational purposes?\n\nFor example, personal recreation or gaming': 'time_non_educ'})
    # Rename column 'What is your preferred learning style?' to 'learn_style'
    df = df.rename(columns={'What is your preferred learning style?': 'learn_style'})
    # Rename column 'How would you rate your work ethic and dedication to studying? \n(Scale from 1 to 5) ' to 'ethic_rate'
    df = df.rename(columns={'How would you rate your work ethic and dedication to studying? \n(Scale from 1 to 5) ': 'ethic_rate'})
    # Rename column 'How often do you procrastinate in completing academic tasks?\n' to 'procastinate_rate'
    df = df.rename(columns={'How often do you procrastinate in completing academic tasks?\n': 'procastinate_rate'})
    # Rename column 'How would you rate your cognitive ability to grasp complex academic concepts?' to 'ability_rate'
    df = df.rename(columns={'How would you rate your cognitive ability to grasp complex academic concepts?': 'ability_rate'})
    # Rename column 'How would you rate your self-efficacy in completing academic tasks on your own without external help? \n' to 'effaciacy_rate'
    df = df.rename(columns={'How would you rate your self-efficacy in completing academic tasks on your own without external help? \n': 'effaciacy_rate'})
    # Rename column 'How often do you feel stressed or anxious due to your academic workload?' to 'anxiety_rate'
    df = df.rename(columns={'How often do you feel stressed or anxious due to your academic workload?': 'anxiety_rate'})
    # Rename column 'How would you rate your comfort level in using new technologies for academic purposes?\n' to 'comfort_rate'
    df = df.rename(columns={'How would you rate your comfort level in using new technologies for academic purposes?\n': 'comfort_rate'})
    # Rename column 'How would you rate your frequence of use of AI tools?' to 'ai_use_freq_rate'
    df = df.rename(columns={'How would you rate your frequence of use of AI tools?': 'ai_use_freq_rate'})
    # Rename column 'Have you ever paid for an AI subscription? If so, how long have you been using it?' to 'aisub_time'
    df = df.rename(columns={'Have you ever paid for an AI subscription? If so, how long have you been using it?': 'aisub_time'})
    # Rename column 'How would you rate your proficiency in using technology for academic purposes? ' to 'techprof_rate'
    df = df.rename(columns={'How would you rate your proficiency in using technology for academic purposes? ': 'tech_prof_rate'})
    # Rename column 'How often do you use AI tools outside of academic purposes? ' to 'ainoneduc_rate'
    df = df.rename(columns={'How often do you use AI tools outside of academic purposes? ': 'ainoneduc_rate'})
    # Rename column 'Have you relied on AI tools to complete assignments or projects without fully understanding the material? ' to 'ai_use_nounderstand'
    df = df.rename(columns={'Have you relied on AI tools to complete assignments or projects without fully understanding the material? ': 'ai_use_nounderstand'})
    # Rename column 'Do you think AI has improved your ability to understand complex topics? ' to 'ai_impact_rate'
    df = df.rename(columns={'Do you think AI has improved your ability to understand complex topics? ': 'ai_impact_rate'})
    # Rename column 'How would you rate your comfort and proficiency with using technology for academic purposes? \n' to 'tech_prof_rate'
    df = df.rename(columns={'How would you rate your comfort and proficiency with using technology for academic purposes? \n': 'tech_comfort_rate'})
    # Rename column 'From a scale of 1 to 5, how strict do you think were the regulations around AI during your time of study?' to 'ai_strict_rate'
    df = df.rename(columns={'From a scale of 1 to 5, how strict do you think were the regulations around AI during your time of study?': 'ai_strict_rate'})
    return df

# Loaded variable 'df' from URI: c:\Users\UNIVERSITY USE\Desktop\BT2101G2AY2425S2\BT2101 Survey  (Responses).xlsx
df = pd.read_excel("BT2101G2AY2425S2/BT2101_Survey_(Responses).xlsx")

df_clean = clean_data(df.copy())
df_clean.head()
df_clean.to_csv('survey.csv')