import pandas as pd

def clean_data(df):
    # Drop column if exists
    if 'Timestamp' in df.columns:
        df = df.drop(columns=['Timestamp'])

    # Rename columns
    df = df.rename(columns={
        'What is your age? ': 'age',
        'What is your gender? ': 'gender',
        'What is your A level UAS score? (If applicable)\n\nIf you are not sure, please click here to calculate your UAS score.': 'alevel',
        'What is your final polytechnic GPA? (If applicable)': 'polytechnic',
        'What is your IB score? (If applicable)': 'ib',
        'Are you currently enrolled in NUS? If yes, what is your current year of study? \nIf no, did you graduate before 2023?': 'enrol',
        'What is your PRIMARY major?': 'major',
        'What is your current GPA (Pre-S/U)?': 'curr_gpa',
        'What is your Y1 GPA (Pre-S/U)?': 'y1_gpa',
        'On average, how many hours do you typically spend studying for exams each week for all your courses?': 'time_exam',
        'Per week, on average, how much time have you committed to non-educational purposes?\n\nFor example, personal recreation or gaming': 'time_non_educ',
        'What is your preferred learning style?': 'learn_style',
        'How would you rate your work ethic and dedication to studying? \n(Scale from 1 to 5) ': 'ethic_rate',
        'How often do you procrastinate in completing academic tasks?\n': 'procrastinate_rate',
        'How would you rate your cognitive ability to grasp complex academic concepts?': 'ability_rate',
        'How would you rate your self-efficacy in completing academic tasks on your own without external help? \n': 'efficacy_rate',
        'How often do you feel stressed or anxious due to your academic workload?': 'anxiety_rate',
        'How would you rate your comfort level in using new technologies for academic purposes?\n': 'comfort_rate',
        'How would you rate your frequence of use of AI tools?': 'ai_use_freq_rate',
        'Have you ever paid for an AI subscription? If so, how long have you been using it?': 'aisub_time',
        'How would you rate your proficiency in using technology for academic purposes? ': 'techprof_rate',
        'How often do you use AI tools outside of academic purposes? ': 'ainoneduc_rate',
        'Have you relied on AI tools to complete assignments or projects without fully understanding the material? ': 'ai_use_nounderstand',
        'Do you think AI has improved your ability to understand complex topics? ': 'ai_impact_rate',
        'How would you rate your comfort and proficiency with using technology for academic purposes? \n': 'tech_comfort_rate',
        'From a scale of 1 to 5, how strict do you think were the regulations around AI during your time of study?': 'ai_strict_rate'
    })

    # Convert gender to numeric: male = 1, female = 0
    df['gender'] = df['gender'].str.strip().str.lower().map({'male': 1, 'female': 0})

    # Combine pre-university scores into a single column
    def get_pre_u(row):
        if pd.notna(row['alevel']):
            return pd.Series([row['alevel'], 'A-level'])
        elif pd.notna(row['ib']):
            return pd.Series([row['ib'], 'IB'])
        elif pd.notna(row['polytechnic']):
            return pd.Series([row['polytechnic'], 'Poly'])
        else:
            return pd.Series([None, None])

    df[['pre_u_score', 'pre_u_type']] = df.apply(get_pre_u, axis=1)

    return df

df = pd.read_excel("BT2101G2AY2425S2/BT2101_Survey_(Responses).xlsx")
df_clean = clean_data(df)
df_clean.head()
df_clean.to_csv('survey.csv')