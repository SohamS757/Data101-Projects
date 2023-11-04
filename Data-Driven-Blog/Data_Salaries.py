# Import necessary packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from ZTest import permutation_test

# Display information about the dataset
print("""
Data jobs salaries - weekly updated
Usability: 10.00

Columns Used:  
-----------------------------------------
  work_year
  experience_level: 
    EN: Entry-level / Junior
    MI: Mid-level / Intermediate
    SE: Senior-level / Expert
    EX: Executive-level / Director
    
  employment_type:
    PT: Part-time
    FT: Full-time
    CT: Contract
    FL: Freelance
    
  salary_in_usd:
  
  remote_ratio:
    0: No remote work (less than 20%)
    50: Partially remote/hybrid
    100: Fully remote (more than 80%)
    
  company_size:
    S: less than 50 employees (small)
    M: 50 to 250 employees (medium)
    L: more than 250 employees (large)  
---------------------------------------
""")

# Importing Data from GitHub/Local
data = pd.read_csv("https://raw.githubusercontent.com/SohamS757/Data101-Projects/main/Data-Driven-Blog/salaries.csv")
print(data.head())
print(data.columns)

# Display unique job titles
unique_job_titles = data['job_title'].unique()
print(unique_job_titles)
print(len(unique_job_titles))  # Unusable due to Length 120

# Removing unnecessary (not usable now) columns
data = data.drop(columns=["employee_residence", "company_location"])
print(data.head())
# COLUMNS: work_year, experience_level, employment_type, salary_in_usd, remote_ratio, company_size

unique_work_years = data['work_year'].unique()
print(unique_work_years)  # 2023 2020 2022 2021

# Checking Ratios for a better hypothesis
# The Top 2 Are Listed Here: (Total trials: 34 needed to get the best 2)
# OUTRAGEOUS BUT INVALID Combinations: EX-CT Combination and FL-L
mean_salary = data['salary_in_usd'].mean()
print(mean_salary)

mean_salary_invalid_1 = data[(data['employment_type'] == "CT") & (data['experience_level'] == "EX")]['salary_in_usd'].mean()
print(mean_salary_invalid_1)
print(len(data[(data['employment_type'] == "CT") & (data['experience_level'] == "EX")]))  # Invalid: Rows: ONLY 1!!

mean_salary_invalid_2 = data[(data['company_size'] == "L") & (data['employment_type'] == "FL")]['salary_in_usd'].mean()
print(mean_salary_invalid_2)
print(len(data[(data['company_size'] == "L") & (data['employment_type'] == "FL")]))  # Invalid Rows: ONLY 1!!

############### Valid Combinations - 

# COMBINATION 1 
mean_salary_valid_1 = data[(data['remote_ratio'] == 100) & (data['experience_level'] == "EX")]['salary_in_usd'].mean()
print(mean_salary_valid_1)
print(len(data[(data['remote_ratio'] == 100) & (data['experience_level'] == "EX")]))  # 209 Rows Somewhat Valid

ex_data = data[data['experience_level'] == "EX"]

data_frame = pd.DataFrame({
    'Category': ["Fully Remote", "Semi-Remote", "No-Remote"],
    'Value': [
        np.mean(ex_data[ex_data['remote_ratio'] == 100]['salary_in_usd']),
        np.mean(ex_data[ex_data['remote_ratio'] == 50]['salary_in_usd']),
        np.mean(ex_data[ex_data['remote_ratio'] == 0]['salary_in_usd'])
    ]
})

# Create a bar plot
plt.bar(data_frame['Category'], data_frame['Value'], color=['blue', 'red', 'yellow'])
plt.title("Salary Distribution for Executive level")
plt.xlabel("Executive Level")
plt.ylabel("Average Salary")
plt.show()

# COMBINATION 2

mean_salary_valid_2 = data[(data['remote_ratio'] == 0) & (data['employment_type'] == "FT")]['salary_in_usd'].mean()
print(mean_salary_valid_2)
print(len(data[(data['remote_ratio'] == 0) & (data['employment_type'] == "FT")]))  # 4882??? Rows Valid

ft_data = data[data['employment_type'] == "FT"]

permutation_test_result_1 = permutation_test(ft_data, 'remote_ratio', 'salary_in_usd', 10000, '50', '100')
print("P-Value (permutation_test_result_1):", permutation_test_result_1)

permutation_test_result_2 = permutation_test(ft_data, 'remote_ratio', 'salary_in_usd', 10000, '50', '0')
print("P-Value (permutation_test_result_2):", permutation_test_result_2)

remote_count = len(np.unique(ft_data))
print(remote_count)

# Remove unnecessary columns
data = data.drop(columns=["employee_residence", "company_location"])

# Valid Combinations
ex_ct_data = data[(data["experience_level"] == "EX") & (data["employment_type"] == "CT")]
fl_l_data = data[(data["company_size"] == "L") & (data["employment_type"] == "FL")]

# Combination 1
ex_100_data = data[(data["experience_level"] == "EX") & (data["remote_ratio"] == 100)]
ex_50_data = data[(data["experience_level"] == "EX") & (data["remote_ratio"] == 50)]
ex_0_data = data[(data["experience_level"] == "EX") & (data["remote_ratio"] == 0)]

data_frame_1 = pd.DataFrame({
    "Category": ["Fully Remote", "Semi-Remote", "No-Remote"],
    "Value": [
        ex_100_data["salary_in_usd"].mean(),
        ex_50_data["salary_in_usd"].mean(),
        ex_0_data["salary_in_usd"].mean()
    ]
})

# Barplot for Combination 1
plt.bar(data_frame_1["Category"], data_frame_1["Value"], color=["blue", "red", "yellow"])
plt.title("Salary Distribution for Executive level")
plt.xlabel("Executive Level")
plt.ylabel("Average Salary")
plt.show()

# Combination 2
ft_0_data = data[(data["employment_type"] == "FT") & (data["remote_ratio"] == 0)]
ft_50_data = data[(data["employment_type"] == "FT") & (data["remote_ratio"] == 50)]
ft_100_data = data[(data["employment_type"] == "FT") & (data["remote_ratio"] == 100)]

data_frame_2 = pd.DataFrame({
    "Category": ["Fully Remote", "Semi-Remote", "No-Remote"],
    "Value": [
        ft_100_data["salary_in_usd"].mean(),
        ft_50_data["salary_in_usd"].mean(),
        ft_0_data["salary_in_usd"].mean()
    ]
})

# Barplot for Combination 2
plt.bar(data_frame_2["Category"], data_frame_2["Value"], color=["blue", "red", "yellow"])
plt.title("Salary Distribution for Full-time")
plt.xlabel("Full Time")
plt.ylabel("Average Salary")
plt.show()

# Comparing Combination 2 with others using Mann-Whitney U test
print("Comparing SECOND finding with others using Mann-Whitney U test")
ft_0_salary = ft_0_data["salary_in_usd"]
ft_50_salary = ft_50_data["salary_in_usd"]
ft_100_salary = ft_100_data["salary_in_usd"]

_, p_value_50_100 = mannwhitneyu(ft_50_salary, ft_100_salary, alternative="two-sided")
_, p_value_50_0 = mannwhitneyu(ft_50_salary, ft_0_salary, alternative="two-sided")

print("Mann-Whitney U test p-value (50 vs. 100):", p_value_50_100)
print("Mann-Whitney U test p-value (50 vs. 0):", p_value_50_0)

# Bayesian Odds Task
print("Baysian Odds Task")
observed = ex_100_data["salary_in_usd"]
belief = ex_100_data["remote_ratio"]

contingency_table = pd.crosstab(belief, observed)

prior_prob = contingency_table.iloc[1, 2] / contingency_table.iloc[:, 2].sum()
prior_odds = prior_prob / (1 - prior_prob)
true_positive = contingency_table.iloc[1, 2] / contingency_table.iloc[:, 2].sum()
false_positive = contingency_table.iloc[0, 2] / contingency_table.iloc[:, 2].sum()
likelihood_ratio = true_positive / false_positive
posterior_odds = likelihood_ratio * prior_odds

print("Contingency Table:")
print(contingency_table)
print("Prior Probability:", prior_prob)
print("Prior Odds:", prior_odds)
print("True Positive:", true_positive)
print("False Positive:", false_positive)
print("Likelihood Ratio:", likelihood_ratio)
print("Posterior Odds:", posterior_odds)
print("-" * 70)

# Special Findings
print("Special Findings - FOR BLOG 'Why Data?'")
# Finding 1 - The Frequency of People and employment-type
employment_type_counts = data["employment_type"].value_counts()
data_frame_3 = pd.DataFrame({
    "Category": employment_type_counts.index,
    "Value": employment_type_counts.values
})

# Barplot for Finding 1
plt.bar(data_frame_3["Category"], data_frame_3["Value"], color=["blue", "red", "yellow", "purple"])
plt.title("Frequency Distribution for Employment-type")
plt.xlabel("Employment Type")
plt.ylabel("Frequency of Employees")
plt.show()

# Finding 2 - The Frequency of People and experience-level
experience_level_counts = data["experience_level"].value_counts()
data_frame_4 = pd.DataFrame({
    "Category": experience_level_counts.index,
    "Value": experience_level_counts.values
})

# Barplot for Finding 2
plt.bar(data_frame_4["Category"], data_frame_4["Value"], color=["blue", "red", "purple", "yellow"])
plt.title("Frequency Distribution for Experience-Level")
plt.xlabel("Experience Level")
plt.ylabel("Frequency of Employees")
plt.show()

print("THE END")
