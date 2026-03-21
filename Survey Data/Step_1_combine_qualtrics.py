import pandas as pd
import os
import re

# Directory where all the survey CSV files are located
directory = "C:/Users/wmcqueen/OneDrive - Georgia State University/Desktop/Quarterly_Summary_Report_PostDP/Survey Data/All CSVs"

# Output file paths
output_file = "C:/Users/wmcqueen/OneDrive - Georgia State University/Desktop/Quarterly_Summary_Report_PostDP/Survey Data/Step_1_Combine_Qualtrics.csv"
row_count_file = "C:/Users/wmcqueen/OneDrive - Georgia State University/Desktop/Quarterly_Summary_Report_PostDP/Survey Data/Row_Counts_Summary.csv"

# Pillar abbreviation to full name mapping
pillar_map = {
    "ADS": "Academic Design & Support",
    "COL": "Career Oriented Learning",
    "Data": "Data",
    "FW": "Financial Wellness",
    "OC": "Outreach and Communication",
    "PA": "Proactive Advising",
    "SFYS": "Structured First Year Support",
    "FR": "Financial Reporting",
    "FRS": "Financial Report Signoff",
    "ICP": "Identified Campus Personnel",
    "ICSSW": "Institutional Context for Student Success Work",
    "Intro": "Introduction and Instructions",
    "NS": "NISS Support",
    "OF": "Open Feedback",
    "OPI": "Overall Project Impact",
    "RecipientEmail": "Metadata",
    "RecipientFirstName": "Metadata",
    "RecipientLastName": "Metadata",
    "RecordedDate": "Metadata",
    "ResponseId": "Metadata",
    "StartDate": "Metadata",
    "Status": "Metadata",
    "UserLanguage": "Metadata",
    "DistributionChannel": "Metadata",
    "Duration (in seconds)": "Metadata",
    "EndDate": "Metadata",
    "ExternalReference": "Metadata",
    "Finished": "Metadata",
    "IPAddress": "Metadata",
    "LocationLatitude": "Metadata",
    "LocationLongitude": "Metadata",
    "Progress": "Metadata"
}

recommendation_map = {
    "R1": "Recommendation 1",
    "R2": "Recommendation 2",
    "R3": "Recommendation 3",
    "R4": "Recommendation 4"
}

question_type_map = {
    "T": "Quantitative",
    "L": "Qualitative",
    "F": "Follow-up",
    "I": "Informative",
    "U": "User Info",
    "N": "Not Required"
}

def extract_question_type(cell):
    if pd.isna(cell):
        return "Metadata"
    parts = str(cell).replace('.', '_').split('_')
    for part in parts:
        if part in question_type_map:
            return question_type_map[part]
    return "Metadata"

def extract_recommendation(cell):
    if pd.isna(cell):
        return "Not a Recommendation"
    parts = str(cell).replace('.', '_').split('_')
    for part in parts:
        if part in recommendation_map:
            return recommendation_map[part]
    for part in parts:
        for key in recommendation_map:
            if part.startswith(key):
                return recommendation_map[key]
    return "Not a Recommendation"

def extract_pillar(cell):
    if pd.isna(cell):
        return ""
    parts = str(cell).replace('.', '_').split('_')
    for part in parts:
        if part in pillar_map:
            return pillar_map[part]
    for part in parts:
        for key in pillar_map:
            if part.startswith(key):
                return pillar_map[key]
    return ""

# Create lists to store data
df_list = []
row_count_data = []

# Loop through each CSV file in the directory
for file in os.listdir(directory):
    if file.endswith(".csv"):
        try:
            # Read the CSV file
            file_path = os.path.join(directory, file)
            df = pd.read_csv(file_path, header=None)
            
            # Count rows and subtract 3
            original_row_count = len(df)
            adjusted_row_count = original_row_count - 3
            
            # Store row count information
            row_count_data.append({
                "File Name": file,
                "Original Row Count": original_row_count,
                "Adjusted Row Count (minus 3)": adjusted_row_count
            })

            # Transpose the DataFrame
            df = df.transpose()
            df.reset_index(drop=True, inplace=True)

            # Add the Pillar column based on the first column's value
            df.insert(1, "Pillar", df.iloc[:, 0].apply(extract_pillar))

            # Add a "School" column with the name of the school (extracted from the file name)
            school_name = file.split("+-+")[1].split("_")[0].replace("+", " ")
            df.insert(0, "School", school_name)

            # Extract the "Survey #"
            survey_part = ""
            parts = file.split("+-+")
            for part in parts:
                if part.strip().startswith("Survey"):
                    survey_part = part.strip().split("_")[0].replace("+", " ")
                    break
            if not survey_part:
                for seg in file.replace("+", " ").split("-"):
                    if seg.strip().startswith("Survey"):
                        survey_part = seg.strip()
                        break
            df.insert(1, "Survey", survey_part)

            # Extract the "Year"
            year_match = re.search(r'Year\+\d+', file)
            year_str = ""
            if year_match:
                year_str = year_match.group().replace("+", " ")
            df.insert(2, "Year", year_str)

            # Add "Survey Type" column
            survey_type = "Acceleration Grant Survey" if "+AG+Report_" in file else "Standard Survey"
            df.insert(3, "Survey Type", survey_type)

            # Add Recommendation column
            df.insert(5, "Recommendation", df.iloc[:, 4].apply(extract_recommendation))

            # Add Question Type column
            df.insert(6, "Question Type", df.iloc[:, 4].apply(extract_question_type))

            # Insert placeholder for Weighted Progress
            df.insert(10, "Weighted Progress", None)

            def weighted_progress(row):
                idx = row.index.get_loc("Weighted Progress")
                statuses = row.values[idx+1:]
                completed = (statuses == "Completed").sum()
                implementing = (statuses == "Implementing").sum()
                planning = (statuses == "Planning").sum()
                not_initiated = (statuses == "Not Initiated").sum()
                not_yet_initiated = (statuses == "Not Yet Initiated").sum()

                numerator = (
                    1 * completed +
                    0.667 * implementing +
                    0.333 * planning +
                    0 * not_initiated +
                    0 * not_yet_initiated
                )
                denominator = (
                    completed +
                    implementing +
                    planning +
                    not_initiated +
                    not_yet_initiated
                )
                if denominator == 0:
                    return "No Valid Responses"
                return round(numerator / denominator, 3)

            df["Weighted Progress"] = df.apply(weighted_progress, axis=1)

            # Append the transposed DataFrame to the list
            df_list.append(df)
        except Exception as e:
            print(f"Error processing file {file}: {e}")

# Combine all transposed DataFrames into a single DataFrame
combined_df = pd.concat(df_list, ignore_index=True)

# Define headers for the combined file
fixed_headers = [
    "School", "Survey #", "Post D&P Year", "Survey Type", "Question #",
    "Recommendation #", "Question Type", "Pillar", "Question",
    "Qualtrics Metadata", "Weighted % Complete"
]

num_total_columns = combined_df.shape[1]
num_respondents = num_total_columns - len(fixed_headers)
respondent_headers = [f"Respondent {i+1}" for i in range(num_respondents)]
all_headers = fixed_headers + respondent_headers
combined_df.columns = all_headers

# Export combined data
combined_df.to_csv(output_file, index=False, header=True)
print(f"Combined data saved to {output_file}")

# Create and export row count summary
row_count_df = pd.DataFrame(row_count_data)
total_adjusted = row_count_df["Adjusted Row Count (minus 3)"].sum()

# Add a total row
total_row = pd.DataFrame([{
    "File Name": "TOTAL",
    "Original Row Count": row_count_df["Original Row Count"].sum(),
    "Adjusted Row Count (minus 3)": total_adjusted
}])
row_count_df = pd.concat([row_count_df, total_row], ignore_index=True)

# Export row count summary
row_count_df.to_csv(row_count_file, index=False)
print(f"Row count summary saved to {row_count_file}")
print(f"Total adjusted row count: {total_adjusted}")