import pandas as pd

# === File paths ===
combined_csv_path = "Survey Data/Step_1_combine_qualtrics.csv"
reference_csv_path = "Survey Data/Step_0_Summary Report Reference Table.csv"
output_path = "Survey Data/Step_2_add_ipeds.csv"


# === Load files ===
combined_df = pd.read_csv(combined_csv_path)  # Assume headers are present
reference_df = pd.read_csv(reference_csv_path)

# === Drop rows with missing 'School' in reference table ===
reference_df = reference_df.dropna(subset=["School"])

# === Ensure combined_df has correct headers ===
expected_fixed_headers = [
    "School", "Survey #", "Post D&P Year", "Survey Type", "Question #",
    "Recommendation #", "Question Type", "Pillar", "Question",
    "Qualtrics Metadata", "Weighted % Complete"
]
# If headers are incorrect or missing, fix them
if not all(col in combined_df.columns for col in expected_fixed_headers):
    num_respondents = combined_df.shape[1] - len(expected_fixed_headers)
    respondent_headers = [f"Respondent {i+1}" for i in range(num_respondents)]
    combined_df.columns = expected_fixed_headers + respondent_headers

# === Merge on 'School' ===
merged_df = pd.merge(
    combined_df,
    reference_df.drop(columns=["School"]),
    left_on="School",
    right_on=reference_df["School"],
    how="left"
)

# === Reorder columns: reference metadata first ===
metadata_columns = [col for col in reference_df.columns if col != "School"]
final_columns = metadata_columns + list(combined_df.columns)
merged_df = merged_df[final_columns]

# === Remove any duplicate header rows if present ===
if merged_df.iloc[0].equals(pd.Series(merged_df.columns)):
    merged_df = merged_df.iloc[1:].reset_index(drop=True)

# === Save final output ===
merged_df.to_csv(output_path, index=False)
print(f"Merged and cleaned file saved to: {output_path}")
