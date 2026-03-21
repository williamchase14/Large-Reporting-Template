import pandas as pd
from pathlib import Path

# === File paths ===
final_combined_path = "Survey Data/Step_2_add_ipeds.csv"
msi_matrix_path = "Survey Data/MSI Eligibility Matrix_2024.csv"

# Keep your preferred filename
filename = "00 Source data file.csv"

# Primary output (existing relative folder)
output_path_1 = Path("Survey Data") / filename

# Second output (your OneDrive Master folder)
# Use a raw string (r"...") to avoid backslash-escape issues on Windows.
output_path_2 = Path(
    r"C:\Users\wmcqueen\OneDrive - Georgia State University\Desktop\Quarterly_Summary_Report_PostDP\Summary Report - R Charts\Master"
) / filename

# === Load CSV files ===
final_df = pd.read_csv(final_combined_path)
msi_df = pd.read_csv(msi_matrix_path)

# === Select only the relevant columns from the MSI matrix ===
msi_subset = msi_df[["unitid", "MSI Type"]]

# === Merge on 'unitid' ===
merged_df = pd.merge(final_df, msi_subset, on="unitid", how="left")

# === Fill missing MSI info with 'Non-MSI' ===
merged_df["MSI Type"] = merged_df["MSI Type"].fillna("Non-MSI")

# === Reorder columns to place MSI columns at the far left ===
msi_columns = ["MSI Type"]
other_columns = [col for col in merged_df.columns if col not in msi_columns]
merged_df = merged_df[msi_columns + other_columns]

# === Ensure output folders exist ===
output_path_1.parent.mkdir(parents=True, exist_ok=True)
output_path_2.parent.mkdir(parents=True, exist_ok=True)

# === Save the merged result to both places ===
# If you open in Excel, utf-8-sig helps preserve special characters
merged_df.to_csv(output_path_1, index=False, encoding="utf-8-sig")
merged_df.to_csv(output_path_2, index=False, encoding="utf-8-sig")

print(f"Merged file saved to: {output_path_1}")
print(f"Merged file also saved to: {output_path_2}")
