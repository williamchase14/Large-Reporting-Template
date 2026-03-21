import os
import glob
import pandas as pd

# === UPDATED PATHS ===
input_folder = r"C:\Users\wmcqueen\OneDrive - Georgia State University\Desktop\Quarterly_Summary_Report_PostDP\Survey Data\All CSVs"
row_count_file = r"C:\Users\wmcqueen\OneDrive - Georgia State University\Desktop\Quarterly_Summary_Report_PostDP\Survey Data\Row_Counts_Summary.csv"

# === COLLECT ALL CSVs IN THE FOLDER (EXCEPT THE OUTPUT) ===
all_csvs = glob.glob(os.path.join(input_folder, "*.csv"))

# Skip the summary file if it exists in the same folder
all_csvs = [p for p in all_csvs if os.path.abspath(p) != os.path.abspath(row_count_file)]

row_count_data = []

for path in sorted(all_csvs):
    try:
        df = pd.read_csv(path, encoding="utf-8-sig", on_bad_lines="skip")
    except UnicodeDecodeError:
        df = pd.read_csv(path, encoding="latin1", on_bad_lines="skip")

    original_row_count = len(df)
    adjusted_row_count = max(original_row_count - 2, 0)

    file_name = os.path.basename(path)
    name_without_ext = os.path.splitext(file_name)[0]

    # === EXTRACT SCHOOL NAME FROM FILE NAME ===
    # Example filename pattern:
    #   "123+-+School+Name_Report_2025.csv"
    try:
        school_name = name_without_ext.split("+-+")[1].split("_")[0].replace("+", " ")
    except Exception:
        # Fallback: use entire filename (minus extension)
        school_name = name_without_ext

    row_count_data.append({
        "School": school_name,
        "File Name": file_name,
        "Adjusted Row Count (minus 2)": adjusted_row_count
    })

# Build DataFrame
row_count_df = pd.DataFrame(row_count_data)

if row_count_df.empty:
    print(f"No CSV files found in: {input_folder}")
else:
    row_count_df = row_count_df.sort_values(by="School", ignore_index=True)

    # Create TOTAL row
    total_row = pd.DataFrame([{
        "School": "TOTAL",
        "File Name": "",
        "Adjusted Row Count (minus 2)": row_count_df["Adjusted Row Count (minus 2)"].sum()
    }])

    # Append total row
    row_count_df = pd.concat([row_count_df, total_row], ignore_index=True)

    # Save output CSV
    row_count_df.to_csv(row_count_file, index=False)

    print(f"Row count summary saved to: {row_count_file}")
    print(f"Files processed: {len(all_csvs)}")
    print(f"Total adjusted row count: {int(row_count_df.loc[row_count_df['School'] == 'TOTAL', 'Adjusted Row Count (minus 2)'].iloc[0])}")