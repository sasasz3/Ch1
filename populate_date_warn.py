import re
import pandas as pd

# 1. Load the dataset
df = pd.read_csv("warn-data - for process.csv", keep_default_na=False)
df.columns = df.columns.str.strip()


# --- PASS 1: Find standard DD/MM/YYYY dates ---
def find_standard_date(row):
    """Scans the row ONLY for a standard DD/MM/YYYY date."""
    standard_pattern = r"\b\d{1,2}/\d{1,2}/\d{4}\b"
    for cell in row:
        cell_str = str(cell).strip()
        if re.search(standard_pattern, cell_str):
            return re.search(standard_pattern, cell_str).group()
    return None


# --- PASS 2: Fallback to Date(YYYY,MM,DD) strings ---
def find_excel_fallback_date(row):
    """Scans the row for Date(YYYY,MM,DD) string if standard date wasn't found."""
    excel_pattern = r"Date\((\d{4}),\s*(\d{1,2}),\s*(\d{1,2})\)"
    for cell in row:
        cell_str = str(cell).strip()
        match = re.search(excel_pattern, cell_str)
        if match:
            year, month, day = match.groups()
            # Standardize it straight to DD/MM/YYYY format
            return f"{day.zfill(2)}/{month.zfill(2)}/{year}"
    return None


# Function to check if Column F ('Effective Date') is empty or invalid
def is_date_missing(series):
    return (
        series.isna()
        | (series == "")
        | (series.astype(str).str.strip() == "")
        | (series.astype(str).str.contains("Date"))
    )


# Execution Pass 1: Prioritize standard DD/MM/YYYY anywhere in the row
missing_mask_1 = is_date_missing(df["Effective Date"])
df.loc[missing_mask_1, "Effective Date"] = df[missing_mask_1].apply(
    find_standard_date, axis=1
)

# Execution Pass 2: Fill remaining gaps using the Excel-style string format
missing_mask_2 = is_date_missing(df["Effective Date"])
df.loc[missing_mask_2, "Effective Date"] = df[missing_mask_2].apply(
    find_excel_fallback_date, axis=1
)

# 2. Save the perfectly ordered result
df.to_csv("warn_data_priority_cleaned.csv", index=False)
print("Data cleaning complete via strict two-pass priority strategy!")