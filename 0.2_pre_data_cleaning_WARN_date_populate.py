import re
import pandas as pd
from config import DATA_FOLDER


#FILE TO CLEAN THE MESSY WARN DATA BY TAKING MISPLACED DATES AND PUTTING INTO THE RIGHT COLUMN


#load data
INPUT_FILE = DATA_FOLDER / "warn-data - for process.csv"
OUTPUT_FILE = DATA_FOLDER / "warn_data_date_fixed.csv"

df = pd.read_csv(INPUT_FILE, keep_default_na=False)
df.columns = df.columns.str.strip()


# find standard DD/MM/YYYY dates ---
def find_standard_date(row):
    """Scans the row ONLY for a standard DD/MM/YYYY date."""
    standard_pattern = r"\b\d{1,2}/\d{1,2}/\d{4}\b"
    for cell in row:
        cell_str = str(cell).strip()
        if re.search(standard_pattern, cell_str):
            return re.search(standard_pattern, cell_str).group()
    return None


# fallback to Date(YYYY,MM,DD) strings ---
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


#check if Column F ('Effective Date') is empty or invalid
def is_date_missing(series):
    return (
        series.isna()
        | (series == "")
        | (series.astype(str).str.strip() == "")
        | (series.astype(str).str.contains("Date"))
    )


# pass 1: Prioritize standard DD/MM/YYYY anywhere in the row
missing_mask_1 = is_date_missing(df["Effective Date"])
df.loc[missing_mask_1, "Effective Date"] = df[missing_mask_1].apply(
    find_standard_date, axis=1
)

# pass 2: Fill remaining gaps using the Excel-style string format
missing_mask_2 = is_date_missing(df["Effective Date"])
df.loc[missing_mask_2, "Effective Date"] = df[missing_mask_2].apply(
    find_excel_fallback_date, axis=1
)

def remove_out_of_range_effective_dates(dataframe):
    """Remove rows with Effective Dates before 2002 or after 2025."""
    effective_dates = pd.to_datetime(
        dataframe["Effective Date"],
        format="%d/%m/%Y",
        errors="coerce",
    )

    valid_year_mask = effective_dates.dt.year.between(2002, 2025)
    return dataframe.loc[valid_year_mask].copy()

df = remove_out_of_range_effective_dates(df)

# save changes
df.to_csv(OUTPUT_FILE, index=False)
print(f"Saved to: {OUTPUT_FILE}")