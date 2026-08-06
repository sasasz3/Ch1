import pandas as pd
from config import DATA_FOLDER


# FILE TO FILTER CAPIQ FOR VALIDATED NON-LAYOFF EVENTS
# AND FILTER OUT DUPLICATE EVENTS IN A 60-DAY WINDOW FROM INITIAL EVENT


INPUT_FILE = DATA_FOLDER / "capitaliq_cleaned.xlsx"
OUTPUT_FILE = DATA_FOLDER / "capitaliq_filtered.xlsx"

df = pd.read_excel(INPUT_FILE, dtype={"gvkey": str})

#rename stupid column
df = df.rename(columns={"Key Developments By Date": "date"})


#get rid of the previously identified non-layoff events
n_before = len(df)
df = df[df["Final_Flag"] == 1].copy()
n_after = len(df)

print(f"Removed {n_before - n_after:,} non-layoff observations.")
print(f"Remaining observations: {n_after:,}")


#create groups of gvkeys in chronological order to get rid of duplicate announcements
df['date_dt'] = pd.to_datetime(df['date'], errors='coerce')
df_valid = df.dropna(subset=['date_dt', 'gvkey']).copy()
total_before = len(df_valid)

#ordering chronologically
df_valid = df_valid.sort_values(['gvkey', 'date_dt'])


#hold gvkey values automatically for rolling
def rolling_blackout(group):
    current_gvkey = group.name

    kept_rows = []
    last_date = None

    for _, row in group.iterrows():
        if last_date is None or (row['date_dt'] - last_date).days > 60:
            kept_rows.append(row)
            last_date = row['date_dt']

    #rows back to main dataframe
    result = pd.DataFrame(kept_rows)
    #reassign gvkey
    result['gvkey'] = current_gvkey
    return result


print("Filtering ...")
df_final = df_valid.groupby('gvkey', as_index=False, group_keys=False).apply(rolling_blackout)

#reset index
df_final = df_final.reset_index(drop=True)
unique_firms = df_final["gvkey"].nunique()

#print stuff
print("\n" + "=" * 30)
print(f"Total Valid Rows: {total_before}")
print(f"Final Unique Events: {len(df_final)}")
print(f"Unique Firms: {unique_firms:,}")
print("=" * 30)



df_final.to_excel(OUTPUT_FILE, index=False, engine='openpyxl')

print(f"Done! Check '{OUTPUT_FILE}'.")