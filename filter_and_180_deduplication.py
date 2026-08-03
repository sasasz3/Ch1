import pandas as pd
from config import DATA_FOLDER


# load data
INPUT_FILE = DATA_FOLDER / "warn_compustat_unfiltered.xlsx"
OUTPUT_FILE = DATA_FOLDER / "warn_compustat_filtered.xlsx"
INPUT_SHEET = "warn_matched"
WINDOW_DAYS = 60


# read input
if not INPUT_FILE.exists():
    raise FileNotFoundError(f"Input file does not exist: {INPUT_FILE}")

print("=" * 75)
print("FILTERING AND DEDUPLICATION")
print("=" * 75)
print(f"Reading: {INPUT_FILE}")

data = pd.read_excel(
    INPUT_FILE,
    sheet_name=INPUT_SHEET,
    engine="openpyxl",
    dtype={"Compustat_gvkey": str},
)


# validate required columns
required_columns = {
    "Compustat_gvkey",
    "Effective Date",
    "Match_Category",
}

missing_columns = required_columns.difference(data.columns)

if missing_columns:
    raise KeyError(
        "The input workbook is missing these required columns: "
        + ", ".join(sorted(missing_columns))
    )

rows_original = len(data)
print(f"Original rows: {rows_original:,}")


# standardise required values
data["_original_row_order"] = range(len(data))

data["_category_standardised"] = (
    data["Match_Category"].astype("string").str.strip().str.upper()
)

# Preserve gvkey as a string so leading zeroes are not lost.
data["Compustat_gvkey"] = (
    data["Compustat_gvkey"].astype("string").str.strip()
)

# Invalid dates become NaT and are retained without participating
# in the 180-day deduplication.
data["Effective Date"] = pd.to_datetime(
    data["Effective Date"],
    format="%d/%m/%Y",
    errors="coerce",
)


# delete "no match" rows
no_match_mask = data["_category_standardised"] == "NO MATCH"
no_match_rows_removed = int(no_match_mask.sum())
filtered_data = data.loc[~no_match_mask].copy()
rows_after_no_match_removal = len(filtered_data)

print(f"NO MATCH rows removed: {no_match_rows_removed:,}")
print(f"Rows remaining before deduplication: {rows_after_no_match_removal:,}")


# deduplication
valid_gvkey = (
    filtered_data["Compustat_gvkey"].notna()
    & filtered_data["Compustat_gvkey"].ne("")
    & filtered_data["Compustat_gvkey"].ne("<NA>")
    & filtered_data["Compustat_gvkey"].str.lower().ne("nan")
)

eligible_for_deduplication = (
    valid_gvkey & filtered_data["Effective Date"].notna()
)

eligible_data = filtered_data.loc[eligible_for_deduplication].sort_values(
    by=[
        "Compustat_gvkey",
        "Effective Date",
        "_original_row_order",
    ]
)


def indexes_to_remove_within_window(group):
    """Return rows occurring no more than 180 days after each retained row."""
    remove_indexes = []
    retained_date = None

    for row_index, effective_date in group["Effective Date"].items():
        if retained_date is None:
            retained_date = effective_date
            continue

        # Days 0 through 180 are duplicates. Day 181 starts a new window.
        if effective_date <= retained_date + pd.Timedelta(days=WINDOW_DAYS):
            remove_indexes.append(row_index)
        else:
            retained_date = effective_date

    return remove_indexes


duplicate_indexes_to_remove = []

for _, gvkey_group in eligible_data.groupby(
    "Compustat_gvkey",
    sort=False,
):
    duplicate_indexes_to_remove.extend(
        indexes_to_remove_within_window(gvkey_group)
    )

duplicate_rows_removed = len(duplicate_indexes_to_remove)
filtered_data = filtered_data.drop(index=duplicate_indexes_to_remove)

# Restore retained observations to their original workbook order.
filtered_data = (
    filtered_data.sort_values("_original_row_order").reset_index(drop=True)
)

rows_final = len(filtered_data)

print(f"Rows removed within 180-day windows: {duplicate_rows_removed:,}")
print(f"Final rows retained: {rows_final:,}")


# final row counts
exact_rows = int((filtered_data["_category_standardised"] == "EXACT").sum())
review_rows = int((filtered_data["_category_standardised"] == "REVIEW").sum())
other_rows = int(
    (~filtered_data["_category_standardised"].isin(["EXACT", "REVIEW"])).sum()
)


# gvkey counts
final_valid_gvkey = (
    filtered_data["Compustat_gvkey"].notna()
    & filtered_data["Compustat_gvkey"].ne("")
    & filtered_data["Compustat_gvkey"].ne("<NA>")
    & filtered_data["Compustat_gvkey"].str.lower().ne("nan")
)

matched_with_gvkey = filtered_data.loc[final_valid_gvkey].copy()


def classify_gvkey(categories):
    """Assign one final category to each unique gvkey."""
    category_set = {str(category).strip().upper() for category in categories}

    if "EXACT" in category_set:
        return "EXACT"
    if "REVIEW" in category_set:
        return "REVIEW"
    return "OTHER"


gvkey_categories = (
    matched_with_gvkey.groupby("Compustat_gvkey")["Match_Category"]
    .apply(classify_gvkey)
)

unique_exact_gvkeys = int((gvkey_categories == "EXACT").sum())
unique_review_only_gvkeys = int((gvkey_categories == "REVIEW").sum())
unique_other_gvkeys = int((gvkey_categories == "OTHER").sum())
total_unique_gvkeys = int(gvkey_categories.index.nunique())


# gvkey audit
gvkey_summary = gvkey_categories.rename("final_gvkey_category").reset_index()

gvkey_row_counts = (
    matched_with_gvkey.groupby("Compustat_gvkey")
    .size()
    .rename("retained_layoff_rows")
    .reset_index()
)

gvkey_first_date = (
    matched_with_gvkey.groupby("Compustat_gvkey")["Effective Date"]
    .min()
    .rename("first_retained_date")
    .reset_index()
)

gvkey_last_date = (
    matched_with_gvkey.groupby("Compustat_gvkey")["Effective Date"]
    .max()
    .rename("last_retained_date")
    .reset_index()
)

gvkey_summary = gvkey_summary.merge(
    gvkey_row_counts,
    on="Compustat_gvkey",
    how="left",
)
gvkey_summary = gvkey_summary.merge(
    gvkey_first_date,
    on="Compustat_gvkey",
    how="left",
)
gvkey_summary = gvkey_summary.merge(
    gvkey_last_date,
    on="Compustat_gvkey",
    how="left",
)


# summary
processing_summary = pd.DataFrame(
    {
        "measure": [
            "original rows",
            "no match rows removed",
            "rows after no match removal",
            "rows removed within 180-day gvkey windows",
            "final rows retained",
            "final exact rows",
            "final review rows",
            "final other-category rows",
            "unique exact gvkeys",
            "unique review-only gvkeys",
            "unique other-category gvkeys",
            "total unique retained gvkeys",
        ],
        "count": [
            rows_original,
            no_match_rows_removed,
            rows_after_no_match_removal,
            duplicate_rows_removed,
            rows_final,
            exact_rows,
            review_rows,
            other_rows,
            unique_exact_gvkeys,
            unique_review_only_gvkeys,
            unique_other_gvkeys,
            total_unique_gvkeys,
        ],
    }
)


# remove temp columns
filtered_data = filtered_data.drop(
    columns=["_original_row_order", "_category_standardised"]
)


# create output file
print(f"Writing: {OUTPUT_FILE}")

with pd.ExcelWriter(
    OUTPUT_FILE,
    engine="openpyxl",
    date_format="yyyy-mm-dd",
    datetime_format="yyyy-mm-dd",
) as writer:
    filtered_data.to_excel(writer, sheet_name="layoffs", index=False)
    processing_summary.to_excel(
        writer,
        sheet_name="processing_summary",
        index=False,
    )
    gvkey_summary.to_excel(writer, sheet_name="gvkey_summary", index=False)


# final log
print()
print("=" * 75)
print("FILTERING AND DEDUPLICATION COMPLETED")
print("=" * 75)
print(f"Original rows:                  {rows_original:,}")
print(f"NO MATCH rows removed:          {no_match_rows_removed:,}")
print(f"180-day duplicates removed:     {duplicate_rows_removed:,}")
print(f"Final rows retained:            {rows_final:,}")
print("-" * 75)
print(f"Final exact rows:               {exact_rows:,}")
print(f"Final review rows:              {review_rows:,}")
print(f"Final other-category rows:      {other_rows:,}")
print("-" * 75)
print(f"Unique exact gvkeys:            {unique_exact_gvkeys:,}")
print(f"Unique review-only gvkeys:      {unique_review_only_gvkeys:,}")
print(f"Unique other-category gvkeys:   {unique_other_gvkeys:,}")
print(f"Total unique retained gvkeys:   {total_unique_gvkeys:,}")
print("-" * 75)
print(f"Output file: {OUTPUT_FILE}")
print("=" * 75)
