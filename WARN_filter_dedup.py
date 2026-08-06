import pandas as pd
from config import DATA_FOLDER

#FILES THE REMOVES THE PREVIOUSLY IDENTIFIED NO MATCH ROWS FROM WARN
#OBSERVATIONS IN THE SAME MONTH ARE TREATED AS THE SAME ANNOUNCEMENT
#COUNTS UNIQUE GVKEYS

# load data
INPUT_FILE = DATA_FOLDER / "warn_compustat_unfiltered.xlsx"
OUTPUT_FILE = DATA_FOLDER / "warn_compustat_filtered.xlsx"
INPUT_SHEET = "warn_matched"


data = pd.read_excel(
    INPUT_FILE,
    sheet_name=INPUT_SHEET,
    engine="openpyxl",
    dtype={
        "Compustat_gvkey": str,
    }
)

data = data.rename(columns=
{
    "Effective Date": "date",
    "Compustat_gvkey": "gvkey",
})


rows_original = len(data)

print(f"Original rows: {rows_original:,}")


#standardise required values
data["_original_row_order"] = range(
    len(data)
)

# standardise category values only for filtering.
data["_category_standardised"] = (
    data["Match_Category"]
    .astype("string")
    .str.strip()
    .str.upper()
)


# Convert the date column into actual dates.
# Invalid dates become NaT and will be retained without
# participating in monthly deduplication.
data["date"] = pd.to_datetime(
    data["date"],
    format="%d/%m/%Y",
    errors="coerce",
)


# delete "no match" rows
no_match_mask = (
    data["_category_standardised"]
    == "NO MATCH"
)

no_match_rows_removed = int(
    no_match_mask.sum()
)

filtered_data = (
    data.loc[~no_match_mask]
    .copy()
)

rows_after_no_match_removal = len(
    filtered_data
)

print(
    f"NO MATCH rows removed: "
    f"{no_match_rows_removed:,}"
)

print(
    f"Rows remaining before deduplication: "
    f"{rows_after_no_match_removal:,}"
)


#deduplication


# Identify valid gvkeys.
valid_gvkey = (
    filtered_data["gvkey"].notna()
    & filtered_data["gvkey"].ne("")
    & filtered_data["gvkey"].ne("<NA>")
    & filtered_data["gvkey"].str.lower().ne("nan")
)

# eligibility for deduplication valid gvkey valid date
eligible_for_deduplication = (
    valid_gvkey
    & filtered_data["date"].notna()
)

eligible_data = (
    filtered_data.loc[
        eligible_for_deduplication
    ]
    .copy()
)

# Sort chronologically within each firm.
# Original row order breaks ties where dates are identical.
eligible_data = eligible_data.sort_values(
    by=[
        "gvkey",
        "date",
        "_original_row_order",
    ],
    ascending=[
        True,
        True,
        True,
    ],
)

indexes_to_keep = []

# Apply a 60-calendar-day blackout from the most recently
# retained event for each GVKEY.
for gvkey, group in eligible_data.groupby(
    "gvkey",
    sort=False,
):
    last_retained_date = None

    for row_index, row in group.iterrows():
        current_date = row["date"]

        if (
            last_retained_date is None
            or (current_date - last_retained_date).days > 60
        ):
            indexes_to_keep.append(row_index)
            last_retained_date = current_date

# Remove eligible observations that were not retained.
eligible_indexes = set(eligible_data.index)
kept_indexes = set(indexes_to_keep)

duplicate_indexes_to_remove = list(
    eligible_indexes - kept_indexes
)

duplicate_rows_removed = len(
    duplicate_indexes_to_remove
)

filtered_data = filtered_data.drop(
    index=duplicate_indexes_to_remove
)

# Restore retained observations to their original input order.
filtered_data = (
    filtered_data
    .sort_values("_original_row_order")
    .reset_index(drop=True)
)

rows_final = len(filtered_data)

print(
    f"Rows removed by 60-day blackout: "
    f"{duplicate_rows_removed:,}"
)

print(
    f"Final rows retained: "
    f"{rows_final:,}"
)

# ============================================================
# FINAL ROW COUNTS
# ============================================================

exact_rows = int(
    (
        filtered_data["_category_standardised"]
        == "EXACT"
    ).sum()
)

review_rows = int(
    (
        filtered_data["_category_standardised"]
        == "REVIEW"
    ).sum()
)

other_rows = int(
    ~filtered_data[
        "_category_standardised"
    ].isin(
        [
            "EXACT",
            "REVIEW",
        ]
    )
    .sum()
)

# The previous expression is clearer when written explicitly.
other_rows = int(
    (
        ~filtered_data[
            "_category_standardised"
        ].isin(
            [
                "EXACT",
                "REVIEW",
            ]
        )
    ).sum()
)


# gvkey counts

final_valid_gvkey = (
    filtered_data["gvkey"].notna()
    & filtered_data["gvkey"].ne("")
    & filtered_data["gvkey"].ne("<NA>")
    & filtered_data["gvkey"].str.lower().ne("nan")
)

matched_with_gvkey = (
    filtered_data.loc[
        final_valid_gvkey
    ]
    .copy()
)


def classify_gvkey(categories):
    """
    Assign one final category to each unique gvkey.

    If a gvkey has both EXACT and REVIEW observations, classify
    it as EXACT so it is counted only once.
    """
    category_set = {
        str(category).strip().upper()
        for category in categories
    }

    if "EXACT" in category_set:
        return "EXACT"

    if "REVIEW" in category_set:
        return "REVIEW"

    return "OTHER"


gvkey_categories = (
    matched_with_gvkey
    .groupby("gvkey")[
        "Match_Category"
    ]
    .apply(classify_gvkey)
)

unique_exact_gvkeys = int(
    (gvkey_categories == "EXACT").sum()
)

unique_review_only_gvkeys = int(
    (gvkey_categories == "REVIEW").sum()
)

unique_other_gvkeys = int(
    (gvkey_categories == "OTHER").sum()
)

total_unique_gvkeys = int(
    gvkey_categories.index.nunique()
)


# gvkey audit
gvkey_summary = (
    gvkey_categories
    .rename("final_gvkey_category")
    .reset_index()
)

gvkey_row_counts = (
    matched_with_gvkey
    .groupby("gvkey")
    .size()
    .rename("retained_layoff_rows")
    .reset_index()
)

gvkey_first_date = (
    matched_with_gvkey
    .groupby("gvkey")["date"]
    .min()
    .rename("first_retained_date")
    .reset_index()
)

gvkey_last_date = (
    matched_with_gvkey
    .groupby("gvkey")["date"]
    .max()
    .rename("last_retained_date")
    .reset_index()
)

gvkey_summary = gvkey_summary.merge(
    gvkey_row_counts,
    on="gvkey",
    how="left",
)

gvkey_summary = gvkey_summary.merge(
    gvkey_first_date,
    on="gvkey",
    how="left",
)

gvkey_summary = gvkey_summary.merge(
    gvkey_last_date,
    on="gvkey",
    how="left",
)


#summary

processing_summary = pd.DataFrame(
    {
        "measure": [
            "original rows",
            "no match rows removed",
            "rows after no match removal",
            "duplicate gvkey-month rows removed",
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
    columns=[
        "_original_row_order",
        "_category_standardised",
    ]
)


# create output file

print(f"Writing: {OUTPUT_FILE}")

with pd.ExcelWriter(
    OUTPUT_FILE,
    engine="openpyxl",
    date_format="yyyy-mm-dd",
    datetime_format="yyyy-mm-dd",
) as writer:
    filtered_data.to_excel(
        writer,
        sheet_name="layoffs",
        index=False,
    )


# ============================================================
# FINAL LOG
# ============================================================

print()
print("=" * 75)
print("FILTERING AND DEDUPLICATION COMPLETED")
print("=" * 75)
print(f"Original rows:                  {rows_original:,}")
print(f"NO MATCH rows removed:          {no_match_rows_removed:,}")
print(f"Monthly duplicates removed:     {duplicate_rows_removed:,}")
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