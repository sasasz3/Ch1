from pathlib import Path

import pandas as pd


# ============================================================
# FILE LOCATIONS
# ============================================================

CH1_FOLDER = Path(
    r"C:\Users\nxb23156\Documents\OneDrive\Ch1"
)

# This is the lowercase/standardised output created previously.
INPUT_FILE = (
    CH1_FOLDER
    / "layoffs_for_analysis.xlsx"
)

OUTPUT_FILE = (
    CH1_FOLDER
    / "layoffs_for_analysis.xlsx"
)

INPUT_SHEET = "warn_matched"


# ============================================================
# READ INPUT
# ============================================================

if not INPUT_FILE.exists():
    raise FileNotFoundError(
        f"Input file does not exist: {INPUT_FILE}"
    )

print("=" * 75)
print("FILTERING AND DEDUPLICATION")
print("=" * 75)
print(f"Reading: {INPUT_FILE}")

data = pd.read_excel(
    INPUT_FILE,
    sheet_name=INPUT_SHEET,
    engine="openpyxl",
    dtype={
        "gvkey": str,
    },
)


# ============================================================
# VALIDATE REQUIRED COLUMNS
# ============================================================

required_columns = {
    "gvkey",
    "date",
    "match_category",
}

missing_columns = required_columns.difference(
    data.columns
)

if missing_columns:
    raise KeyError(
        "The input workbook is missing these required columns: "
        + ", ".join(
            sorted(missing_columns)
        )
    )

rows_original = len(data)

print(f"Original rows: {rows_original:,}")


# ============================================================
# STANDARDISE REQUIRED VALUES
# ============================================================

# Preserve the original order for final sorting and date ties.
data["_original_row_order"] = range(
    len(data)
)

# Standardise category values only for filtering.
data["_category_standardised"] = (
    data["match_category"]
    .astype("string")
    .str.strip()
    .str.upper()
)

# Preserve gvkey as a string so leading zeroes are not lost.
data["gvkey"] = (
    data["gvkey"]
    .astype("string")
    .str.strip()
)

# Convert the date column into actual dates.
# Invalid dates become NaT and will be retained without
# participating in monthly deduplication.
data["date"] = pd.to_datetime(
    data["date"],
    errors="coerce",
)


# ============================================================
# DELETE NO-MATCH ROWS
# ============================================================

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


# ============================================================
# PREPARE MONTHLY DEDUPLICATION
# ============================================================

# Create a calendar year-month variable.
#
# January 2020 and January 2021 are separate groups.
filtered_data["_calendar_month"] = (
    filtered_data["date"]
    .dt.to_period("M")
)

# Identify valid gvkeys.
valid_gvkey = (
    filtered_data["gvkey"].notna()
    & filtered_data["gvkey"].ne("")
    & filtered_data["gvkey"].ne("<NA>")
    & filtered_data["gvkey"].str.lower().ne("nan")
)

# Only rows with both a valid gvkey and valid date can be
# included in monthly duplicate groups.
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


# ============================================================
# REMOVE SAME-GVKEY, SAME-MONTH DUPLICATES
# ============================================================

# Sort each gvkey-month group so its earliest date is first.
#
# If two observations have the same earliest date, the one that
# appeared first in the input workbook is retained.
eligible_data = eligible_data.sort_values(
    by=[
        "gvkey",
        "_calendar_month",
        "date",
        "_original_row_order",
    ],
    ascending=[
        True,
        True,
        True,
        True,
    ],
)

duplicate_mask = eligible_data.duplicated(
    subset=[
        "gvkey",
        "_calendar_month",
    ],
    keep="first",
)

duplicate_indexes_to_remove = (
    eligible_data.loc[
        duplicate_mask
    ].index
)

duplicate_rows_removed = len(
    duplicate_indexes_to_remove
)

filtered_data = filtered_data.drop(
    index=duplicate_indexes_to_remove
)

# Restore the retained observations to their original order.
filtered_data = (
    filtered_data
    .sort_values("_original_row_order")
    .reset_index(drop=True)
)

rows_final = len(filtered_data)

print(
    f"Duplicate gvkey-month rows removed: "
    f"{duplicate_rows_removed:,}"
)

print(
    f"Final rows retained: {rows_final:,}"
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


# ============================================================
# FINAL UNIQUE-GVKEY COUNTS
# ============================================================

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
        "match_category"
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


# ============================================================
# GVKEY-LEVEL AUDIT TABLE
# ============================================================

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


# ============================================================
# PROCESSING SUMMARY
# ============================================================

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


# ============================================================
# REMOVE TEMPORARY COLUMNS
# ============================================================

filtered_data = filtered_data.drop(
    columns=[
        "_original_row_order",
        "_category_standardised",
        "_calendar_month",
    ]
)


# ============================================================
# WRITE FINAL WORKBOOK
# ============================================================

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

    processing_summary.to_excel(
        writer,
        sheet_name="processing_summary",
        index=False,
    )

    gvkey_summary.to_excel(
        writer,
        sheet_name="gvkey_summary",
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