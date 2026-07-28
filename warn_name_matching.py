import os
import re
import time
from difflib import SequenceMatcher
import pandas as pd


def clean_company_name(name):
    """Standardizes company names by lowercasing, removing text inside brackets/parentheses,

    stripping specific punctuation, and removing corporate noise words safely.
    """
    if pd.isna(name):
        return ""

    # 1. Convert to lowercase string
    name_clean = str(name).lower()

    # 2. Strip text inside parentheses and brackets completely
    name_clean = re.sub(r"\(.*?\)", " ", name_clean)
    name_clean = re.sub(r"\[.*?\]", " ", name_clean)

    # 3. Strip punctuation and special characters
    name_clean = re.sub(r"[\:\.\,\;\-\_\/\?\*\(\)\[\]]", " ", name_clean)

    # Clean up multiple spaces that might have been introduced
    name_clean = " ".join(name_clean.split())

    # 4. Remove common corporate noise words safely using word boundaries (\b)
    noise_words = ["inc", "llc", "co", "hco", "com", "the", "corp", "corporation", "ltd", "limited"]
    pattern = r"\b(" + "|".join(noise_words) + r")\b"
    name_clean = re.sub(pattern, " ", name_clean)

    # Strip trailing/leading spaces and collapse remaining spaces
    name_clean = " ".join(name_clean.split())

    return name_clean


def get_match_score(str1, str2):
    """Calculates a compatibility percentage between two strings."""
    if not str1 or not str2:
        return 0.0
    return SequenceMatcher(None, str1, str2).ratio()


def main():
    # File configuration
    file_path = "layoff_warn.xlsx"

    if not os.path.exists(file_path):
        print(
            f"Error: '{file_path}' not found. Please ensure the excel file is in your working directory."
        )
        return

    print("Loading datasets...")
    xls = pd.ExcelFile(file_path)
    warn_df = pd.read_excel(xls, sheet_name="warn")
    comp_df = pd.read_excel(xls, sheet_name="compustat")

    print("Standardizing company names across both datasets...")
    # Apply identical cleaning step to both sides
    warn_df["Cleaned_Company"] = warn_df["Company"].apply(clean_company_name)
    comp_df["Cleaned_Compustat"] = comp_df["Company Name"].apply(
        clean_company_name
    )

    # Pre-build records to speed up row-by-row iteration
    comp_records = (
        comp_df[comp_df["Cleaned_Compustat"] != ""]
        .to_dict(orient="records")
    )

    # Result placeholders
    match_statuses = []
    matched_gvkeys = []
    matched_tickers = []
    matched_sics = []
    matched_comp_names = []

    total_rows = len(warn_df)
    print(f"Total rows to process in 'warn' dataset: {total_rows}")
    print("Matching companies...")

    start_time = time.time()
    log_interval = 100  # Tracks and logs progress every 100 records

    for idx, row in warn_df.iterrows():
        # --- Time Tracker Log Block ---
        current_count = idx + 1
        if current_count % log_interval == 0 or current_count == total_rows:
            elapsed_time = time.time() - start_time
            rows_per_sec = current_count / elapsed_time
            remaining_rows = total_rows - current_count
            eta_seconds = remaining_rows / rows_per_sec if rows_per_sec > 0 else 0

            elapsed_str = time.strftime("%M:%S", time.gmtime(elapsed_time))
            eta_str = time.strftime("%M:%S", time.gmtime(eta_seconds))
            progress_pct = (current_count / total_rows) * 100

            print(
                f"[{progress_pct:6.2f}% Complete] Processed {current_count}/{total_rows} rows | Elapsed: {elapsed_str} | ETA: {eta_str}")

        warn_clean = row["Cleaned_Company"]

        if not warn_clean:
            match_statuses.append("NA")
            matched_gvkeys.append(None)
            matched_tickers.append(None)
            matched_sics.append(None)
            matched_comp_names.append(None)
            continue

        warn_words = warn_clean.split()
        first_word = warn_words[0] if len(warn_words) > 0 else ""

        # Limit fuzzy comparison string length to block trailing location noise
        warn_compare = " ".join(warn_words[:4])

        best_score = 0.0
        best_match_record = None

        # --- Blocking / Candidate Filtering Step ---
        candidates = []
        for rec in comp_records:
            comp_clean = rec["Cleaned_Compustat"]
            comp_words = comp_clean.split()

            if comp_words and comp_words[0] == first_word:
                candidates.append(rec)
            elif len(warn_words) == 1 and warn_clean in comp_clean:
                candidates.append(rec)

        # --- Fuzzy Matching & Override Step ---
        for candidate in candidates:
            comp_clean = candidate["Cleaned_Compustat"]
            comp_words = comp_clean.split()
            comp_compare = " ".join(comp_words[:4])

            # 1. Base fuzzy similarity score
            score = get_match_score(warn_compare, comp_compare)

            # 2. Safe Acronym / Word Token Substring Override
            # Checks if full tokens match instead of slicing raw characters
            is_warn_token_subset = all(word in comp_words for word in warn_words)
            is_comp_token_subset = all(word in warn_words for word in comp_words)

            if is_warn_token_subset or is_comp_token_subset:
                # If it's a short unique phrase/acronym (like ABM)
                if len(warn_clean) <= 4 or len(comp_clean) <= 4:
                    # Generic guard list prevents common words from mismatching blindly
                    generic_guards = ["bank", "group", "systems", "tech", "media", "intl", "international"]
                    if not any(g in warn_clean for g in generic_guards):
                        score = max(score, 0.95)  # Structural match override
                else:
                    # Slight boost for longer true multi-word token subsets
                    score = max(score, 0.85)

            if score > best_score:
                best_score = score
                best_match_record = candidate

        # --- Classification Step ---
        # 60% remains the baseline limit for non-substring matches
        if best_match_record and best_score >= 0.60:
            percentage_label = f"{int(best_score * 100)}%"
            match_statuses.append(percentage_label)
            matched_gvkeys.append(best_match_record["gvkey"])
            matched_tickers.append(best_match_record["Ticker Symbol"])
            matched_sics.append(best_match_record["SIC Code"])
            matched_comp_names.append(best_match_record["Company Name"])
        else:
            match_statuses.append("NA")
            matched_gvkeys.append(None)
            matched_tickers.append(None)
            matched_sics.append(None)
            matched_comp_names.append(None)

    # Assign metadata values back to the main DataFrame
    warn_df["Match_Status"] = match_statuses
    warn_df["Matched_Compustat_Name"] = matched_comp_names
    warn_df["gvkey"] = matched_gvkeys
    warn_df["Ticker Symbol"] = matched_tickers
    warn_df["SIC Code"] = matched_sics

    # Clean out our temporary working column
    warn_df = warn_df.drop(columns=["Cleaned_Company"])

    # Output generation
    output_file = "layoff_warn_matched.xlsx"
    print(f"Saving results to {output_file}...")
    warn_df.to_excel(output_file, index=False)

    total_duration = time.strftime("%M:%S", time.gmtime(time.time() - start_time))
    print(f"Process completed successfully in {total_duration}!")


if __name__ == "__main__":
    main()