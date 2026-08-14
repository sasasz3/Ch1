import pandas as pd
import re
from config import DATA_FOLDER


# FILE TO CLEAN THE CAPIQ DATASET FROM INVALID GVKEYS AND SIC CODES THAT ARE NOT NEEDED
# AND IDENTIFY NON-LAYOFF EVENTS - TO BE REVIEWED MANUALLY


INPUT_FILE = DATA_FOLDER / "capitaliq_raw.xlsx"
OUTPUT_FILE = DATA_FOLDER / "capitaliq_cleaned.xlsx"



#DEFINING LAYOFF-RELATED KEYWORDS

# direct: All permutations of layoffs, downsizing, redundancies and reduction in
LAYOFF_DIRECT = r'\blay\w*[- ]?off\w*|downsiz\w*|redundan\w*|reduction\s?in\s?force|rif'

# workforce reduction related action verbs
ACTION_ROOTS = r'\b(?:reduc\w*|cut\w*|rightsiz\w*|eliminat\w*|terminat\w*|displac\w*|shed\w*|slash\w*|ax\w*|trim\w*|chop\w*|jettison\w*|let\w*\s?go|eras\w*|loss\w*|lose|put\s?out|fire\w*|drop\w*)\b'

# synonyms for the term workforce
WORKFORCE_ROOTS = r'\b(?:employ\w*|work\s?force|position\w*|colleague\w*|staff\w*|role\w*|job\w*|worker\w*|head\s?count|manpower|people|member\w*|dealer\w*|sales\s?force|hand\w*|personnel|team\s?member\w*)\b'

# closure related words
CLOSE_VERBS = r'\bclos\w*\b'


#implementation
def classify_dataset(df):

    h = df['Key Development Headline'].fillna('').astype(str).str.lower()
    s = df['Key Development Situation'].fillna('').astype(str).str.lower()

    # closure words only being searched in the headline to avoid false positives
    df['Closure_Dummy'] = h.str.contains(CLOSE_VERBS, regex=True).astype(int)

    # direct layoff words may appear in both headline and situation
    df['Pure_Layoff_Dummy'] = (h.str.contains(LAYOFF_DIRECT, regex=True, flags=re.I) |
                               s.str.contains(LAYOFF_DIRECT, regex=True, flags=re.I)).astype(int)

    # workforce related words has to be coupled with action words
    # may appear in both headline or situation summary
    def check_borderline(text):
        return bool(re.search(ACTION_ROOTS, text, re.I)) and bool(re.search(WORKFORCE_ROOTS, text, re.I))

    is_borderline = h.apply(check_borderline)
    df['Borderline_Action_Dummy'] = ((is_borderline) &
                                     (df['Pure_Layoff_Dummy'] == 0) &
                                     (df['Closure_Dummy'] == 0)).astype(int)

    # for easier filtering later
    df['Final_Flag'] = ((df['Closure_Dummy'] == 1) |
                        (df['Pure_Layoff_Dummy'] == 1) |
                        (df['Borderline_Action_Dummy'] == 1)).astype(int)

    return df


# execution

try:
    df = pd.read_excel(INPUT_FILE, sheet_name='Screening')


# getting rid of invalid gvkeys
# these are mostly announcements with more than one ExcelID that the plugin couldn't match
# they could be validated

    n_before = len(df)
    df["gvkey"] = df["gvkey"].astype(str)
    valid_gvkey = df["gvkey"].str.fullmatch(r"GV_\d{6}")
    df = df[valid_gvkey].copy()

    # remove the "GV_" prefix while preserving leading zeros
    df["gvkey"] = df["gvkey"].str.replace("GV_", "", regex=False)

    n_after = len(df)

    print(f"Removed {n_before - n_after:,} observations with invalid GVKEYs.")


# get rid of financial and utilities before filtering for non-layoff events
    n_before = len(df)

    df["sic"] = pd.to_numeric(df["sic"], errors="coerce")

    df = df[
        ~(
                df["sic"].between(4900, 4999, inclusive="both") |
                df["sic"].between(6000, 6999, inclusive="both")
        )
    ].copy()

    n_after = len(df)

    print(f"Removed {n_before - n_after:,} observations with SIC 4900-4999 or 6000-6999.")



    df = classify_dataset(df)


    # put dummies first
    cols = ['Closure_Dummy', 'Pure_Layoff_Dummy', 'Borderline_Action_Dummy', 'Final_Flag']
    df = df[cols + [c for c in df.columns if c not in cols]]


    df.to_excel(OUTPUT_FILE, index=False)

    print("--- Classification Summary ---")
    print(f"Direct Layoffs: {df['Pure_Layoff_Dummy'].sum()}")
    print(f"Closures (Headline Only): {df['Closure_Dummy'].sum()}")
    print(f"Borderline (Needs Review): {df['Borderline_Action_Dummy'].sum()}")
    print(f"Total Unique Flags: {df['Final_Flag'].sum()}")
    print(f"Neither Layoff, Closure nor Borderline: {(df['Final_Flag'] == 0).sum()}")
    print(f"\nSuccess! Results saved to: {OUTPUT_FILE}")

except Exception as e:
    print(f"Error: {e}")



