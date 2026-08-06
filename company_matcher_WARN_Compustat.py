import math
import re
import time
from collections import defaultdict
from functools import lru_cache
from config import DATA_FOLDER
import pandas as pd
from rapidfuzz import fuzz

#FILE TO MATCH THE (FIXED DATE) WARN DATA WITH COMPUSTAT
#BASED ONLY ON COMPANY NAMES
#ATTACH GVKEYS AND OTHER IDENTIFIERS TO ASSUMED MATCHES


# load data
WARN_FILE = DATA_FOLDER / "warn_data_date_fixed.csv"
COMPUSTAT_FILE = DATA_FOLDER / "compustat.xlsx"
OUTPUT_FILE = DATA_FOLDER / "warn_compustat_unfiltered.xlsx"


LOG_INTERVAL = 100


# COMPANY NAME STRIPPING AND CLEANING RULES

#getting rid of parenthesis () and []
PARENTHESES_PATTERN = re.compile(r"\([^()]*\)")

SQUARE_BRACKETS_PATTERN = re.compile(
    r"\[[^\[\]]*\]"
)

#removing share-class endings cl -a, cl - b
CLASS_SUFFIX_PATTERN = re.compile(
    r"\s*-\s*cl\s+[ab]\b",
    flags=re.IGNORECASE,
)

# removing all characters except letters, numbers and apostrophes
SPECIAL_CHARACTER_PATTERN = re.compile(
    r"[^\w\s']",
    flags=re.UNICODE,
)

SPACE_PATTERN = re.compile(r"\s+")

THE_PATTERN = re.compile(
    r"\bthe\b",
    flags=re.IGNORECASE,
)

#special function to keep the word "U.S"
US_PATTERN = re.compile(
    r"(?<!\w)u\s*\.\s*s\s*\.?(?!\w)",
    flags=re.IGNORECASE,
)

US_PLACEHOLDER = "specialustoken"



# removing operational/location descriptions and any identification
# and numbers immediately following them
OPERATIONAL_PATTERN = re.compile(
    r"""
    \b(?:
        plants? |
        sites? |
        factor(?:y|ies) |
        headquarters? |
        facilit(?:y|ies) |
        distribution |
        outlets? |
        stores?
    )\b
    (?:
        \s*
        (?:\#|no\.?|number)?
        \s*
        \d+[a-z]?
    )*
    """,
    flags=re.IGNORECASE | re.VERBOSE,
)


#standardising legal phrases in company names
LEGAL_STANDARDISATIONS = [
    # Professional limited liability company
    (
        re.compile(
            r"\bprofessional\s+limited\s+liability\s+compan(?:y|ies)\b",
            flags=re.IGNORECASE,
        ),
        "pllc",
    ),
    (
        re.compile(
            r"(?<!\w)p\s*\.\s*l\s*\.\s*l\s*\.\s*c\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "pllc",
    ),
    (
        re.compile(
            r"\bp\s+l\s+l\s+c\b",
            flags=re.IGNORECASE,
        ),
        "pllc",
    ),

    # Professional corporation
    (
        re.compile(
            r"\bprofessional\s+corporation\b",
            flags=re.IGNORECASE,
        ),
        "pc",
    ),
    (
        re.compile(
            r"(?<!\w)p\s*\.\s*c\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "pc",
    ),
    (
        re.compile(
            r"\bp\s+c\b",
            flags=re.IGNORECASE,
        ),
        "pc",
    ),

    # Public limited company
    (
        re.compile(
            r"\bpublic\s+limited\s+compan(?:y|ies)\b",
            flags=re.IGNORECASE,
        ),
        "plc",
    ),
    (
        re.compile(
            r"(?<!\w)p\s*\.\s*l\s*\.\s*c\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "plc",
    ),
    (
        re.compile(
            r"\bp\s+l\s+c\b",
            flags=re.IGNORECASE,
        ),
        "plc",
    ),

    # Limited liability company
    (
        re.compile(
            r"\blimited\s+liability\s+compan(?:y|ies)\b",
            flags=re.IGNORECASE,
        ),
        "llc",
    ),
    (
        re.compile(
            r"(?<!\w)l\s*\.\s*l\s*\.\s*c\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "llc",
    ),
    (
        re.compile(
            r"\bl\s+l\s+c\b",
            flags=re.IGNORECASE,
        ),
        "llc",
    ),

    # Limited liability partnership
    (
        re.compile(
            r"\blimited\s+liability\s+partnership\b",
            flags=re.IGNORECASE,
        ),
        "llp",
    ),
    (
        re.compile(
            r"(?<!\w)l\s*\.\s*l\s*\.\s*p\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "llp",
    ),
    (
        re.compile(
            r"\bl\s+l\s+p\b",
            flags=re.IGNORECASE,
        ),
        "llp",
    ),

    # Limited partnership
    (
        re.compile(
            r"\blimited\s+partnership\b",
            flags=re.IGNORECASE,
        ),
        "lp",
    ),
    (
        re.compile(
            r"(?<!\w)l\s*\.\s*p\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "lp",
    ),
    (
        re.compile(
            r"\bl\s+p\b",
            flags=re.IGNORECASE,
        ),
        "lp",
    ),

    # Incorporated
    (
        re.compile(
            r"\bincorporated\b",
            flags=re.IGNORECASE,
        ),
        "inc",
    ),
    (
        re.compile(
            r"(?<!\w)inc\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "inc",
    ),

    # Corporation
    (
        re.compile(
            r"\bcorporations?\b",
            flags=re.IGNORECASE,
        ),
        "corp",
    ),
    (
        re.compile(
            r"(?<!\w)corp\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "corp",
    ),

    # Company or companies
    (
        re.compile(
            r"\bcompan(?:y|ies)\b",
            flags=re.IGNORECASE,
        ),
        "co",
    ),
    (
        re.compile(
            r"(?<!\w)co\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "co",
    ),

    # Limited
    (
        re.compile(
            r"\blimited\b",
            flags=re.IGNORECASE,
        ),
        "ltd",
    ),
    (
        re.compile(
            r"(?<!\w)ltd\s*\.?(?!\w)",
            flags=re.IGNORECASE,
        ),
        "ltd",
    ),
]


# legal terms remain in cleaned names for review purposes but are removed from the
# representation used for fallback matching and scoring.
LEGAL_ABBREVIATIONS = {
    "co",
    "corp",
    "inc",
    "ltd",
    "llc",
    "plc",
    "llp",
    "lp",
    "pllc",
    "pc",
    "sa",
    "ag",
    "nv",
    "bv",
    "gmbh",
}


#low information words are treated with lower priority not to distort matching score
LOW_INFORMATION_WORDS = {
    "holdings",
    "hldgs",
    "group",
    "industries",
    "enterprises",
    "companies",
    "solutions",
}


# cautious openings appear in a lot of names so they are taken out before matching rule applied
CAUTIOUS_OPENING_PHRASES = [
    ("house", "of"),
    ("united", "states"),
    ("U.S.",),
    ("us",),
    ("american",),
    ("international",),
    ("central",),
]


# APPLY CLEANING AND STRIPPING FUNCTIONS

#remove parentheses
def remove_bracketed_text(text):

    previous_text = None

    while text != previous_text:
        previous_text = text

        text = PARENTHESES_PATTERN.sub(
            " ",
            text,
        )

        text = SQUARE_BRACKETS_PATTERN.sub(
            " ",
            text,
        )

    return text


#cuts company names to four words for efficiency reasons
@lru_cache(maxsize=None)
def clean_company_name(value):

    if value is None:
        return ""

    try:
        if pd.isna(value):
            return ""
    except (TypeError, ValueError):
        pass

    text = str(value).casefold()

    #standardise apostrophes
    text = text.replace("’", "'")
    text = text.replace("`", "'")

    #remove parenthetical and square-bracket information
    text = remove_bracketed_text(text)

    #remove share-class suffixes before removing hyphens.
    text = CLASS_SUFFIX_PATTERN.sub(
        " ",
        text,
    )

    #special rule for the word "U.S."
    text = US_PATTERN.sub(
        f" {US_PLACEHOLDER} ",
        text,
    )

    #standardise legal terms.
    for pattern, replacement in LEGAL_STANDARDISATIONS:
        text = pattern.sub(
            f" {replacement} ",
            text,
        )

    #remove operational/location words and following numbers.
    text = OPERATIONAL_PATTERN.sub(
        " ",
        text,
    )

    #remove special characters, including ampersands.
    text = SPECIAL_CHARACTER_PATTERN.sub(
        " ",
        text,
    )

    #remove the word "the".
    text = THE_PATTERN.sub(
        " ",
        text,
    )

    #normalise whitespace.
    text = SPACE_PATTERN.sub(
        " ",
        text,
    ).strip(" '_")

    words = text.split()

    # restore U.S. with periods.
    words = [
        "U.S." if word == US_PLACEHOLDER else word
        for word in words
    ]

    # raximum of four cleaned words.
    words = words[:4]

    return " ".join(words)

#creating word tuple to find and compare full words against each other
def get_words(cleaned_name):
    """Convert a cleaned company name into a word tuple."""
    return tuple(cleaned_name.split())


def remove_legal_words(words):
    """Remove standardised legal terms."""
    return tuple(
        word
        for word in words
        if word not in LEGAL_ABBREVIATIONS
    )


def remove_cautious_opening(words):
    """
    Remove one cautious opening phrase when it occurs at the
    beginning of the matching representation.

    The cautious opening remains in the cleaned-name output.
    """
    words = tuple(words)

    for phrase in CAUTIOUS_OPENING_PHRASES:
        phrase_length = len(phrase)

        if words[:phrase_length] == phrase:
            return words[phrase_length:]

    return words


def create_matching_words(cleaned_name):
    """
    Create the word sequence used for fallback matching:

      1. Start with the cleaned name.
      2. Remove legal terms.
      3. Remove a cautious opening.
      4. Match the consecutive words that remain.
    """
    words = get_words(cleaned_name)
    words = remove_legal_words(words)
    words = remove_cautious_opening(words)

    return tuple(words)


def low_information_remainder(words):
    """
    Return True when every supplied word is low-information.

    An empty remainder also returns True.
    """
    return all(
        word in LOW_INFORMATION_WORDS
        for word in words
    )


#logic for treating cases where only the first two word matches
def positional_word_score(
    warn_word,
    compustat_word,
):
    """
    Score one unmatched position.

    Missing values and low-information words are treated as
    neutral when the difference is only low-information content.
    """
    # Both positions are absent.
    if warn_word is None and compustat_word is None:
        return 100.0

    # One side is absent.
    if warn_word is None:
        if compustat_word in LOW_INFORMATION_WORDS:
            return 100.0
        return 0.0

    if compustat_word is None:
        if warn_word in LOW_INFORMATION_WORDS:
            return 100.0
        return 0.0

    # Exact word match.
    if warn_word == compustat_word:
        return 100.0

    # Different low-information words are allowed.
    if (
        warn_word in LOW_INFORMATION_WORDS
        and compustat_word in LOW_INFORMATION_WORDS
    ):
        return 100.0

    # Otherwise compare the two words by letters.
    return float(
        fuzz.ratio(
            warn_word,
            compustat_word,
        )
    )


def two_word_remainder_score(
    warn_matching_words,
    compustat_matching_words,
):
    """
    Score only the third and fourth words after the first two
    words have matched.

    Third word:  85% weight
    Fourth word: 15% weight
    """
    warn_third = (
        warn_matching_words[2]
        if len(warn_matching_words) > 2
        else None
    )

    compustat_third = (
        compustat_matching_words[2]
        if len(compustat_matching_words) > 2
        else None
    )

    warn_fourth = (
        warn_matching_words[3]
        if len(warn_matching_words) > 3
        else None
    )

    compustat_fourth = (
        compustat_matching_words[3]
        if len(compustat_matching_words) > 3
        else None
    )

    third_word_score = positional_word_score(
        warn_third,
        compustat_third,
    )

    fourth_word_score = positional_word_score(
        warn_fourth,
        compustat_fourth,
    )

    final_score = (
        0.85 * third_word_score
        + 0.15 * fourth_word_score
    )

    return round(final_score, 2)


#rules for when only the first words matches in the company names

def one_word_match_allowed(
    warn_matching_words,
    compustat_matching_words,
):
    """
    Allow a one-word review match only when:

      1. The first words are identical.
      2. Every remaining word on each side is either absent or
         included in LOW_INFORMATION_WORDS.

    Different low-information words are allowed.
    """
    if not warn_matching_words:
        return False

    if not compustat_matching_words:
        return False

    if (
        warn_matching_words[0]
        != compustat_matching_words[0]
    ):
        return False

    warn_remainder = warn_matching_words[1:]
    compustat_remainder = compustat_matching_words[1:]

    return (
        low_information_remainder(warn_remainder)
        and low_information_remainder(
            compustat_remainder
        )
    )


def one_word_remainder_score(
    warn_matching_words,
    compustat_matching_words,
):
    """
    Provide an audit score for an allowed one-word review.

    The score compares only the low-information remainder.
    It does not affect whether the match is allowed.
    """
    warn_remainder = warn_matching_words[1:]
    compustat_remainder = compustat_matching_words[1:]

    # If one or both sides have no remaining meaningful words,
    # the permitted low-information difference is treated as
    # structurally acceptable.
    if (
        not warn_remainder
        or not compustat_remainder
    ):
        return 100.0

    return round(
        fuzz.ratio(
            " ".join(warn_remainder),
            " ".join(compustat_remainder),
        ),
        2,
    )


#keeping track of time during execution
def format_time(seconds):
    """Convert seconds into a readable duration."""
    if seconds is None or not math.isfinite(seconds):
        return "calculating..."

    seconds = max(0, int(seconds))

    hours, remainder = divmod(
        seconds,
        3600,
    )

    minutes, seconds = divmod(
        remainder,
        60,
    )

    if hours > 0:
        return (
            f"{hours}h "
            f"{minutes:02d}m "
            f"{seconds:02d}s"
        )

    if minutes > 0:
        return f"{minutes}m {seconds:02d}s"

    return f"{seconds}s"


#reading in files
print("Reading WARN workbook...", flush=True)

warn_data = pd.read_csv(
    WARN_FILE,
    keep_default_na=False,
)
#rename the messy WARN columns
warn_data.rename(
    columns={
        "State": "Region",
        "Company": "State",
        "City": "Company",
        "Workers": "City",
        "WARN Date": "Address",
        # "Effective Date" and all remaining columns stay unchanged
    },
    inplace=True,
)

print("Reading Compustat workbook...", flush=True)

compustat_data = pd.read_excel(
    COMPUSTAT_FILE,
    sheet_name="compustat",
    engine="openpyxl",
    dtype={
        "gvkey": str,
        "Ticker Symbol": str,
        "SIC": str,
    },
)

if "Company" not in warn_data.columns:
    raise KeyError(
        "The WARN workbook does not contain a 'Company' column."
    )

if "Company Name" not in compustat_data.columns:
    raise KeyError(
        "The Compustat workbook does not contain a "
        "'Company Name' column."
    )

if "SIC" not in compustat_data.columns:
    raise KeyError(
        "The Compustat workbook does not contain an 'SIC' column."
    )


#excluding unwanted companies from financial and utilities industries for a cleaner matching
sic_numeric = pd.to_numeric(
    compustat_data["SIC"],
    errors="coerce",
)

excluded_sic = (
    sic_numeric.between(
        4900,
        4999,
        inclusive="both",
    )
    | sic_numeric.between(
        6000,
        6999,
        inclusive="both",
    )
)

rows_before_sic_filter = len(compustat_data)
rows_excluded_by_sic = int(excluded_sic.sum())

compustat_data = (
    compustat_data.loc[~excluded_sic]
    .copy()
    .reset_index(drop=True)
)

print(
    f"Excluded {rows_excluded_by_sic:,} Compustat rows "
    f"with SIC 4900-4999 or 6000-6999.",
    flush=True,
)

print(
    f"Compustat rows remaining: "
    f"{len(compustat_data):,} "
    f"of {rows_before_sic_filter:,}.",
    flush=True,
)


#cleaning compustat company names
print("Cleaning Compustat company names...", flush=True)

compustat_data = compustat_data[
    compustat_data["Company Name"].notna()
].copy()

compustat_data["Compustat_Clean_Name"] = (
    compustat_data["Company Name"].map(
        clean_company_name
    )
)

compustat_data = compustat_data[
    compustat_data["Compustat_Clean_Name"] != ""
].reset_index(drop=True)

compustat_names = (
    compustat_data["Company Name"]
    .astype(str)
    .tolist()
)

compustat_clean_names = (
    compustat_data["Compustat_Clean_Name"]
    .tolist()
)

compustat_clean_words = [
    get_words(name)
    for name in compustat_clean_names
]

compustat_substantive_words = [
    remove_legal_words(words)
    for words in compustat_clean_words
]

compustat_matching_words = [
    create_matching_words(name)
    for name in compustat_clean_names
]

compustat_gvkeys = (
    compustat_data["gvkey"].tolist()
    if "gvkey" in compustat_data.columns
    else [None] * len(compustat_data)
)

compustat_tickers = (
    compustat_data["Ticker Symbol"].tolist()
    if "Ticker Symbol" in compustat_data.columns
    else [None] * len(compustat_data)
)

compustat_sic = compustat_data[
    "SIC"
].tolist()


# building matching indexes

print("Building matching indexes...", flush=True)

#complete cleaned names, including legal terms
exact_name_index = defaultdict(list)

#complete substantive names, excluding legal terms
substantive_name_index = defaultdict(list)

#prefixes after excluding legal terms and cautious openings
matching_prefix_index = {
    1: defaultdict(set),
    2: defaultdict(set),
    3: defaultdict(set),
    4: defaultdict(set),
}

for company_index in range(len(compustat_data)):
    clean_words = compustat_clean_words[
        company_index
    ]

    substantive_words = (
        compustat_substantive_words[
            company_index
        ]
    )

    matching_words = compustat_matching_words[
        company_index
    ]

    if clean_words:
        exact_name_index[
            clean_words
        ].append(company_index)

    if substantive_words:
        substantive_name_index[
            substantive_words
        ].append(company_index)

    maximum_prefix_length = min(
        4,
        len(matching_words),
    )

    for prefix_length in range(
        1,
        maximum_prefix_length + 1,
    ):
        prefix = matching_words[:prefix_length]

        matching_prefix_index[
            prefix_length
        ][prefix].add(company_index)


# output helper functions
def candidate_result(
    candidate_index,
    similarity,
    category,
    words_matched,
    match_rule,
):
    """Return the output columns for one selected candidate."""
    return (
        compustat_names[candidate_index],
        compustat_clean_names[candidate_index],
        " ".join(
            compustat_matching_words[
                candidate_index
            ]
        ),
        compustat_gvkeys[candidate_index],
        compustat_tickers[candidate_index],
        compustat_sic[candidate_index],
        similarity,
        category,
        words_matched,
        match_rule,
    )


def no_match_result(reason):
    """Return an empty candidate result."""
    return (
        None,
        None,
        None,
        None,
        None,
        None,
        0.0,
        "NO MATCH",
        0,
        reason,
    )


#forcing independent matching

@lru_cache(maxsize=None)
def match_one_company(warn_clean_name):
    """
    Match one WARN company independently.

    Matching order:

      1. Complete cleaned exact match.
      2. Complete legal-equivalent exact match.
      3. Four matching words -> EXACT.
      4. Three matching words -> EXACT.
      5. Two matching words -> REVIEW, with weighted remainder.
      6. One matching word -> REVIEW only when all remaining
         words on both sides are low-information.
      7. Otherwise -> NO MATCH.
    """
    if not warn_clean_name:
        return no_match_result(
            "Empty WARN name"
        )

    warn_clean_words = get_words(
        warn_clean_name
    )

    if not warn_clean_words:
        return no_match_result(
            "Empty WARN name"
        )

    exact_candidates = exact_name_index.get(
        warn_clean_words,
        [],
    )

    if exact_candidates:
        selected_index = exact_candidates[0]

        return candidate_result(
            candidate_index=selected_index,
            similarity=100.0,
            category="EXACT",
            words_matched=len(
                warn_clean_words
            ),
            match_rule=(
                "Complete cleaned names are identical"
            ),
        )


    warn_substantive_words = remove_legal_words(
        warn_clean_words
    )

    legal_candidates = (
        substantive_name_index.get(
            warn_substantive_words,
            [],
        )
        if warn_substantive_words
        else []
    )

    if legal_candidates:
        selected_index = max(
            legal_candidates,
            key=lambda index: (
                fuzz.ratio(
                    warn_clean_name,
                    compustat_clean_names[index],
                ),
                -abs(
                    len(warn_clean_words)
                    - len(
                        compustat_clean_words[index]
                    )
                ),
                -index,
            ),
        )

        return candidate_result(
            candidate_index=selected_index,
            similarity=100.0,
            category="EXACT",
            words_matched=len(
                warn_substantive_words
            ),
            match_rule=(
                "Complete substantive names are identical; "
                "differences are legal terms only"
            ),
        )



    warn_matching_words = (
        create_matching_words(
            warn_clean_name
        )
    )

    if not warn_matching_words:
        return no_match_result(
            "No words remain after legal and cautious exclusions"
        )




    if len(warn_matching_words) >= 4:
        four_word_prefix = (
            warn_matching_words[:4]
        )

        four_word_candidates = (
            matching_prefix_index[4].get(
                four_word_prefix,
                set(),
            )
        )

        if four_word_candidates:
            selected_index = max(
                four_word_candidates,
                key=lambda index: (
                    fuzz.ratio(
                        " ".join(warn_matching_words),
                        " ".join(
                            compustat_matching_words[
                                index
                            ]
                        ),
                    ),
                    -index,
                ),
            )

            return candidate_result(
                candidate_index=selected_index,
                similarity=100.0,
                category="EXACT",
                words_matched=4,
                match_rule=(
                    "First four consecutive matching words "
                    "are identical"
                ),
            )



    if len(warn_matching_words) >= 3:
        three_word_prefix = (
            warn_matching_words[:3]
        )

        three_word_candidates = (
            matching_prefix_index[3].get(
                three_word_prefix,
                set(),
            )
        )

        if three_word_candidates:
            selected_index = max(
                three_word_candidates,
                key=lambda index: (
                    fuzz.ratio(
                        " ".join(warn_matching_words),
                        " ".join(
                            compustat_matching_words[
                                index
                            ]
                        ),
                    ),
                    -index,
                ),
            )

            return candidate_result(
                candidate_index=selected_index,
                similarity=100.0,
                category="EXACT",
                words_matched=3,
                match_rule=(
                    "First three consecutive matching words "
                    "are identical"
                ),
            )



    if len(warn_matching_words) >= 2:
        two_word_prefix = (
            warn_matching_words[:2]
        )

        two_word_candidates = (
            matching_prefix_index[2].get(
                two_word_prefix,
                set(),
            )
        )

        if two_word_candidates:
            scored_candidates = [
                (
                    index,
                    two_word_remainder_score(
                        warn_matching_words,
                        compustat_matching_words[
                            index
                        ],
                    ),
                )
                for index in two_word_candidates
            ]

            selected_index, selected_score = max(
                scored_candidates,
                key=lambda item: (
                    item[1],
                    -abs(
                        len(warn_matching_words)
                        - len(
                            compustat_matching_words[
                                item[0]
                            ]
                        )
                    ),
                    -item[0],
                ),
            )

            return candidate_result(
                candidate_index=selected_index,
                similarity=selected_score,
                category="REVIEW",
                words_matched=2,
                match_rule=(
                    "First two consecutive matching words "
                    "are identical; score uses third word "
                    "at 85% and fourth word at 15%"
                ),
            )


    one_word_prefix = (
        warn_matching_words[:1]
    )

    one_word_candidates = (
        matching_prefix_index[1].get(
            one_word_prefix,
            set(),
        )
    )

    permitted_one_word_candidates = [
        index
        for index in one_word_candidates
        if one_word_match_allowed(
            warn_matching_words,
            compustat_matching_words[index],
        )
    ]

    if permitted_one_word_candidates:
        scored_candidates = [
            (
                index,
                one_word_remainder_score(
                    warn_matching_words,
                    compustat_matching_words[
                        index
                    ],
                ),
            )
            for index in permitted_one_word_candidates
        ]

        selected_index, selected_score = max(
            scored_candidates,
            key=lambda item: (
                item[1],
                -abs(
                    len(warn_matching_words)
                    - len(
                        compustat_matching_words[
                            item[0]
                        ]
                    )
                ),
                -item[0],
            ),
        )

        return candidate_result(
            candidate_index=selected_index,
            similarity=selected_score,
            category="REVIEW",
            words_matched=1,
            match_rule=(
                "First matching word is identical; all "
                "remaining words are absent or low-information"
            ),
        )

    return no_match_result(
        "No permitted four-, three-, two-, or one-word match"
    )


# clean and match

print("Cleaning WARN company names...", flush=True)

warn_clean_names = (
    warn_data["Company"].map(
        clean_company_name
    )
)

warn_matching_names = (
    warn_clean_names.map(
        lambda name: " ".join(
            create_matching_words(name)
        )
    )
)

total_rows = len(warn_data)
results = []

print()
print("=" * 75)
print("STARTING COMPANY MATCHING")
print("=" * 75)
print(f"WARN rows:           {total_rows:,}")
print(f"Compustat companies: {len(compustat_data):,}")
print("=" * 75)
print()

start_time = time.perf_counter()

for row_number, warn_clean_name in enumerate(
    warn_clean_names,
    start=1,
):
    results.append(
        match_one_company(
            warn_clean_name
        )
    )

    should_log = (
        row_number == 1
        or row_number % LOG_INTERVAL == 0
        or row_number == total_rows
    )

    if should_log:
        elapsed_seconds = (
            time.perf_counter()
            - start_time
        )

        rows_per_second = (
            row_number / elapsed_seconds
            if elapsed_seconds > 0
            else 0
        )

        remaining_rows = (
            total_rows - row_number
        )

        estimated_time_remaining = (
            remaining_rows / rows_per_second
            if rows_per_second > 0
            else None
        )

        percentage = (
            row_number / total_rows
        ) * 100

        print(
            f"[{percentage:6.2f}%] "
            f"{row_number:,}/{total_rows:,} rows | "
            f"{rows_per_second:,.2f} rows/sec | "
            f"Elapsed: {format_time(elapsed_seconds)} | "
            f"ETA: {format_time(estimated_time_remaining)}",
            flush=True,
        )

matching_time = (
    time.perf_counter()
    - start_time
)


# CREATE UNFILTERED OUTPUT

result_columns = pd.DataFrame(
    results,
    columns=[
        "Compustat_Best_Match",
        "Compustat_Clean_Name",
        "Compustat_Matching_Name",
        "Compustat_gvkey",
        "Compustat_Ticker",
        "Compustat_SIC",
        "Similarity_Percent",
        "Match_Category",
        "Whole_Words_Matched",
        "Match_Rule",
    ],
    index=warn_data.index,
)

output_data = warn_data.copy()

output_data["WARN_Clean_Name"] = (
    warn_clean_names
)

output_data["WARN_Matching_Name"] = (
    warn_matching_names
)

output_data = pd.concat(
    [
        output_data,
        result_columns,
    ],
    axis=1,
)

# COUNT UNIQUE MATCHES

# Standardise gvkey only for counting.
output_data["Compustat_gvkey"] = (
    output_data["Compustat_gvkey"]
    .astype("string")
    .str.strip()
)

valid_gvkey = (
    output_data["Compustat_gvkey"].notna()
    & output_data["Compustat_gvkey"].ne("")
    & output_data["Compustat_gvkey"].ne("<NA>")
    & output_data[
        "Compustat_gvkey"
    ].str.lower().ne("nan")
)

matched_with_gvkey = (
    output_data.loc[valid_gvkey]
    .copy()
)


def classify_unfiltered_gvkey(categories):
    """
    Assign one category to each unique gvkey.

    If a gvkey appears in both EXACT and REVIEW observations,
    classify it as EXACT to prevent double-counting.
    """
    category_set = set(categories)

    if "EXACT" in category_set:
        return "EXACT"

    if "REVIEW" in category_set:
        return "REVIEW"

    return "OTHER"


gvkey_categories = (
    matched_with_gvkey
    .groupby("Compustat_gvkey")[
        "Match_Category"
    ]
    .apply(classify_unfiltered_gvkey)
)

unique_exact_gvkeys = int(
    (gvkey_categories == "EXACT").sum()
)

unique_review_only_gvkeys = int(
    (gvkey_categories == "REVIEW").sum()
)

total_unique_matched_gvkeys = int(
    gvkey_categories.index.nunique()
)

# NO MATCH rows do not have a gvkey. Count unique unmatched
# cleaned WARN names instead.
no_match_data = output_data[
    output_data["Match_Category"] == "NO MATCH"
].copy()

unique_unmatched_warn_names = int(
    no_match_data.loc[
        no_match_data["WARN_Clean_Name"]
        .astype(str)
        .str.strip()
        .ne(""),
        "WARN_Clean_Name",
    ].nunique()
)


# Create a gvkey-level audit table.
gvkey_summary = (
    gvkey_categories
    .rename("Unfiltered_Gvkey_Category")
    .reset_index()
)

gvkey_row_counts = (
    matched_with_gvkey
    .groupby("Compustat_gvkey")
    .size()
    .rename("Matched_WARN_Rows")
    .reset_index()
)

gvkey_summary = gvkey_summary.merge(
    gvkey_row_counts,
    on="Compustat_gvkey",
    how="left",
)



# SUMMARY

exact_rows = int(
    (
        output_data["Match_Category"]
        == "EXACT"
    ).sum()
)

review_rows = int(
    (
        output_data["Match_Category"]
        == "REVIEW"
    ).sum()
)

no_match_rows = int(
    (
        output_data["Match_Category"]
        == "NO MATCH"
    ).sum()
)

one_word_reviews = int(
    (
        (output_data["Match_Category"] == "REVIEW")
        & (
            output_data["Whole_Words_Matched"]
            == 1
        )
    ).sum()
)

two_word_reviews = int(
    (
        (output_data["Match_Category"] == "REVIEW")
        & (
            output_data["Whole_Words_Matched"]
            == 2
        )
    ).sum()
)

three_word_exact = int(
    (
        (output_data["Match_Category"] == "EXACT")
        & (
            output_data["Whole_Words_Matched"]
            == 3
        )
    ).sum()
)

four_word_exact = int(
    (
        (output_data["Match_Category"] == "EXACT")
        & (
            output_data["Whole_Words_Matched"]
            == 4
        )
    ).sum()
)

matching_summary = pd.DataFrame(
    {
        "Measure": [
            "Total WARN rows",
            "EXACT WARN rows",
            "REVIEW WARN rows",
            "NO MATCH WARN rows",
            "One-word REVIEW rows",
            "Two-word REVIEW rows",
            "Three-word EXACT rows",
            "Four-word EXACT rows",
            "Unique gvkeys classified as EXACT",
            "Unique gvkeys classified as REVIEW only",
            "Total unique matched gvkeys",
            "Unique unmatched WARN cleaned names",
            "Compustat rows excluded by SIC",
            "Compustat rows remaining after SIC filter",
        ],
        "Count": [
            len(output_data),
            exact_rows,
            review_rows,
            no_match_rows,
            one_word_reviews,
            two_word_reviews,
            three_word_exact,
            four_word_exact,
            unique_exact_gvkeys,
            unique_review_only_gvkeys,
            total_unique_matched_gvkeys,
            unique_unmatched_warn_names,
            rows_excluded_by_sic,
            len(compustat_data),
        ],
    }
)

# SAVE EVERYTHING

print()
print("Writing unfiltered output...", flush=True)

excel_start_time = time.perf_counter()

with pd.ExcelWriter(
    OUTPUT_FILE,
    engine="openpyxl",
) as writer:
    output_data.to_excel(
        writer,
        sheet_name="warn_matched",
        index=False,
    )

    matching_summary.to_excel(
        writer,
        sheet_name="matching_summary",
        index=False,
    )

    gvkey_summary.to_excel(
        writer,
        sheet_name="gvkey_summary",
        index=False,
    )

excel_time = (
    time.perf_counter()
    - excel_start_time
)

total_time = (
    time.perf_counter()
    - start_time
)

print()
print("=" * 75)
print("UNFILTERED MATCHING COMPLETED")
print("=" * 75)
print(f"Exact rows:               {exact_rows:,}")
print(f"Review rows:              {review_rows:,}")
print(f"No-match rows:            {no_match_rows:,}")
print(f"One-word reviews:         {one_word_reviews:,}")
print(f"Two-word reviews:         {two_word_reviews:,}")
print(f"Three-word exact matches: {three_word_exact:,}")
print(f"Four-word exact matches:  {four_word_exact:,}")
print("-" * 75)
print(f"Matching time:            {format_time(matching_time)}")
print(f"Excel writing time:       {format_time(excel_time)}")
print(f"Total running time:       {format_time(total_time)}")
print(f"Output file:              {OUTPUT_FILE}")
print("=" * 75)

print("-" * 75)
print("UNFILTERED UNIQUE GVKEY COUNTS")
print("-" * 75)
print(
    f"Unique exact gvkeys:       "
    f"{unique_exact_gvkeys:,}"
)
print(
    f"Unique review-only gvkeys: "
    f"{unique_review_only_gvkeys:,}"
)
print(
    f"Total matched gvkeys:      "
    f"{total_unique_matched_gvkeys:,}"
)
print(
    f"Unique unmatched names:    "
    f"{unique_unmatched_warn_names:,}"
)
print("=" * 75)