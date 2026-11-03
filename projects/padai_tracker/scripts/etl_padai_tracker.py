#!/usr/bin/env python3
"""
ETL Pipeline for UK DSIT 'Public attitudes to data and AI: Tracker survey' (Waves 1-4)
Ingests wide Savanta Wave-on-Wave crosstab workbooks (CATI 77 sheets / CAWI 156 sheets),
resolves merged multi-tier demographic headers, and outputs a tidy long dataset.
"""

import re
import json
from pathlib import Path
import openpyxl
import pandas as pd


def unmerge_and_extract_headers(ws, demog_row: int, subgroup_row: int, max_col: int):
    """
    Resolves openpyxl MergedCellRange objects on the demographic and subgroup rows
    so every wave column inherits its parent Demographic_Category and Subgroup.
    """
    demog_vals = [ws.cell(demog_row, c).value for c in range(1, max_col + 1)]
    sub_vals = [ws.cell(subgroup_row, c).value for c in range(1, max_col + 1)]

    for rng in ws.merged_cells.ranges:
        top_val = ws.cell(rng.min_row, rng.min_col).value
        if rng.min_row <= demog_row <= rng.max_row:
            for c in range(rng.min_col, min(rng.max_col, max_col) + 1):
                demog_vals[c - 1] = top_val
        if rng.min_row <= subgroup_row <= rng.max_row:
            for c in range(rng.min_col, min(rng.max_col, max_col) + 1):
                sub_vals[c - 1] = top_val

    # Clean whitespace and forward-fill any unmerged residual gaps across wave blocks
    last_demog, last_sub = None, None
    for idx in range(2, max_col):
        if demog_vals[idx] is not None and str(demog_vals[idx]).strip():
            last_demog = str(demog_vals[idx]).strip()
        else:
            demog_vals[idx] = last_demog

        if sub_vals[idx] is not None and str(sub_vals[idx]).strip():
            last_sub = str(sub_vals[idx]).strip()
        else:
            sub_vals[idx] = last_sub

    return demog_vals, sub_vals


def parse_padai_workbook(excel_path: str | Path) -> pd.DataFrame:
    """
    Parses all Wave-on-Wave tables in the workbook into the standardized tidy schema:
    Table_Name | Question | Wave | Demographic_Category | Subgroup |
    Metric_Type | Value | Unweighted_N | Significance_Flag
    """
    # read_only=False is mandatory because Savanta's XML omits cell coordinates on Rows 6-7
    wb = openpyxl.load_workbook(excel_path, read_only=False, data_only=True)
    records = []

    for sheet_name in wb.sheetnames:
        if sheet_name.strip().lower() == "contents":
            continue

        ws = wb[sheet_name]
        max_row, max_col = ws.max_row, ws.max_column

        # 1. Dynamically locate the 'Significance Level: 95%' anchor row
        sig_row = None
        for r in range(1, min(20, max_row + 1)):
            cell_b = str(ws.cell(r, 2).value or "").strip()
            if cell_b.startswith("Significance Level"):
                sig_row = r
                break

        if sig_row is None:
            continue

        # Verify this is a standard Wave-on-Wave table (skips non-wave summary tables like Table 121)
        unw_label = str(ws.cell(sig_row + 1, 2).value or "").strip()
        first_wave_cell = str(ws.cell(sig_row - 1, 3).value or "").strip()
        if unw_label != "Unweighted Total" or not first_wave_cell.startswith("Wave"):
            continue

        table_name = str(ws.cell(3, 2).value or sheet_name).strip()
        question_text = str(ws.cell(4, 2).value or "").strip()

        demog_row = sig_row - 3
        subgroup_row = sig_row - 2
        wave_row = sig_row - 1
        unw_row = sig_row + 1
        data_start_row = sig_row + 3  # Row after weighted 'Total'

        # 2. Extract and unmerge column metadata (Columns 3..max_col)
        demog_vals, sub_vals = unmerge_and_extract_headers(ws, demog_row, subgroup_row, max_col)

        col_meta = {}
        for c in range(3, max_col + 1):
            wave_val = ws.cell(wave_row, c).value
            if wave_val is None or not str(wave_val).strip().startswith("Wave"):
                continue

            col_letter = str(ws.cell(sig_row, c).value or "").strip().lower()
            unw_n_raw = ws.cell(unw_row, c).value
            try:
                unw_n = int(float(unw_n_raw)) if unw_n_raw is not None else 0
            except (ValueError, TypeError):
                unw_n = 0

            demog_cat = demog_vals[c - 1] or "Total"
            subgroup = sub_vals[c - 1] or "Total"
            if demog_cat == "Total" and subgroup == "Wave":
                subgroup = "All Respondents"

            col_meta[c] = {
                "Wave": str(wave_val).strip(),
                "Demographic_Category": demog_cat,
                "Subgroup": subgroup,
                "Col_Letter": col_letter,
                "Unweighted_N": unw_n,
            }

        # 3. Iterate through response triplets in the sheet body
        r = data_start_row
        while r <= max_row:
            label_raw = ws.cell(r, 2).value
            if label_raw is None:
                r += 1
                continue

            label = str(label_raw).strip()
            if (
                label.startswith("Columns Tested:")
                or label.startswith("Prepared by")
                or label == ""
            ):
                break

            # Determine row block structure:
            # Standard items have: Row r (Weighted Count), Row r+1 (Proportion), Row r+2 (Sig Letters)
            # 'Mean' row has: Row r (Mean value), Row r+1 (Sig Letters)
            if label.lower() == "mean":
                val_row = r
                sig_flag_row = r + 1 if (r + 1 <= max_row and ws.cell(r + 1, 2).value is None) else None
                next_r = (sig_flag_row + 1) if sig_flag_row else (r + 1)
                metric_prefix = "Mean"
            else:
                val_row = r + 1 if (r + 1 <= max_row and ws.cell(r + 1, 2).value is None) else r
                sig_flag_row = (
                    r + 2
                    if (
                        val_row == r + 1
                        and r + 2 <= max_row
                        and ws.cell(r + 2, 2).value is None
                    )
                    else None
                )
                next_r = (sig_flag_row + 1) if sig_flag_row else (val_row + 1)
                metric_prefix = label

            for c, meta in col_meta.items():
                raw_val = ws.cell(val_row, c).value
                raw_cnt = ws.cell(r, c).value

                # When a question option was not asked in an earlier wave, both count and prop are 0
                if raw_val is None:
                    continue
                try:
                    val_num = float(raw_val)
                except (ValueError, TypeError):
                    continue

                # Distinguish true 0.0% from unasked wave placeholders (where weighted count == 0 and letters are empty)
                sig_str = ""
                if sig_flag_row is not None:
                    cell_sig = ws.cell(sig_flag_row, c).value
                    if cell_sig is not None:
                        sig_str = re.sub(r"[^a-zA-Z]", "", str(cell_sig)).lower()

                # Tag each record with both its column letter and any beaten column letters (e.g. "a:cd")
                formatted_sig = (
                    f"{meta['Col_Letter']}:{sig_str}" if sig_str else f"{meta['Col_Letter']}:"
                )

                records.append(
                    {
                        "Table_Name": table_name,
                        "Question": question_text,
                        "Wave": meta["Wave"],
                        "Demographic_Category": meta["Demographic_Category"],
                        "Subgroup": meta["Subgroup"],
                        "Metric_Type": metric_prefix,
                        "Value": round(val_num, 4),
                        "Unweighted_N": meta["Unweighted_N"],
                        "Significance_Flag": formatted_sig,
                    }
                )

            r = next_r

    df = pd.DataFrame(records)
    return df


def add_wave1_vs_wave4_significance(df: pd.DataFrame) -> pd.DataFrame:
    """
    Enriches the tidy dataframe with an explicit boolean `Sig_W1_W4` flag.
    In Savanta tables, Wave 1 = 'a' and Wave 4 = 'd' (for 4-wave demographics).
    A shift between Wave 1 and Wave 4 is statistically significant at p < .05 iff:
      - Wave 1's Significance_Flag contains 'd' (W1 > W4, p < .05), OR
      - Wave 4's Significance_Flag contains 'a' (W4 > W1, p < .05).
    """
    w1 = df[df["Wave"] == "Wave 1"].set_index(
        ["Table_Name", "Demographic_Category", "Subgroup", "Metric_Type"]
    )["Significance_Flag"]
    w4 = df[df["Wave"] == "Wave 4"].set_index(
        ["Table_Name", "Demographic_Category", "Subgroup", "Metric_Type"]
    )["Significance_Flag"]

    sig_pairs = set()
    for idx in w1.index.intersection(w4.index):
        flag1 = str(w1.loc[idx]).split(":")[-1]
        flag4 = str(w4.loc[idx]).split(":")[-1]
        if ("d" in flag1) or ("a" in flag4):
            sig_pairs.add(idx)

    df["Sig_W1_W4"] = df.set_index(
        ["Table_Name", "Demographic_Category", "Subgroup", "Metric_Type"]
    ).index.isin(sig_pairs)
    return df


if __name__ == "__main__":
    # Supports either the CAWI (156 sheets) or CATI (77 sheets) workbook
    input_candidates = [
        Path("P036672_RTA_CATI_Wtd_WoW_tables_20241115 (1).xlsx"),
        Path("P036672_RTA_CAWI_Wtd_WoW_tables_20241115.xlsx"),
    ]
    excel_file = next((p for p in input_candidates if p.exists()), input_candidates[0])

    print(f"[ETL] Ingesting workbook: {excel_file} ...")
    tidy_df = parse_padai_workbook(excel_file)
    tidy_df = add_wave1_vs_wave4_significance(tidy_df)

    out_dir = Path("data")
    out_dir.mkdir(exist_ok=True)

    # 1. Save full canonical CSV matching requested schema
    schema_cols = [
        "Table_Name",
        "Question",
        "Wave",
        "Demographic_Category",
        "Subgroup",
        "Metric_Type",
        "Value",
        "Unweighted_N",
        "Significance_Flag",
    ]
    tidy_df[schema_cols].to_csv(out_dir / "padai_tidy_full.csv", index=False)

    # 2. Export a curated subset for fast client-side OJS execution on GitHub Pages
    # Includes Summary Trust (Table 1), Likert Trust Distributions (Tables 3-12),
    # Data Attitudes (Table 22), AI Awareness (Table 82), and AI Impact (Table 84)
    core_tables = [
        "Table 1", "Table 2", "Table 3", "Table 4", "Table 5",
        "Table 6", "Table 7", "Table 8", "Table 9", "Table 10",
        "Table 11", "Table 12", "Table 22", "Table 82", "Table 84", "Table 145"
    ]
    dashboard_df = tidy_df[tidy_df["Table_Name"].isin(core_tables)].copy()
    dashboard_df.to_csv(out_dir / "padai_dashboard.csv", index=False)

    print(f"[ETL] Complete! Extracted {len(tidy_df):,} tidy rows across {tidy_df['Table_Name'].nunique()} tables.")