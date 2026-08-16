###############################################################################
##  NES-LTER Zooplankton Tow Metadata v3 
##            column-by-column QA/QC
##  Project: nes-lter-zooplankton-tow-meta-v3
##  Script:  06_tow_metadata_qaqc.R
##
##  Reads the published-ready CSV written by 04, checks every column against
##  expected type / range / vocabulary / NA rules, prints a per-column report,
##  and exports a data-dictionary CSV (column name + type + summary).
##
##  Nothing here modifies the data.
###############################################################################

library(tidyverse)
library(here)
library(glue)

## ------------------------------------------ ##
#   Load the most recent published-ready CSV -----
## ------------------------------------------ ##
tow_files <- list.files(here("data", "processed"),
                        pattern = "^nes-lter-zooplankton-tow-metadata-v3-.*\\.csv$",
                        full.names = TRUE)
if (length(tow_files) == 0) stop("No nes-lter-zooplankton-tow-metadata-v3-*.csv in data/processed/")
tow_dates <- as.Date(str_extract(basename(tow_files), "\\d{8}"), "%Y%m%d")
latest    <- tow_files[which.max(tow_dates)]
message("QA on: ", basename(latest))

# read everything as-is; don't let readr guess away a problem we want to catch
tow <- read_csv(latest, show_col_types = FALSE)

# collector for issues so the run ends with one summary table
issues <- tibble(check = character(), detail = character(), n = integer())
flag_issue <- function(check, detail, n) {
  if (n > 0) issues <<- add_row(issues, check = check, detail = detail, n = n)
}

## ============================================================================
## 0) EXPECTED SCHEMA  — edit here if columns change
## ============================================================================
# type: "chr","dbl","int","dttm"; every published column should be listed so a
# new/renamed/dropped column is caught immediately.
schema <- tribble(
  ~column,                 ~type,   ~required,
  "cruise",                "chr",   TRUE,
  "station",               "chr",   TRUE,
  "cast",                  "chr",   TRUE,
  "sample_name",           "chr",   TRUE,
  "net_type",              "chr",   TRUE,
  "datetime_UTC_start",    "dttm",  TRUE,
  "datetime_UTC_end",      "dttm",  FALSE,
  "latitude_start",        "dbl",   TRUE,
  "longitude_start",       "dbl",   TRUE,
  "latitude_end",          "dbl",   FALSE,
  "longitude_end",         "dbl",   FALSE,
  "depth_bottom",          "dbl",   FALSE,
  "depth_target",          "dbl",   FALSE,
  "depth_TDR",             "dbl",   FALSE,
  "depth_PX",              "dbl",   FALSE,
  "net_max_depth",         "dbl",   FALSE,
  "avg_angle",             "dbl",   FALSE,
  "max_wire_out",          "dbl",   FALSE,
  "wire_rate_out",         "dbl",   FALSE,
  "wire_rate_in",          "dbl",   FALSE,
  "STW_start",             "dbl",   FALSE,
  "SOG_start",             "dbl",   FALSE,
  "STW_end",               "dbl",   FALSE,
  "SOG_end",               "dbl",   FALSE,
  "flowmeter_sn_335",      "dbl",   FALSE,
  "flow_start_335",        "dbl",   FALSE,
  "flow_end_335",          "dbl",   FALSE,
  "tot_flow_counts_335",   "dbl",   FALSE,
  "vol_filtered_335",      "dbl",   FALSE,
  "NOAA_335",              "chr",   FALSE,
  "DNA_335",               "chr",   FALSE,
  "flowmeter_sn_150",      "dbl",   FALSE,
  "flow_start_150",        "dbl",   FALSE,
  "flow_end_150",          "dbl",   FALSE,
  "tot_flow_counts_150",   "dbl",   FALSE,
  "vol_filtered_150",      "dbl",   FALSE,
  "morph_ID_150",          "chr",   FALSE,
  "DNA_150",               "chr",   FALSE,
  "size_fract_150",        "chr",   FALSE,
  "taxa_pick_150",         "chr",   FALSE,
  "size_fract_20",         "chr",   FALSE,
  "haul_factor_10m2_335",  "dbl",   FALSE,
  "haul_factor_10m2_150",  "dbl",   FALSE,
  "haul_factor_100m3_335", "dbl",   FALSE,
  "haul_factor_100m3_150", "dbl",   FALSE,
  "comments",              "chr",   FALSE,
  "primary_flag",          "dbl",   TRUE,
  "secondary_flag",        "chr",   FALSE
)

# controlled Y/N columns
yn_cols <- c("NOAA_335","DNA_335","morph_ID_150","DNA_150",
             "size_fract_150","taxa_pick_150","size_fract_20")

# valid QARTOD primary flags
valid_flags <- c(1, 3, 4, 9)

## ============================================================================
## 1) STRUCTURE — columns present, no extras, types as expected
## ============================================================================
cat("\n========== 1. STRUCTURE ==========\n")

missing_cols <- setdiff(schema$column, names(tow))
extra_cols   <- setdiff(names(tow), schema$column)
if (length(missing_cols)) cat("MISSING columns:", paste(missing_cols, collapse = ", "), "\n")
if (length(extra_cols))   cat("UNEXPECTED columns:", paste(extra_cols, collapse = ", "), "\n")
if (!length(missing_cols) && !length(extra_cols)) cat("All expected columns present, no extras.\n")
flag_issue("structure", "missing columns", length(missing_cols))
flag_issue("structure", "unexpected columns", length(extra_cols))

type_map <- c(character = "chr", numeric = "dbl", integer = "int",
              POSIXct = "dttm", Date = "date", logical = "lgl")
actual_types <- tibble(column = names(tow),
                       actual = map_chr(tow, ~ recode(class(.x)[1], !!!type_map, .default = class(.x)[1])))
type_check <- schema %>%
  inner_join(actual_types, by = "column") %>%
  mutate(ok = type == actual |
              (type == "dbl" & actual == "int"))   # int where dbl expected is fine
bad_types <- type_check %>% filter(!ok)
if (nrow(bad_types)) {
  cat("\nTYPE MISMATCHES:\n"); print(bad_types %>% select(column, expected = type, actual))
} else cat("All column types match expected.\n")
flag_issue("structure", "type mismatch", nrow(bad_types))

## ============================================================================
## 2) REQUIRED-FIELD COMPLETENESS — required columns must be fully populated
## ============================================================================
cat("\n========== 2. REQUIRED FIELDS ==========\n")
req_na <- schema %>%
  filter(required, column %in% names(tow)) %>%
  mutate(n_na = map_int(column, ~ sum(is.na(tow[[.x]])))) %>%
  filter(n_na > 0)
if (nrow(req_na)) { cat("Required columns with NAs:\n"); print(req_na %>% select(column, n_na)) }
else cat("All required columns fully populated.\n")
flag_issue("required", "NA in required column", nrow(req_na))

## ============================================================================
## 3) KEY UNIQUENESS — sample_name unique; cruise/station/cast/net_type unique
## ============================================================================
cat("\n========== 3. KEY UNIQUENESS ==========\n")
dup_name <- tow %>% count(sample_name) %>% filter(n > 1)
dup_key  <- tow %>% count(cruise, station, cast, net_type) %>% filter(n > 1)
if (nrow(dup_name)) { cat("Duplicate sample_name:\n"); print(dup_name) } else cat("sample_name unique.\n")
if (nrow(dup_key))  { cat("Duplicate cruise/station/cast/net_type:\n"); print(dup_key) } else cat("cruise/station/cast/net_type unique.\n")
flag_issue("keys", "duplicate sample_name", nrow(dup_name))
flag_issue("keys", "duplicate station/cast key", nrow(dup_key))

# sample_name should equal cruise_station_castprefix pattern loosely
name_mismatch <- tow %>%
  mutate(expect_prefix = paste(cruise, station, sep = "_")) %>%
  filter(!str_starts(sample_name, fixed(expect_prefix))) %>%
  select(cruise, station, cast, sample_name)
if (nrow(name_mismatch)) { cat("\nsample_name not matching cruise_station prefix:\n"); print(name_mismatch, n = Inf) }
flag_issue("keys", "sample_name prefix mismatch", nrow(name_mismatch))

## ============================================================================
## 4) CONTROLLED VOCABULARIES
## ============================================================================
cat("\n========== 4. VOCABULARIES ==========\n")

# net_type
bad_net <- tow %>% filter(!net_type %in% c("bongo", "ring")) %>% count(net_type)
if (nrow(bad_net)) { cat("Unexpected net_type:\n"); print(bad_net) } else cat("net_type OK (bongo/ring).\n")
flag_issue("vocab", "unexpected net_type", nrow(bad_net))

# Y/N columns
cat("\nY/N columns — unexpected values (anything not Y/N/NA):\n")
yn_report <- map_dfr(intersect(yn_cols, names(tow)), function(col) {
  vals <- tow[[col]]
  bad <- setdiff(unique(vals[!is.na(vals)]), c("Y", "N"))
  tibble(column = col, bad_values = if (length(bad)) paste(bad, collapse = "|") else NA_character_,
         n_bad = sum(!vals %in% c("Y","N") & !is.na(vals)))
})
print(yn_report)
flag_issue("vocab", "bad Y/N value", sum(yn_report$n_bad, na.rm = TRUE))

# primary_flag
bad_flag <- tow %>% filter(!primary_flag %in% valid_flags | is.na(primary_flag)) %>%
  count(primary_flag)
if (nrow(bad_flag)) { cat("\nInvalid/NA primary_flag (QARTOD requires 1/3/4/9, none NA):\n"); print(bad_flag) }
else cat("\nprimary_flag OK (all in 1/3/4/9).\n")
flag_issue("vocab", "invalid/NA primary_flag", sum(bad_flag$n))

## ============================================================================
## 5) NUMERIC RANGES — physically plausible bounds (edit to taste)
## ============================================================================
cat("\n========== 5. NUMERIC RANGES ==========\n")
# min/max per range-checked column; a value outside [lo, hi] is flagged
ranges <- tribble(
  ~column,                ~lo,     ~hi,
  "latitude_start",        38,      44,
  "longitude_start",      -74,     -68,
  "latitude_end",          38,      44,
  "longitude_end",        -74,     -68,
  "depth_bottom",          0,       6000,
  "depth_target",          0,       1000,
  "depth_TDR",             0,       1000,
  "depth_PX",              0,       1000,
  "net_max_depth",         0,       1000,
  "avg_angle",             0,       75,
  "max_wire_out",          0,       2000,
  "wire_rate_out",         0,       100,
  "wire_rate_in",          0,       100,
  "STW_start",             0,       5,
  "SOG_start",             0,       5,
  "STW_end",               0,       5,
  "SOG_end",               0,       5,
  "tot_flow_counts_335",   0,       200000,
  "tot_flow_counts_150",   0,       200000,
  "vol_filtered_335",      0,       1500,
  "vol_filtered_150",      0,       1500,
  "haul_factor_10m2_335",  0,       200,
  "haul_factor_10m2_150",  0,       200,
  "haul_factor_100m3_335", 0,       50,
  "haul_factor_100m3_150", 0,       50
)
range_report <- ranges %>%
  filter(column %in% names(tow)) %>%
  mutate(
    n_out = pmap_int(list(column, lo, hi),
                     ~ sum(tow[[..1]] < ..2 | tow[[..1]] > ..3, na.rm = TRUE)),
    obs_min = map_dbl(column, ~ suppressWarnings(min(tow[[.x]], na.rm = TRUE))),
    obs_max = map_dbl(column, ~ suppressWarnings(max(tow[[.x]], na.rm = TRUE)))
  ) %>%
  mutate(across(c(obs_min, obs_max), ~ ifelse(is.infinite(.), NA, .)))
print(range_report, n = Inf)
flag_issue("range", "value out of expected bounds", sum(range_report$n_out))

## ============================================================================
## 6) CROSS-FIELD LOGIC — relationships that must hold
## ============================================================================
cat("\n========== 6. CROSS-FIELD LOGIC ==========\n")

# a) net_max_depth should not greatly exceed depth_bottom (small overshoot ok)
depth_over <- tow %>%
  filter(!is.na(net_max_depth), !is.na(depth_bottom),
         net_max_depth > depth_bottom + 5) %>%
  select(cruise, station, cast, net_type, net_max_depth, depth_bottom)
if (nrow(depth_over)) { cat("net_max_depth > depth_bottom + 5 m:\n"); print(depth_over, n = Inf) }
else cat("net_max_depth within bottom depth (+5 m tolerance).\n")
flag_issue("logic", "net_max_depth exceeds bottom", nrow(depth_over))

# b) flow counts should equal wrap(end - start) for non-flag-3 bongo rows
DIAL <- 999999
count_mismatch <- tow %>%
  filter(net_type == "bongo") %>%
  mutate(
    exp335 = if_else(flow_end_335 < flow_start_335, flow_end_335 + DIAL - flow_start_335, flow_end_335 - flow_start_335),
    exp150 = if_else(flow_end_150 < flow_start_150, flow_end_150 + DIAL - flow_start_150, flow_end_150 - flow_start_150),
    bad335 = !is.na(exp335) & !is.na(tot_flow_counts_335) & abs(tot_flow_counts_335 - exp335) > 0.5,
    bad150 = !is.na(exp150) & !is.na(tot_flow_counts_150) & abs(tot_flow_counts_150 - exp150) > 0.5
  ) %>%
  filter((bad335 | bad150), !primary_flag %in% c(3)) %>%   # flag-3 substitutions excluded
  select(cruise, station, cast, primary_flag,
         tot_flow_counts_335, exp335, tot_flow_counts_150, exp150)
if (nrow(count_mismatch)) { cat("\nflow counts != end-start (non-flag-3 bongo):\n"); print(count_mismatch, n = Inf) }
else cat("flow counts reconcile with start/end (non-flag-3 bongo).\n")
flag_issue("logic", "flow count mismatch", nrow(count_mismatch))

# c) volume should follow counts * Ff * A for flowmeter (non-flag-3) rows
Ff <- 0.026873; A <- 0.2922
vol_mismatch <- tow %>%
  filter(net_type == "bongo", !primary_flag %in% c(3)) %>%
  mutate(
    v335 = tot_flow_counts_335 * Ff * A,
    v150 = tot_flow_counts_150 * Ff * A,
    bad335 = !is.na(v335) & !is.na(vol_filtered_335) & abs(vol_filtered_335 - v335) > 0.5,
    bad150 = !is.na(v150) & !is.na(vol_filtered_150) & abs(vol_filtered_150 - v150) > 0.5
  ) %>%
  filter(bad335 | bad150) %>%
  select(cruise, station, cast, vol_filtered_335, v335, vol_filtered_150, v150)
if (nrow(vol_mismatch)) { cat("\nvolume != counts*Ff*A (non-flag-3 bongo):\n"); print(vol_mismatch, n = Inf) }
else cat("volume reconciles with counts (non-flag-3 bongo).\n")
flag_issue("logic", "volume mismatch", nrow(vol_mismatch))

# d) ring rows must have NA volume + NA haul factors
ring_vol <- tow %>%
  filter(net_type == "ring",
         !is.na(vol_filtered_335) | !is.na(vol_filtered_150) |
         !is.na(haul_factor_10m2_335) | !is.na(haul_factor_100m3_335)) %>%
  select(cruise, station, cast, sample_name)
if (nrow(ring_vol)) { cat("\nring rows with non-NA volume/haul factor:\n"); print(ring_vol, n = Inf) }
else cat("ring rows correctly NA for volume/haul factors.\n")
flag_issue("logic", "ring row has volume/haul", nrow(ring_vol))

# e) datetime_end should be after datetime_start (allowing midnight rollover handled upstream)
time_order <- tow %>%
  filter(!is.na(datetime_UTC_start), !is.na(datetime_UTC_end),
         datetime_UTC_end < datetime_UTC_start) %>%
  select(cruise, station, cast, datetime_UTC_start, datetime_UTC_end)
if (nrow(time_order)) { cat("\nend before start (check rollover):\n"); print(time_order, n = Inf) }
else cat("datetime_end >= datetime_start.\n")
flag_issue("logic", "end before start", nrow(time_order))

# f) every non-Good flag should carry a secondary note
flag_no_note <- tow %>%
  filter(primary_flag != 1, is.na(secondary_flag)) %>%
  select(cruise, station, cast, sample_name, primary_flag)
if (nrow(flag_no_note)) { cat("\nflagged rows missing secondary_flag:\n"); print(flag_no_note, n = Inf) }
else cat("all non-Good flags carry a secondary note.\n")
flag_issue("logic", "flag without secondary note", nrow(flag_no_note))

## ============================================================================
## 7) PER-COLUMN SUMMARY  (NA counts, distinct values, min/max)
## ============================================================================
cat("\n========== 7. PER-COLUMN SUMMARY ==========\n")
col_summary <- tibble(column = names(tow)) %>%
  mutate(
    type      = map_chr(tow, ~ recode(class(.x)[1], !!!type_map, .default = class(.x)[1])),
    n         = nrow(tow),
    n_na      = map_int(tow, ~ sum(is.na(.x))),
    pct_na    = round(100 * n_na / n, 1),
    n_distinct= map_int(tow, ~ dplyr::n_distinct(.x, na.rm = TRUE)),
    min_val   = map_chr(tow, ~ if (is.numeric(.x)) as.character(suppressWarnings(min(.x, na.rm = TRUE))) else NA_character_),
    max_val   = map_chr(tow, ~ if (is.numeric(.x)) as.character(suppressWarnings(max(.x, na.rm = TRUE))) else NA_character_),
    example   = map_chr(tow, ~ as.character(na.omit(.x)[1]))
  ) %>%
  mutate(across(c(min_val, max_val), ~ ifelse(. %in% c("Inf","-Inf"), NA, .)))
print(col_summary, n = Inf)

## ============================================================================
## 8) EXPORT data dictionary (column name + type + summary)
## ============================================================================
stamp <- format(Sys.Date(), "%Y%m%d")
dict_path <- here("data", "processed", glue("tow-metadata-v3-data-dictionary-{stamp}.csv"))
write_csv(col_summary, dict_path)
message("Data dictionary written: ", basename(dict_path))

## ============================================================================
## 9) FINAL VERDICT
## ============================================================================
cat("\n========== VERDICT ==========\n")
if (nrow(issues) == 0) {
  cat("PASS — no issues detected across all checks.\n")
} else {
  cat("Issues found (review each):\n")
  print(issues, n = Inf)
}

## session info for reproducibility (matches your 05 convention)
cat("\n---- session info ----\n")
print(sessionInfo())
