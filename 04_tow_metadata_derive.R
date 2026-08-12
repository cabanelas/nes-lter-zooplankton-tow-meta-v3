################################################################################
## Script:  04_tow_metadata_derive.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Derive the calculated tow-metadata columns for the NEW v3 cruises
##          and produce the final combined tow-metadata table (2018-2026).
##          v2 (older version) rows already carry all derived columns from the
##          published package and are left UNTOUCHED here; this script only
##          fills the columns that came in NA for the new cruises.
##
##      1. join ship speed (STW/SOG, deploy+recover) from script 02
##      2. net_max_depth  (cosine law, when TDR/PX depth unavailable)
##      3. volume filtered (keep logsheet value; fallback V=A*T*S only
##         where flowmeter volume is NA)
##      4. haul factors
##      5. data flags (primary/secondary) from comments
##
## Inputs (data/):
##   - processed/nes-lter-bongologs-{last}-YYYYMMDD.rds  (03_bongo_logs_merge.R)
##   - processed/shipspeed_eventlog_v3.csv               (02_ship_speed_eventlog_merge.R)
## Outputs (data/processed/):
##   - nes-lter-zooplankton-tow-metadata-v3-YYYYMMDD.csv
################################################################################
#
# =============================================================================
# ***  FLOWMETER VOLUME FACTOR   ***
# -----------------------------------------------------------------------------
# The published v2 volumes AND the new-cruise logsheet volumes were BOTH verified
# (Aug 2026) to use this formula:
#
#     vol_filtered_m3 = tot_flow_counts * 0.026873 * A         (single-step form)
#                     = (tot_flow_counts / 10) * 0.26873 * A   (identical value)
#   where A = pi * (0.61/2)^2 = 0.2922 m^2  (61-cm bongo mouth area)
#
# The "0.26873 with counts/10" written in the old v2 scripts is NOT a bug: the
# /10 makes it equal to 0.026873 with raw counts. Both reproduce published
# volumes exactly (checked cell-by-cell against knb-lter-nes.24.2 and against
# the AE2426/EN727/AR88/AR92 logsheets).
#
#   >>> DO NOT combine (counts/10) WITH 0.026873 <<<  that is 10x too small and
#       would make new-cruise volumes wrong and abundances 10x too large,
#       breaking consistency with every published old cruise.
#
# tot_flow_counts is RAW counter revolutions (final reading - initial reading).
# Published abundances used volumes from the tow-metadata CSV, so they inherit
# this (correct) formula - they do NOT need republishing.
# =============================================================================

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)
library(lubridate)

## ------------------------------------------ ##
#            Constants -----
## ------------------------------------------ ##
FLOW_FACTOR <- 0.026873               # m per revolution 
NET_DIAM_M  <- 0.61                   # 61-cm bongo mouth
A_MOUTH     <- pi * (NET_DIAM_M/2)^2  # 0.2922 m^2
KT_TO_MS    <- 0.514444               # knots -> m/s
DEFAULT_SPEED_MS <- 1.5               # last-resort tow speed if STW & SOG all NA

# new v3 cruises that need derived columns filled (v2 rows untouched)
new_cruises <- c("AE2426", "EN727", "AR88", "AR92", "AR95", "AR99", "HRS2601")

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

## --- tow metadata from 03_bongo_logs_merge.R --- ##
# latest saved .rds
files <- list.files(here("data", "processed"),
                    pattern = "^nes-lter-bongologs-.*\\.rds$", full.names = TRUE)
dates <- as.Date(str_extract(basename(files), "\\d{8}"), "%Y%m%d")
latest_bongolog <- files[which.max(dates)]
message("Reading tow metadata: ", basename(latest_bongolog))
tow_meta <- readRDS(latest_bongolog)

## --- ship speed + event log from 02_ship_speed_eventlog_merge.R --- ##
ship_speed <- read_csv(here("data", "processed", "shipspeed_eventlog_v3.csv"))

## ------------------------------------------ ##
#       Tidy -----
## ------------------------------------------ ##

## ------------------------------------------ ##
#      1) Ship speed: pivot deploy/recover -> wide -----
## ------------------------------------------ ##
# 02 output is long (one row per deploy/recover)
# Pivot to start/end columns
# Strip B/R prefix on cast so keys match tow_meta cast ("B12" -> "12")

ship_speed_wide <- ship_speed %>%
  mutate(cast = str_remove(cast, "^[BR]")) %>%
  filter(action %in% c("deploy", "recover")) %>%
  select(cruise, station, cast, action,
         speedlog_waterspeedfwd, speedlog_groundspeedfwd) %>%
  distinct(cruise, station, cast, action, .keep_all = TRUE) %>%
  pivot_wider(
    names_from  = action,
    values_from = c(speedlog_waterspeedfwd, speedlog_groundspeedfwd),
    names_glue  = "{.value}_{action}"
  ) %>%
  rename(
    STW_start = speedlog_waterspeedfwd_deploy,
    STW_end   = speedlog_waterspeedfwd_recover,
    SOG_start = speedlog_groundspeedfwd_deploy,
    SOG_end   = speedlog_groundspeedfwd_recover
  )

# Join speed onto NEW cruises only; coalesce == v2 speeds stay intact
tow_meta <- tow_meta %>%
  left_join(ship_speed_wide, by = c("cruise", "station", "cast"),
            suffix = c("", "_new")) %>%
  mutate(
    STW_start = if_else(cruise %in% new_cruises, 
                        coalesce(STW_start_new, STW_start), STW_start),
    STW_end   = if_else(cruise %in% new_cruises, 
                        coalesce(STW_end_new,   STW_end),   STW_end),
    SOG_start = if_else(cruise %in% new_cruises, 
                        coalesce(SOG_start_new, SOG_start), SOG_start),
    SOG_end   = if_else(cruise %in% new_cruises, 
                        coalesce(SOG_end_new,   SOG_end),   SOG_end)
  ) %>%
  select(-ends_with("_new"))

# Endeavor STW is NA bc speedlog reports GPS-SOG in both channels

## ------------------------------------------ ##
#      EN720: overwrite v2's placeholder SOG -----
## ------------------------------------------ ##
# v2 published a hardcoded SOG = 2 kt for all EN720 tows (real underway speed
# was unavailable at v2 publication)
# adding the real per-tow SOG now -> overwrite
# STW stays NA since Endeavor = SOG-only 
# All other EN720 columns (depth, volume, haul, flags) remain as published in v2
en720_speed <- ship_speed_wide %>%
  filter(cruise == "EN720") %>%
  select(cruise, station, cast, SOG_start_fix = SOG_start, SOG_end_fix = SOG_end)

tow_meta <- tow_meta %>%
  left_join(en720_speed, by = c("cruise", "station", "cast")) %>%
  mutate(
    SOG_start = if_else(cruise == "EN720" & !is.na(SOG_start_fix), SOG_start_fix, SOG_start),
    SOG_end   = if_else(cruise == "EN720" & !is.na(SOG_end_fix),   SOG_end_fix,   SOG_end)
  ) %>%
  select(-SOG_start_fix, -SOG_end_fix)

## ------------------------------------------ ##
#      2) net_max_depth (cosine law fallback) -----
## ------------------------------------------ ##
# Priority for the published net_max_depth:
#   depth_TDR (or already-set net_max_depth) > cosine-law calc > depth_target
# Cosine law:  Z = L * cos(wire_angle)   [angle from vertical]
# Ring nets are vertical tows -> use max_wire_out (or depth_target)
tdr_offset <- read_csv(here("data", "raw", "tdr_offsets.csv"))
tdr_offset2 <- read_csv(here("data", "raw", "nes-lter-bongo-tdr-offsets.csv"))
## i need to check whether TDR depths are already corrected/offsets applied
#compare with tdrdata output/compare depths
#TDR and PX depth columns should be "fixed" correct ones for calculations  -- check that that is the case by comparing with TDR data and values in the scrpt

### HERE 
tow_meta <- tow_meta %>%
  mutate(
    # adding "." to mark these cols as temporary
    .angle_rad = avg_angle * pi/180,
    .tow_depth_calc = case_when(
      instrument == "Ring Net" & !is.na(max_wire_out) ~ max_wire_out,
      instrument == "Ring Net" & is.na(max_wire_out) & !is.na(depth_target) ~ depth_target,
      !is.na(max_wire_out) & !is.na(avg_angle) ~ max_wire_out * cos(.angle_rad),
      TRUE ~ NA_real_
    )
  )

# Fill net_max_depth ONLY where it's currently NA (new cruises), preserving v2.
tow_meta <- tow_meta %>%
  mutate(
    net_max_depth = case_when(
      !is.na(net_max_depth)                     ~ net_max_depth,        # keep v2 / logsheet
      !is.na(depth_TDR)                         ~ depth_TDR,
      !is.na(.tow_depth_calc)                   ~ .tow_depth_calc,
      !is.na(depth_target)                      ~ depth_target,
      TRUE ~ NA_real_
    )
  )

## ------------------------------------------ ##
#      3) Volume filtered -----
## ------------------------------------------ ##
# Flowmeter volumes already present & verified correct (logsheet + v2) -> KEEP.
# Compute calculated fallback V = A * T * S only where flowmeter vol is NA.
#   T = tow duration (s);  S = tow speed (m/s)
# Speed priority: STW_start -> STW_end -> SOG_start -> SOG_end -> DEFAULT_SPEED_MS
#   (SOG used before defaulting, so SOG-only cruises like EN727/AE2426/HRS use
#    real measured speed instead of a 1.5 m/s guess. ~1-2 kt is typical.)

tow_meta <- tow_meta %>%
  mutate(
    .dur_s = as.numeric(difftime(datetime_UTC_end, datetime_UTC_start, units = "secs")),
    .dur_s = if_else(.dur_s < 0, .dur_s + 24*3600, .dur_s),   # crossed midnight
    .speed_kt = coalesce(abs(STW_start), abs(STW_end),
                         abs(SOG_start), abs(SOG_end)),
    .speed_ms = if_else(is.na(.speed_kt), DEFAULT_SPEED_MS, .speed_kt * KT_TO_MS),
    .vol_calc = A_MOUTH * .dur_s * .speed_ms
  )

# Fill NA flowmeter volumes with the calculated value; flag those rows.
tow_meta <- tow_meta %>%
  mutate(
    .vol335_was_na = is.na(vol_filtered_335) & instrument == "Bongo Net",
    .vol150_was_na = is.na(vol_filtered_150) & instrument == "Bongo Net",
    vol_filtered_335 = if_else(.vol335_was_na, .vol_calc, vol_filtered_335),
    vol_filtered_150 = if_else(.vol150_was_na, .vol_calc, vol_filtered_150),
    # Ring nets are not quantitative -> volume stays NA
    vol_filtered_335 = if_else(instrument == "Ring Net", NA_real_, vol_filtered_335),
    vol_filtered_150 = if_else(instrument == "Ring Net", NA_real_, vol_filtered_150)
  )

## ------------------------------------------ ##
#      4) Haul factors -----
## ------------------------------------------ ##
# Only fill where NA (new cruises); keep published v2 values.
tow_meta <- tow_meta %>%
  mutate(
    haul_factor_10m2_335  = if_else(is.na(haul_factor_10m2_335),
                                    (net_max_depth*10)/vol_filtered_335, haul_factor_10m2_335),
    haul_factor_10m2_150  = if_else(is.na(haul_factor_10m2_150),
                                    (net_max_depth*10)/vol_filtered_150, haul_factor_10m2_150),
    haul_factor_100m3_335 = if_else(is.na(haul_factor_100m3_335),
                                    100/vol_filtered_335, haul_factor_100m3_335),
    haul_factor_100m3_150 = if_else(is.na(haul_factor_100m3_150),
                                    100/vol_filtered_150, haul_factor_100m3_150)
  )

## ------------------------------------------ ##
#      5) Data flags (new cruises) -----
## ------------------------------------------ ##
# Build secondary_flag from comments + depth/volume provenance, then set
# primary_flag = 3 wherever a secondary_flag exists. Only touch rows whose
# flags are still NA (i.e. new cruises); v2 flags are preserved.

tow_meta <- tow_meta %>%
  mutate(
    .sec = case_when(
      grepl("hit bottom", comments, ignore.case = TRUE) ~ "hit bottom.",
      is.na(depth_TDR) & is.na(depth_PX) & !is.na(.tow_depth_calc) ~
        "Depth recorder (TDR) data not available. Net max depth was calculated based on wire information (cosine law).",
      is.na(depth_TDR) & is.na(depth_PX) & is.na(.tow_depth_calc) & !is.na(depth_target) ~
        "Target depth used for net max depth due to unavailable TDR data and wire information.",
      instrument == "Ring Net" & is.na(max_wire_out) & !is.na(depth_target) ~
        "Target depth used for net max depth due to unavailable wire data for Ring Net.",
      TRUE ~ NA_character_
    )
  )

# append comment-driven notes
append_note <- function(sec, comments, pattern, note) {
  hit <- grepl(pattern, comments, ignore.case = TRUE)
  ifelse(hit, str_squish(paste(coalesce(sec, ""), note)), sec)
}
tow_meta <- tow_meta %>%
  mutate(
    .sec = append_note(.sec, comments, "tons of small salps", "many salps in this cruise."),
    .sec = append_note(.sec, comments, "cod end broke",        "cod end broke."),
    .sec = append_note(.sec, comments, "cod end was tangled",  "tangled cod end."),
    .sec = append_note(.sec, comments, "Non quantitative 150", "non quantitative 150 micron sample."),
    .sec = append_note(.sec, comments, "150um sample probably non quantitative", "non quantitative 150 micron sample."),
    .sec = append_note(.sec, comments, "Non quantitative 335", "non quantitative 335 micron sample."),
    .sec = append_note(.sec, comments, "Deployed and recovered without sample", "no sample."),
    .sec = append_note(.sec, comments, "forgot to get flow start", "flowmeter issue."),
    .sec = append_note(.sec, comments, "Flowmeter calibration", "no sample."),
    .sec = append_note(.sec, comments, "no 20um ring net sample", "no 20um ring net sample."),
    .sec = append_note(.sec, comments, "flowmeter reading is off", "flowmeter issue."),
    .sec = append_note(.sec, comments, "missing flowmeter numbers for 335um net", "flowmeter issue."),
    .sec = na_if(str_squish(.sec), "")
  )

# note calculated-volume tows in the flag
tow_meta <- tow_meta %>%
  mutate(
    .sec = if_else(.vol335_was_na | .vol150_was_na,
                   str_squish(paste(coalesce(.sec, ""),
                     "Volume sampled calculated from ship speed and tow duration (flowmeter unavailable).")),
                   .sec),
    .sec = na_if(.sec, "")
  )

# apply ONLY to new-cruise rows with currently-missing flags (preserve v2)
tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = if_else(cruise %in% new_cruises & is.na(secondary_flag), .sec, secondary_flag),
    primary_flag   = case_when(
      cruise %in% new_cruises & is.na(primary_flag) & !is.na(secondary_flag) ~ 3,
      cruise %in% new_cruises & is.na(primary_flag) &  is.na(secondary_flag) ~ 1,
      TRUE ~ primary_flag
    )
  )

## ------------------------------------------ ##
#      Drop working columns -----
## ------------------------------------------ ##
tow_meta <- tow_meta %>%
  select(-starts_with("."))

## ------------------------------------------ ##
#      QA/QC -----
## ------------------------------------------ ##

# 1. new cruises now populated where expected
tow_meta %>%
  filter(cruise %in% new_cruises) %>%
  group_by(cruise) %>%
  summarise(n = n(),
            stw_na  = sum(is.na(STW_start)),
            sog_na  = sum(is.na(SOG_start)),
            depth_na = sum(is.na(net_max_depth)),
            vol335_na = sum(is.na(vol_filtered_335) & instrument == "Bongo Net"),
            haul_na = sum(is.na(haul_factor_10m2_335) & instrument == "Bongo Net"),
            pflag_na = sum(is.na(primary_flag)),
            .groups = "drop") %>%
  print(n = Inf)

# 2. net_max_depth should rarely exceed depth_bottom (a little is ok - tow drift)
tow_meta %>%
  filter(net_max_depth > depth_bottom) %>%
  mutate(diff = net_max_depth - depth_bottom) %>%
  select(cruise, station, cast, depth_bottom, depth_TDR, depth_PX,
         net_max_depth, diff) %>%
  arrange(desc(diff))

# 3. Endeavor STW still NA (sanity - should be TRUE)
tow_meta %>% filter(str_starts(cruise, "EN")) %>%
  summarise(endeavor_stw_all_na = all(is.na(STW_start)))

# 4. haul factors finite (no /0 from zero volume)
tow_meta %>%
  filter(is.infinite(haul_factor_100m3_335) | is.infinite(haul_factor_10m2_335)) %>%
  select(cruise, station, cast, vol_filtered_335, net_max_depth)

# 5. volume range sane per cruise
tow_meta %>%
  filter(instrument == "Bongo Net") %>%
  group_by(cruise) %>%
  summarise(min_v = min(vol_filtered_335, na.rm = TRUE),
            max_v = max(vol_filtered_335, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  print(n = Inf)

## ------------------------------------------ ##
#      Write -----
## ------------------------------------------ ##
stamp <- format(Sys.Date(), "%Y%m%d")
write_csv(tow_meta, here("data", "processed",
                         glue::glue("nes-lter-zooplankton-tow-metadata-v3-{stamp}.csv")))
