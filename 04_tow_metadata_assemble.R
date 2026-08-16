################################################################################
## Script:  04_tow_metadata_assemble.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Fill every column that has NA from 03 script for the NEW v3 cruises, 
##          and produce the final combined tow-metadata table (2018-2026).
##          v2 (older version) rows already carry all derived columns from the
##          published package and are left mostly untouched here; this script mainly
##          fills the columns that came in NA for the new cruises. couple exceptions
##          when a fix was needed
##
##    1. coordinates   (end from elog; fixed a few start coords)
##    2. depths        (depth_TDR corrected + depth_PX, incl. AR99 ring)
##    3. net_max_depth (coalesce corrected_TDR / PX / cosine / target)
##    4. speeds        (STW/SOG, deploy+recover from script 02)
##    5. size_fract_20 (from inventory sheet)
##    6. volume        (keep flowmeter; fallback V=A*T*S where flowmeter == NA)
##    7. haul factors
##    8. data flags
##
## Inputs (data/):
##  (data/processed/):
##   - nes-lter-bongologs-{last}-YYYYMMDD.rds  (03_bongo_logs_merge.R)
##   - shipspeed_eventlog_v3.csv               (02_ship_speed_eventlog_merge.R)
##   - sample_inventory_combined-YYYYMMDD.csv  (sample_inventory_combine.R)
##  (data/raw/):
##   - elog_zoop_tows_thruHRS2601_2026-08-10.csv     (nes-lter-api-pulls)
##          https://github.com/cabanelas/nes-lter-api-pulls
##   - nes-lter-bongo-tdr-offsets.csv             (nes-lter-tdr-bongo)
##          https://github.com/cabanelas/nes-lter-tdr-bongo
##
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
# /10 == 0.026873 with raw counts. Both reproduce published volumes exactly.
# checked cell-by-cell against knb-lter-nes.24.2 and AE2426/EN727/AR88/AR92 logsheets
#
#   >>> DO NOT combine (counts/10) WITH 0.026873 <<<  that is 10x too small.
#
# Flowmeter volumes here are KEPT as recorded; this script only computes the
# A*T*S fallback where a flowmeter volume is missing. So this factor is not
# re-applied below - it is documented for provenance.
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
#            Constants / switches -----
## ------------------------------------------ ##
#FLOW_FACTOR <- 0.026873              # m per revolution 
NET_DIAM_M   <- 0.61                  # 61-cm bongo mouth
A_MOUTH      <- pi * (NET_DIAM_M/2)^2 # 0.2922 m^2
KT_TO_MS     <- 0.514444              # knots -> m/s

# new v3 cruises that need derived columns filled (v2 rows preserved)
new_cruises <- c("AE2426", "EN727", "AR88", "AR92", "AR95", "AR99", "HRS2601")

# TRUE  = offset-correct depth_TDR for ALL cruises (v2 included); deliberate v2 edit
# FALSE = only new cruises get corrected depth_TDR; v2 depth_TDR left as published
CORRECT_V2_DEPTH_TDR <- TRUE

## helper: derive net_type from a B/R cast prefix, used to key joins so an AR99
## bongo cast and ring cast at the same station can't collide after the strip
net_type_from_cast <- function(cast) if_else(grepl("^R", cast), "ring", "bongo")
strip_BR           <- function(cast) gsub("^[BR]", "", as.character(cast))

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

## --- merged tow metadata from 03 (latest saved .rds) --- ##
files <- list.files(here("data", "processed"),
                    pattern = "^tow-meta-v3-intermediate-.*\\.rds$", 
                    full.names = TRUE)
dates <- as.Date(str_extract(basename(files), "\\d{8}"), "%Y%m%d")
latest_bongolog <- files[which.max(dates)]
message("Reading tow metadata: ", basename(latest_bongolog))
tow_meta <- readRDS(latest_bongolog)

## --- event log (for coordinates) --- ##
event_log <- read_csv(here("data", "raw",
                           "elog_zoop_tows_thruHRS2601_2026-08-10.csv"))

## --- PX + TDR --- ##
tdr <- read_csv(here("data", "raw", "nes-lter-bongo-tdr-offsets.csv"))

## --- ship speed from script 02_ship_speed_eventlog_merge.R --- ##
ship_speed <- read_csv(here("data", "processed", "shipspeed_eventlog_v3.csv"))

## --- most-recent sample inventory --- ##
inv_files <- list.files(here("data", "processed"),
                        pattern = "^sample_inventory_combined-.*\\.csv$",
                        full.names = TRUE)
if (length(inv_files) == 0) stop("No sample_inventory_combined-*.csv in data/processed/")
inv_dates <- as.Date(str_extract(basename(inv_files), "\\d{8}"), "%Y%m%d")
latest_inventory <- inv_files[which.max(inv_dates)]
message("Reading sample inventory: ", basename(latest_inventory))
inventory <- read_csv(latest_inventory)

# columns that are entirely NA across the new-cruise rows
tow_meta %>%
  filter(cruise %in% new_cruises, net_type != "ring") %>%
  summarize(across(everything(), ~ all(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "all_na") %>%
  filter(all_na) %>%
  mutate(pos = match(column, names(tow_meta))) %>%
  arrange(pos) %>%
  pull(column)

## ========================================================================== ##
## 1) COORDINATES 
## ========================================================================== ##
# Bongo logsheets have START coords; END (recover) coords come from the elog
# coalesce() fills only NA ends -> v2 end coords preserved
# A few START coords are known-bad in the logsheet and are set from the elog.

## ------------------------------------------ ##
##  End coordinates for new-cruise bongo tows (from elog) 
## ------------------------------------------ ##
## --- latitude_end & longitude_end   --- ##
## --- end coords (recover) from elog --- ##
elog_end_coords <- event_log %>%
  filter(action == "recover") %>%
  mutate(net_type = net_type_from_cast(cast),
         cast     = strip_BR(cast)) %>%
  group_by(cruise, station, cast, net_type) %>%
  summarize(latitude_end_elog  = first(latitude),
            longitude_end_elog = first(longitude), .groups = "drop")

tow_meta <- tow_meta %>%
  left_join(elog_end_coords, 
            by = c("cruise", "station", "cast", "net_type")) %>%
  mutate(latitude_end  = coalesce(latitude_end,  latitude_end_elog),
         longitude_end = coalesce(longitude_end, longitude_end_elog)) %>%
  select(-latitude_end_elog, -longitude_end_elog)

## ------------------------------------------ ##
##  Check coordinates: logsheet/v2 vs elog START coords (all cruises) 
## ------------------------------------------ ##
## --- deliberate START-coord fixes (bad logsheet entries -> elog) --- ## 
# typos in logsheets, use elog vals
# AR38   L6  : both lat + lon
# AE2426 L2  : lat
# AR92   L6  : lat
# AR95   MVCO: lon
# AR95   L6  : lon
elog_start_coords <- event_log %>%
  filter(action == "deploy") %>%
  mutate(net_type = net_type_from_cast(cast),
         cast     = strip_BR(cast)) %>%
  group_by(cruise, station, cast, net_type) %>%
  summarize(lat_start_elog = first(latitude),
            lon_start_elog = first(longitude), .groups = "drop")

## --- comparing bongo logsheets coordinates vs elog coordinates --- ##
start_check <- tow_meta %>%
  select(cruise, station, cast, sample_name, net_type,
         latitude_start, longitude_start) %>%
  left_join(elog_start_coords, 
            by = c("cruise", "station", "cast", "net_type")) %>%
  mutate(
    lat_diff = latitude_start  - lat_start_elog,
    lon_diff = longitude_start - lon_start_elog,
    dist_m = sqrt((lat_diff * 111000)^2 +
                    (lon_diff * 111000 * cos(latitude_start * pi/180))^2)
  )

start_check %>%
  summarize(n_compared = sum(!is.na(dist_m)),
            n_no_elog   = sum(is.na(dist_m)),
            max_dist_m  = max(dist_m, na.rm = TRUE),
            mean_dist_m = mean(dist_m, na.rm = TRUE),
            n_over_500m = sum(dist_m > 500, na.rm = TRUE))

# show me ones with highest distance discrepancy 
start_check %>%
  filter(dist_m > 500) %>%
  arrange(desc(dist_m)) %>%
  select(cruise, station, cast, latitude_start, lat_start_elog,
         longitude_start, lon_start_elog, dist_m) %>%
  print(n = Inf)

tow_meta <- tow_meta %>%
  left_join(elog_start_coords, by = c("cruise", "station", "cast", "net_type")) %>%
  mutate(
    # AR38 L6: both coordinates
    latitude_start  = if_else(cruise == "AR38" & station == "L6",
                             lat_start_elog, latitude_start),
    longitude_start = if_else(cruise == "AR38" & station == "L6",
                              lon_start_elog, longitude_start),
    # latitude-only fixes
    latitude_start  = if_else((cruise == "AE2426" & station == "L2") |
                               (cruise == "AR92" & station == "L6"),
                             lat_start_elog, latitude_start),
    # longitude-only fixes
    longitude_start = if_else((cruise == "AR95" & station == "MVCO") |
                                (cruise == "AR95" & station == "L6"),
                              lon_start_elog, longitude_start)
  ) %>%
  select(-lat_start_elog, -lon_start_elog)

## ========================================================================== ##
## 2) DEPTHS  (depth_TDR corrected & depth_PX)
## ========================================================================== ##

## ------------------------------------------ ##
##  depth_TDR 
## ------------------------------------------ ##
## --- 2a) corrected depth_TDR = raw + offset (bongo AND AR99 ring) --- ##
tdr_corr <- tdr %>%
  mutate(net_type       = net_type_from_cast(cast),
         cast           = strip_BR(cast),
         depth_TDR_corr = round(tdr_max_depth_m + offset_m, 2)) %>%
  select(cruise, station, cast, net_type, depth_TDR_corr)

# EN657  L1: TDR file was nested under another cast; v2 used cosine (15.78)
# AE2426 L2, L7, L8: TDR column were actually PX depths, fixed to actual TDR
tow_meta <- tow_meta %>%
  mutate(.depth_TDR_orig = depth_TDR) %>%              # snapshot for the diff
  left_join(tdr_corr, by = c("cruise", "station", "cast", "net_type")) %>%
  mutate(
    depth_TDR = case_when(
      !is.na(depth_TDR_corr) & (CORRECT_V2_DEPTH_TDR | cruise %in% new_cruises)
        ~ depth_TDR_corr,
      TRUE ~ depth_TDR
    )
  ) %>%
  select(-c(depth_TDR_corr, .depth_TDR_orig)) %>%
  relocate(depth_TDR, .after = depth_target)

## ------------------------------------------ ##
##  Add depth_PX column 
## ------------------------------------------ ##

## --- 2b) depth_PX (new v3 column; v2 rows stay NA) --- ##
tdr %>% filter(!is.na(px_max_depth_m)) %>% count(cruise) %>% arrange(desc(n))
tdr %>% group_by(cruise) %>%
  summarize(n = n(),
            n_px = sum(!is.na(px_max_depth_m)),
            .groups = "drop")

px_max <- tdr %>%
  filter(!is.na(px_max_depth_m)) %>%
  mutate(net_type = net_type_from_cast(cast),
         cast     = strip_BR(cast)) %>%
  group_by(cruise, station, cast, net_type) %>%
  summarize(depth_PX = max(px_max_depth_m, na.rm = TRUE), .groups = "drop")

## --- merge and add missing depth_PX -- 
# casts where PX data wasnt saved but depth written
# EN727 L9 = 200
# AR99 L2 = 44 & L9 = 205
# AR95 L6 = 91
tow_meta <- tow_meta %>%
  left_join(px_max, by = c("cruise", "station", "cast", "net_type")) %>%
  mutate(
    depth_PX = case_when(          # manual fills where PX data was missing
      sample_name == "EN727_L9_B11" ~ 200,
      sample_name == "AR95_L6_B11" ~ 91,
      sample_name == "AR99_L2_B3"  ~ 44,
      sample_name == "AR99_L9_B5"  ~ 205,
      TRUE ~ depth_PX
    )
  ) %>%
  relocate(depth_PX, .after = depth_TDR)

tow_meta %>%
  filter(!is.na(depth_PX)) %>%
  select(cruise, station, cast, depth_PX, depth_TDR) %>%
  arrange(cruise, station) %>%
  print(n = Inf)

## ========================================================================== ##
## 3) net_max_depth   (coalesce corrected_TDR / PX / cosine / target)
## ========================================================================== ##
# Priority: 
#    published v2/logsheet value > depth_PX > cosine-law calc > depth_target
# Cosine law:  Z = L * cos(wire_angle)   [angle from vertical]
#       Z = calculated tow depth (in meters)
#       L = maximum wire out (in meters)
#   Cos_a = cosine of the wire angle at max wire out. Wire angle is measured between towing wire and the vertical.
# Ring nets are vertical tows -> use max_wire_out (or depth_target)

tow_meta <- tow_meta %>%
  mutate(
    # adding "." to mark these cols as temporary
    .angle_rad = avg_angle * pi/180, # convert avg_angle to radians
    .tow_depth_calc = case_when(
      # vertical tow for Ring Net
      net_type == "ring" & !is.na(max_wire_out)                       ~ max_wire_out,
      net_type == "ring" & is.na(max_wire_out) & !is.na(depth_target) ~ depth_target,
      !is.na(max_wire_out) & !is.na(avg_angle)                        ~ max_wire_out * cos(.angle_rad),
      TRUE ~ NA_real_
    ),
    # Fill net_max_depth ONLY where it's currently NA (new cruises), preserving v2
    net_max_depth = case_when(
      # EN712 / EN720: use corrected depth_TDR
      cruise %in% c("EN712","EN720") & !is.na(depth_TDR) ~ depth_TDR,
      !is.na(net_max_depth)   ~ net_max_depth,     # PRESERVE v2 / logsheet
      !is.na(depth_TDR) & !is.na(depth_PX) ~ (depth_TDR + depth_PX) / 2,  # both -> average
      !is.na(depth_TDR)       ~ depth_TDR,         # corrected TDR (new cruises)
      !is.na(depth_PX)        ~ depth_PX,
      !is.na(.tow_depth_calc) ~ .tow_depth_calc,
      !is.na(depth_target)    ~ depth_target,
      TRUE ~ NA_real_
    )
  ) 
  
# Check if net_max_depth_m is ever greater than depth_bottom
tow_meta %>%
  filter(net_max_depth > depth_bottom) %>%
  mutate(difference = net_max_depth - depth_bottom) %>%
  select(cruise, station, cast, depth_bottom, depth_TDR, net_max_depth, difference)

## ========================================================================== ##
## 4) SPEEDS   (STW/SOG from script 02, new cruises; EN720 SOG override)
## ========================================================================== ##

## ------------------------------------------ ##
#  Ship speed: pivot deploy/recover -> wide -----
## ------------------------------------------ ##
# 02 output is long (one row per deploy/recover) -> pivot to start/end
ship_speed_wide <- ship_speed %>%
  mutate(net_type = net_type_from_cast(cast),
         cast     = strip_BR(cast)) %>%
  filter(action %in% c("deploy", "recover")) %>%
  select(cruise, station, cast, net_type, action,
         speedlog_waterspeedfwd, speedlog_groundspeedfwd) %>%
  distinct(cruise, station, cast, net_type, action, .keep_all = TRUE) %>%
  pivot_wider(names_from  = action,
              values_from = c(speedlog_waterspeedfwd, speedlog_groundspeedfwd),
              names_glue  = "{.value}_{action}") %>%
  rename(STW_start = speedlog_waterspeedfwd_deploy,
         STW_end   = speedlog_waterspeedfwd_recover,
         SOG_start = speedlog_groundspeedfwd_deploy,
         SOG_end   = speedlog_groundspeedfwd_recover)

# fill NEW cruises only; coalesce keeps v2 speeds intact

tow_meta <- tow_meta %>%
  left_join(ship_speed_wide, by = c("cruise", "station", "cast", "net_type"),
            suffix = c("", "_new")) %>%
  mutate(
    STW_start = if_else(cruise %in% new_cruises,
                        abs(coalesce(STW_start_new, STW_start)), STW_start),
    STW_end   = if_else(cruise %in% new_cruises,
                        abs(coalesce(STW_end_new,   STW_end)),   STW_end),
    SOG_start = if_else(cruise %in% new_cruises,
                        abs(coalesce(SOG_start_new, SOG_start)), SOG_start),
    SOG_end   = if_else(cruise %in% new_cruises,
                        abs(coalesce(SOG_end_new,   SOG_end)),   SOG_end)
  ) %>%
  select(-ends_with("_new"))
# Endeavor STW stays NA bc speedlog reports GPS-SOG, no STW

## ------------------------------------------ ##
#      EN720: overwrite v2's placeholder SOG -----
## ------------------------------------------ ##
## EN720 was hardcoded 2 kt; real per-tow SOG now available; STW == NA
# all other EN720 columns remain as published in v2
en720_speed <- ship_speed_wide %>%
  filter(cruise == "EN720") %>%
  select(cruise, station, cast, net_type,
         SOG_start_fix = SOG_start, SOG_end_fix = SOG_end)

tow_meta <- tow_meta %>%
  left_join(en720_speed, by = c("cruise", "station", "cast", "net_type")) %>%
  mutate(
    SOG_start = if_else(cruise == "EN720" & !is.na(SOG_start_fix), SOG_start_fix, SOG_start),
    SOG_end   = if_else(cruise == "EN720" & !is.na(SOG_end_fix),   SOG_end_fix,   SOG_end)
  ) %>%
  select(-SOG_start_fix, -SOG_end_fix)

## ========================================================================== ##
## 5) Fill in size_fract_20
## ========================================================================== ##
# Y if a 20um size-fraction sample exists (mesh_20_size_fract > 0), else N.
# Fill only where size_fract_20 == NA; existing (v2) values preserved
# AR99 exception: the 20um ring net became a SEPARATE deployment at AR99, so its
# bongo tows carry no 20um sample -> "N", and the ring rows carry it -> "Y".
inv_sf20 <- inventory %>%
  group_by(cruise, station, cast) %>%
  summarize(size_fract_20_inv = if_else(any(mesh_20_size_fract > 0, 
                                            na.rm = TRUE),
                                        "Y", "N"),
            .groups = "drop")

tow_meta <- tow_meta %>%
  left_join(inv_sf20, by = c("cruise", "station", "cast")) %>%
  mutate(
    size_fract_20 = case_when(
      cruise == "AR99" & net_type == "ring"  ~ "Y",
      cruise == "AR99" & net_type == "bongo" ~ "N",
      is.na(size_fract_20)                   ~ size_fract_20_inv,
      TRUE                                   ~ size_fract_20
    )
  ) %>%
  select(-size_fract_20_inv)

# Confirmed against logsheets & inventory Aug 2026 (QA vs sample inventory):
#   AR63 L4 & L7 : size_fract_20  N -> Y
#   AT46 L6      : DNA_335        Y -> N
#   HRS2303 MVCO : size_fract_20  N -> Y
#   AR38 (all)   : size_fract_20  N -> Y
tow_meta <- tow_meta %>%
  mutate(
    size_fract_20 = case_when(
      cruise == "AR63" & station %in% c("L4", "L7") ~ "Y",
      cruise == "HRS2303" & station == "MVCO"       ~ "Y",
      cruise == "AR38" & net_type == "bongo"        ~ "Y",
      TRUE ~ size_fract_20
    ),
    DNA_335 = if_else(sample_name == "AT46_L6_B6", "N", DNA_335)
  )

## ========================================================================== ##
## 6) VOLUME FILTERED   (keep flowmeter; V = A*T*S where NA)
## ========================================================================== ##
# Volume Sampled m3 meters cubed:
##   Flowmeter calibration factor = 0.26873
##   Gear Area = 0.2922 m^2
## Volume Sampled m3 = (Flowmeter revolutions) * Flow calibration factor * Gear Area (m2)

# tot_flow_counts_mesh <- flowmeter_end - flowmeter_start  # Total counts
# revolutions <- total_counts / 10  # counts to revolutions
# total flow <- revolutions * 26873  # Standard Speed Rotor Constant

#diameter_m <- 0.61  # diameter in meters
#radius_m <- diameter_m / 2  # radius in meters (0.305)
#A <- pi * radius_m^2  # area of the net mouth in square meters (0.2922)

# haul factor as specified for EcoMon & CalCOFI cruises

# Flowmeter volumes already present & verified correct (logsheet + v2) -> KEEP

# When flowmeter vol == NA -> Compute  V = A * T * S 
#   T = tow duration (s);  S = tow speed (m/s)
# Speed priority: STW_start -> STW_end -> SOG_start -> SOG_end

# plausible tow-speed window (knots); outside this -> treat as unusable
SPEED_MIN_KT <- 1
SPEED_MAX_KT <- 4

# helper: keep a speed only if it's in range, else NA (so coalesce moves on)
.valid <- function(x) if_else(!is.na(x) & abs(x) >= SPEED_MIN_KT & abs(x) <= SPEED_MAX_KT, abs(x), NA_real_)

tow_meta <- tow_meta %>%
  mutate(
    .dur_s = as.numeric(difftime(datetime_UTC_end, datetime_UTC_start, units = "secs")),
    .dur_s = if_else(.dur_s < 0, .dur_s + 24*3600, .dur_s),
    # each source screened for plausibility, then prioritized
    .speed_kt = coalesce(.valid(STW_start), .valid(STW_end),
                         .valid(SOG_start), .valid(SOG_end)),
    .speed_ms = .speed_kt * KT_TO_MS,
    .vol_calc = A_MOUTH * .dur_s * .speed_ms,
    .vol335_was_na = is.na(vol_filtered_335) & net_type == "bongo" & cruise %in% new_cruises,
    .vol150_was_na = is.na(vol_filtered_150) & net_type == "bongo" & cruise %in% new_cruises,
    vol_filtered_335 = if_else(.vol335_was_na, .vol_calc, vol_filtered_335),
    vol_filtered_150 = if_else(.vol150_was_na, .vol_calc, vol_filtered_150),
    vol_filtered_335 = if_else(net_type == "ring", NA_real_, vol_filtered_335),
    vol_filtered_150 = if_else(net_type == "ring", NA_real_, vol_filtered_150)
  )

# check which rows did not have flowmeter volume and relied on this fallback
tow_meta %>%
  filter(.vol335_was_na | .vol150_was_na) %>%
  select(cruise, station, cast, sample_name,
         .vol335_was_na, .vol150_was_na,
         .speed_kt, .dur_s, .vol_calc,
         vol_filtered_335, vol_filtered_150)

## ============================================================================ ##
## 7) HAUL FACTORS   (fill NA only; v2 kept)
## ============================================================================ ##
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

## ============================================================================ ##
## 8) DATA FLAGS   (new cruises; v2 flags preserved)
## ============================================================================ ##
## QARTOD primary-level (IOC 54:V3): 1 Good, 3 Suspect/high-interest,
## 4 Bad (failed critical), 9 Missing. 2 (not evaluated) discouraged
## primary_flag = worst-case across all conditions on the row
##
## Manual v2 changes (EN657 L1 B1, EN617 MVCO 35B, EN608)
# EN657 L1 B1, for v2 didnt have TDR so had used estimate; now have real TDR depth
# EN608 needs primary flags check
# EN617 MVCO 35B flG 3 FLOW CALIB theres another one that has it 

# Build secondary_flag from depth/volume provenance + comments, then set
# primary_flag = 3 where a secondary_flag exists (else 1). Only new cruises.

## ------------------------------------------ ##
#     Flag rules: pattern -> secondary note + severity -----
## ------------------------------------------ ##
# 3 = suspect / of high interest (derived or partial loss)
# 4 = bad (failed critical: no usable / non-quantitative sample)
flag_rules <- tribble(
  ~pattern,                                                              ~note,                                      ~level,
  "cod end broke|cod end came off|cod end leaked|leaking|loose mesh",    "cod end issue; some sample may be lost.",  3L,
  #"no ring net|no 20um ring net|ring net broke|ring net ripped|ring net.*hole|lost cod end on 20um", "no ring net / 20um sample.", 3L,
  "flowmeter for 150.*(off|not working)|flowmeter.*150um.*not working",  "150um flowmeter issue.",                   3L,
  "spill|lost.*sample|lost ~|sample was not processed",                  "some sample lost.",                        3L,
  "salps",                                                               "many salps.",                              3L,
  "non.?quantitative",                                                   "non-quantitative sample.",                 4L,
  "deployed and recovered without sample|flowmeter calibration",         "no sample.",                               4L
)

## ------------------------------------------ ##
#     Build secondary note + severity -----
## ------------------------------------------ ##
# base: depth/volume provenance + hit-bottom (negation-guarded)
# NOTE: hit bottom set to 3 usable-with-caution
tow_meta <- tow_meta %>%
  mutate(
    .sec = case_when(
      grepl("(?<!didn't )(?<!did not )(?<!not )hit bottom", comments, ignore.case = TRUE, perl = TRUE) ~ "hit bottom.",
      is.na(depth_TDR) & is.na(depth_PX) & !is.na(.tow_depth_calc) ~
        "Depth recorder (TDR) data not available. Net max depth calculated from wire information (cosine law).",
      is.na(depth_TDR) & is.na(depth_PX) & is.na(.tow_depth_calc) & !is.na(depth_target) ~
        "Target depth used for net max depth (TDR and wire data unavailable).",
      net_type == "ring" & is.na(max_wire_out) & !is.na(depth_target) ~
        "Target depth used for net max depth (wire data unavailable for ring net).",
      TRUE ~ NA_character_
    ),
    .sev = case_when(
      grepl("(?<!didn't )(?<!did not )(?<!not )hit bottom", comments, ignore.case = TRUE, perl = TRUE) ~ 3L,
      !is.na(.sec) ~ 3L,      # any depth-provenance note = derived depth = suspect
      TRUE ~ 1L
    )
  )

# comment-driven rules: append note, bump severity to worst-case
for (i in seq_len(nrow(flag_rules))) {
  hit <- grepl(flag_rules$pattern[i], tow_meta$comments, ignore.case = TRUE)
  tow_meta <- tow_meta %>%
    mutate(
      .sec = if_else(hit, str_squish(paste(coalesce(.sec, ""), flag_rules$note[i])), .sec),
      .sev = pmax(.sev, if_else(hit, flag_rules$level[i], 1L))
    )
}

# calculated-volume tows (derived, not measured) = suspect
tow_meta <- tow_meta %>%
  mutate(
    .sec = if_else(.vol335_was_na | .vol150_was_na,
                   str_squish(paste(coalesce(.sec, ""),
                                    "Volume sampled calculated from ship speed and tow duration (flowmeter unavailable).")),
                   .sec),
    .sev = pmax(.sev, if_else(.vol335_was_na | .vol150_was_na, 3L, 1L)),
    .sec = na_if(str_squish(.sec), "")
  )

# missing-data (flag 9): required field absent from every source.
# override (not pmax) and only when no other note applies, so 9 doesn't
# mask a more informative 4. 9 rows still get a note.
tow_meta <- tow_meta %>%
  mutate(
    .missing = is.na(net_max_depth) |
      (net_type == "bongo" & is.na(vol_filtered_335) & is.na(vol_filtered_150)),
    .sec = if_else(.missing & is.na(.sec),
                   #secondary_flag
                   # actual measurement of depth recorder (TDR) not available. 
                   # Net max. depth was calculated based on wire information and bottom max depth
                   "Required measurement missing (net max depth or volume filtered).", .sec),
    .sev = if_else(.missing & .sev == 1L, 9L, .sev)
  )

## ------------------------------------------ ##
#     Apply to NEW cruises only (preserve v2) -----
## ------------------------------------------ ##
tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = if_else(cruise %in% new_cruises & is.na(secondary_flag), .sec, secondary_flag),
    primary_flag   = if_else(cruise %in% new_cruises & is.na(primary_flag), .sev, primary_flag)
  )

## ------------------------------------------ ##
#     Manual v2 carve-outs (explicit; audited) -----
## ------------------------------------------ ##
# EN657 L1 B1: v2 flagged 3 (estimated depth); v3 has real TDR -> Good, clear note.
tow_meta <- tow_meta %>%
  mutate(
    primary_flag   = if_else(sample_name == "EN657_L1_B1", 1, primary_flag),
    secondary_flag = if_else(sample_name == "EN657_L1_B1", NA_character_, secondary_flag)
  )
# EN617 MVCO 35B / EN608: confirm before forcing values (see checks below).

tow_meta %>%
  filter(cruise %in% new_cruises, .sev != 1L) %>%
  select(cruise, station, cast, sample_name, net_type,
         primary_flag = .sev, secondary_flag = .sec, comments) %>%
  arrange(desc(primary_flag), cruise, station) %>%
  print(n = Inf, width = Inf)

## ------------------------------------------ ##
#     Drop working columns -----
## ------------------------------------------ ##
tow_meta <- tow_meta %>% select(-starts_with("."))

## ------------------------------------------ ##
#     Flag QA -----
## ------------------------------------------ ##
# every row must be flagged (QARTOD law 1); 2 discouraged
tow_meta %>% count(primary_flag)
tow_meta %>% filter(is.na(primary_flag)) %>% count(cruise)           # expect 0 after carve-outs
# any non-1 flag should carry a secondary note
tow_meta %>% filter(primary_flag != 1, is.na(secondary_flag)) %>%
  select(cruise, station, cast, sample_name, primary_flag)
# see the new-cruise flag distribution + example notes
tow_meta %>% filter(cruise %in% new_cruises) %>%
  count(primary_flag, secondary_flag) %>% arrange(primary_flag) %>% print(n = Inf)

## check comments for any other flags needed
# 3:
# Cod end broke == cod end broke
# Broke == cod end broke
# Cod end was tangled == tangled cod end   
# Non quantitative == non quantitative 
# Forgot to get flow start == flowmeter issue
# [H]hit bottom == hit bottom
# F[f]lowmeter [C]calibration == no sample
# No 20um ring net sample == no 20um ring net sample
# Tons of small salps == many salps in this cruise
# flowmeter reading is off == flowmeter issue
# Missing flowmeter numbers for 335um net == flowmeter issue

tow_meta %>% filter(cruise %in% new_cruises,
                    grepl("forgot to get flow|missing flowmeter numbers|tangled", comments, ignore.case = TRUE)) %>%
  select(sample_name, comments)

## ========================================================================== ##
## QA/QC
## ========================================================================== ##
# 1. new cruises populated where expected
tow_meta %>%
  filter(cruise %in% new_cruises) %>%
  group_by(cruise) %>%
  summarise(n = n(),
            stw_na   = sum(is.na(STW_start)),
            sog_na   = sum(is.na(SOG_start)),
            depth_na = sum(is.na(net_max_depth)),
            vol335_na = sum(is.na(vol_filtered_335) & net_type == "bongo"),
            haul_na  = sum(is.na(haul_factor_10m2_335) & net_type == "bongo"),
            pflag_na = sum(is.na(primary_flag)),
            .groups = "drop") %>%
  print(n = Inf)

# 2. net_max_depth rarely > depth_bottom (small overshoot ok - tow drift)
tow_meta %>%
  filter(net_max_depth > depth_bottom) %>%
  mutate(diff = net_max_depth - depth_bottom) %>%
  select(cruise, station, cast, depth_bottom, depth_TDR, depth_PX, net_max_depth, diff) %>%
  arrange(desc(diff)) %>%
  print(n = Inf)

# 3. Endeavor STW still all NA (sanity - should be TRUE)
tow_meta %>% filter(str_starts(cruise, "EN")) %>%
  summarise(endeavor_stw_all_na = all(is.na(STW_start)))

# 4. haul factors finite (no /0 from zero volume)
tow_meta %>%
  filter(is.infinite(haul_factor_100m3_335) | is.infinite(haul_factor_10m2_335)) %>%
  select(cruise, station, cast, vol_filtered_335, net_max_depth)

# 5. volume range sane per cruise (bongo only)
tow_meta %>%
  filter(net_type == "bongo") %>%
  group_by(cruise) %>%
  summarise(min_v = min(vol_filtered_335, na.rm = TRUE),
            max_v = max(vol_filtered_335, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  print(n = Inf)

tow_meta %>%
  filter(!is.na(net_max_depth)) %>%
  group_by(cruise) %>%
  summarise(
    min_depth = min(net_max_depth, na.rm = TRUE),
    max_depth = max(net_max_depth, na.rm = TRUE),
    .groups = "drop"
  )

## ------------------------------------------ ##
#      Write -----
## ------------------------------------------ ##
stamp <- format(Sys.Date(), "%Y%m%d")
write_csv(tow_meta, here("data", "processed",
                         glue::glue("nes-lter-zooplankton-tow-metadata-v3-{stamp}.csv")))
