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
# EN608, AR28B, EN617 and some older cruises dont have ending coordinates
# recover action not recorded on elog back them

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

# check which rows did not have flowmeter volume and used fallback approach
tow_meta %>%
  filter(.vol335_was_na | .vol150_was_na) %>%
  select(cruise, station, cast, sample_name,
         .vol335_was_na, .vol150_was_na,
         .speed_kt, .dur_s, .vol_calc,
         vol_filtered_335, vol_filtered_150)

# AR63 L5 B2 (v2): valid counts but vol_filtered published NA
# recompute volume from counts
tow_meta <- tow_meta %>%
  mutate(
    vol_filtered_335 = if_else(sample_name == "AR63_L5_B2" & is.na(vol_filtered_335),
                               tot_flow_counts_335 * 0.026873 * A_MOUTH,
                               vol_filtered_335),
    vol_filtered_150 = if_else(sample_name == "AR63_L5_B2" & is.na(vol_filtered_150),
                               tot_flow_counts_150 * 0.026873 * A_MOUTH,
                               vol_filtered_150)
  )

## ========================================================================== ##
## 7) HAUL FACTORS   
## ========================================================================== ##
# fill NAs only; v2 kept
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

## ========================================================================== ##
## 8) DATA FLAGS   (new cruises; v2 flags preserved)
## ========================================================================== ##
## QARTOD primary-level (IOC 54:V3): 
##    1 == Good
##    3 == Suspect/high-interest (derived or partial loss)
##    4 == Bad (failed critical)
##    9 == Missing
## 2 (not evaluated) discouraged
## primary_flag = worst-case across all conditions on the row
#
# Build secondary_flag from depth/volume provenance + comments, then set
# primary_flag = 3 where a secondary_flag exists (else 1). Only new cruises.

## ------------------------------------------ ##
#     Flag rules: pattern -> secondary note + severity -----
## ------------------------------------------ ##
flag_rules <- tribble(
  # pattern = looks regex in the comments col 
  # note    = secondary-flag = human-readable
  # level   = QARTOD severity = primary flag
  ~pattern,                                                             ~note,                      ~level,
  "cod end broke|cod end came off|cod end leaked|leaking|loose mesh",   "Cod end issue.",           3L,
  "flowmeter for 150.*(off|not working)|flowmeter.*150um.*not working", "150um flowmeter issue.",   3L,
  # "spill|lost.*sample|lost ~",                                          "Partial sample spilled.",  3L,
  "salps",                                                              "Salps abundant.",          3L,
  "non.?quantitative",                                                  "Non-quantitative sample.", 4L
  # "deployed and recovered without sample|flowmeter calibration",        "No sample.",               4L
)

# preview how many new cruise rows each rule above hits 
for (i in seq_len(nrow(flag_rules))) {
  n <- tow_meta %>%
    filter(cruise %in% new_cruises,
           # rows matching rule i
           grepl(flag_rules$pattern[i], comments, ignore.case = TRUE)) %>% 
    nrow()
  cat(sprintf("%-28s -> %d rows\n", flag_rules$note[i], n))
}

## ------------------------------------------ ##
#     Build secondary note + severity -----
## ------------------------------------------ ##
# base: depth/volume provenance + hit-bottom (negation-guarded)
# hit bottom == 3 == usable with caution

# .sec  = secondary note = the note text (NA if nothing applies)
# .sev  = severity = 1 Good / 3 Suspect / 4 Bad / 9 Missing
tow_meta <- tow_meta %>%
  mutate(
    .sec = case_when(
      # 1) hit bottom
      grepl("(?<!didn't )(?<!did not )(?<!not )hit bottom", comments, ignore.case = TRUE, perl = TRUE) ~ "hit bottom.",
      # 2) no TDR and no PX, but wire/cosine gave a depth -> depth is derived
      is.na(depth_TDR) & is.na(depth_PX) & !is.na(.tow_depth_calc) ~
        "Depth recorder (TDR) and PX sensor data not available. Net max depth calculated from wire information (cosine law).",
      # 3) no TDR/PX/wire either -> used target depth
      is.na(depth_TDR) & is.na(depth_PX) & is.na(.tow_depth_calc) & !is.na(depth_target) ~
        "Target depth used for net max depth (TDR, PX, and wire data unavailable).",
      # 4) ring net with no wire data -> target depth
      net_type == "ring" & is.na(max_wire_out) & !is.na(depth_target) ~
        "Target depth used for net max depth (wire data unavailable for ring net).",
      TRUE ~ NA_character_
    ),
    .sev = case_when(
      grepl("(?<!didn't )(?<!did not )(?<!not )hit bottom", comments, ignore.case = TRUE, perl = TRUE) ~ 3L,
      !is.na(.sec) ~ 3L, # any depth-provenance note = derived depth = suspect
      TRUE ~ 1L # clean row -> Good (1)
    )
  )

## --- Apply comment-driven rules --- ##
# looping so multiple rules can append independently 
# append note, bump severity to worst-case
for (i in seq_len(nrow(flag_rules))) {
  hit <- grepl(flag_rules$pattern[i], tow_meta$comments, ignore.case = TRUE)
  tow_meta <- tow_meta %>%
    mutate(
      .sec = if_else(hit, str_squish(paste(coalesce(.sec, ""), 
                                           flag_rules$note[i])), .sec),
      .sev = pmax(.sev, if_else(hit, flag_rules$level[i], 1L))
    )
}

## new-cruise spills: append net-specific spill note. hand-mapped - comments
## are tangled and most rows already carry a cod-end/flowmeter note.
spill_335 <- c("EN727_L9_B11", "AR88_L9_B15", "AR92_L11_B11", "AR95_L1_B1")
spill_150 <- c("AR88_L3_B18", "AR88_L2_B19", "AR95_L9_B16", "AR99_L1_B2")

tow_meta <- tow_meta %>%
  mutate(
    .spill_note = case_when(
      sample_name %in% spill_335 ~ "Partial 335 micron sample spilled.",
      sample_name %in% spill_150 ~ "Partial 150 micron sample spilled.",
      TRUE ~ NA_character_
    ),
    # append onto existing note (cod-end / flowmeter) rather than replace;
    .sec = if_else(!is.na(.spill_note),
                   str_squish(paste(coalesce(.sec, ""), .spill_note)), .sec),
    .sec = na_if(.sec, "")
  ) %>%
  select(-.spill_note)

tow_meta %>%
  filter(cruise %in% new_cruises) %>%
  arrange(desc(.sev), cruise, station, cast) %>%
  select(sample_name, net_type,
         .sev, .sec, comments) %>%
  print(n = Inf, width = Inf)
tow_meta %>%
  filter(cruise %in% new_cruises, .sev != 1L) %>%
  count(.sev, .sec) %>%
  arrange(desc(.sev)) %>%
  print(n = Inf)
tow_meta %>%
  filter(cruise %in% new_cruises,
         (!is.na(.sec) & .sev == 1L) | (.sev != 1L & is.na(.sec))) %>%
  select(cruise, station, cast, .sev, .sec, comments)
tow_meta %>% filter(cruise %in% new_cruises) %>%
  select(cruise, station, cast, sample_name, net_type, .sev, .sec, comments) %>%
  arrange(desc(.sev)) %>% View("new_cruise_flags")

# --- calculated volume (derived, not measured w flowmeter) = suspect --- ##
# this only catches truly missing flowmeter vals, but if flowmeter wrong/bad
# not caught here -- that is checked in script 05... 
tow_meta <- tow_meta %>%
  mutate(
    .vol_note = case_when(
      .vol335_was_na & .vol150_was_na ~ "Volume sampled (335 and 150 um) calculated from ship speed and tow duration (flowmeter unavailable).",
      .vol335_was_na                  ~ "Volume sampled (335 um) calculated from ship speed and tow duration (flowmeter unavailable).",
      .vol150_was_na                  ~ "Volume sampled (150 um) calculated from ship speed and tow duration (flowmeter unavailable).",
      TRUE ~ NA_character_
    ),
    .sec = if_else(!is.na(.vol_note),
                   str_squish(paste(coalesce(.sec, ""), .vol_note)), .sec),
    .sev = pmax(.sev, if_else(.vol335_was_na | .vol150_was_na, 3L, 1L)),
    .sec = na_if(str_squish(.sec), "")
  )

# missing-data flag 9
# override (not pmax) and only when no other note applies, so 9 doesn't
# mask a more informative flag
tow_meta <- tow_meta %>%
  mutate(
    .missing = is.na(net_max_depth) |
      (net_type == "bongo" & is.na(vol_filtered_335) & is.na(vol_filtered_150)),
    .sec = if_else(.missing & is.na(.sec),
                   #secondary_flag
                   # TDR not available
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

####!!!!!NEED TO MAKE SURE THAT THE FIXES DONT OVERWRITE primary flag if not intending to 

## ------------------------------------------ ##
#     Manual v2 fixes -----
## ------------------------------------------ ##

## --- a) remove "no 20um ring net sample" --- ##
tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = str_remove(secondary_flag,
                                regex("\\s*no 20\\s*um ring net sample\\.?",
                                      ignore_case = TRUE)),
    secondary_flag = na_if(str_squish(secondary_flag), "")
  )

## --- b) non-quantitative --- ##
# change primary flag to 4; catch all no quant in comments (missed a couple in v2)
# double check tht AR63 L2 abd AR38 L6 got it
tow_meta <- tow_meta %>% 
  mutate(
    .nq_noted   = grepl("non.?quantitative", secondary_flag, ignore.case = TRUE),
    .nq_comment = grepl("non.?quantitative", comments,       ignore.case = TRUE),
    .nq_any     = .nq_noted | .nq_comment,          # any non-quant signal at all
    .nq_add     = .nq_comment & !.nq_noted,         # in comments but not yet noted
    .nq_note = case_when(                           # built only for rows needing it
      !.nq_add                                        ~ NA_character_,
      grepl("335", comments) & grepl("150", comments) ~ "Non quantitative 335 and 150 micron sample.",
      grepl("335", comments)                          ~ "Non quantitative 335 micron sample.",
      grepl("150", comments)                          ~ "Non quantitative 150 micron sample.",
      TRUE                                            ~ "Non quantitative sample."
    )
  )

# VERIFY the auto-detected rows 
tow_meta %>%
  filter(.nq_add) %>%
  select(cruise, station, cast, sample_name, .nq_note, secondary_flag, comments) %>%
  print(n = Inf, width = Inf)

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = if_else(.nq_add,                    # append where missing
                             str_squish(paste(coalesce(secondary_flag, ""), .nq_note)),
                             secondary_flag),
    primary_flag   = if_else(.nq_any, 4, primary_flag)   # all non-quant -> 4
  ) %>%
  select(-starts_with(".nq"))

## --- c) "no sample" -> primary = 9 (missing) --- ##
# No sample flag -> 9, not 3.
no_sample_comments <- c(
  "Cod end broke - no sample. OOI Pioneer cruise.",
  "Deployed and recovered without sample. Conditions too rough to re do cast."
)  

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = case_when(
      comments == "Cod end broke - no sample. OOI Pioneer cruise." ~ "Cod end issue. No sample collected.",
      comments == "Deployed and recovered without sample. Conditions too rough to re do cast." ~ "No sample collected.",
      TRUE ~ secondary_flag
    ),
    primary_flag = if_else(comments %in% no_sample_comments, 9, primary_flag)
  )

## --- add missing "Tangled cod end" --- ##
## needs: Tangled cod end.
# Twist in net bewtween the ring and the cod end. OOI Pioneer cruise.
# Cod end of ring net got tangled around the wire, not sure at what point of the tow it happened. No time to redo. Saved what was collected. OOI Pioneer cruise.
## --- tangled cod end: AR66B L10 R6, AR34B L10 R21 --- ##
tangled_ids <- c("AR66B_L10_R6", "AR34B_L10_R21")

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = if_else(sample_name %in% tangled_ids,
                             str_squish(paste(coalesce(secondary_flag, ""), "Tangled cod end.")),
                             secondary_flag),
    primary_flag   = if_else(sample_name %in% tangled_ids,
                             pmax(coalesce(primary_flag, 1L), 3L),
                             primary_flag)
  )

## --- add "Salps abundant." --- ##
# There were a lot of salps in both 335 and 150 um nets. The total amount sampled was 5.6 L for 335 um net and 5.85L for 150 um net. A subsample of 500mL was taken from each net to then be split as usual.
# Salpy but splitable.
# Salpy but not bad as as L4. (this one already has a primary flag to add to it)
# Many salps and large organisms in 150um net, had to remove prior to putting samples in the SIA jars.
# Lots of salps. OOI Pioneer cruise.
# Lots of salps in sample wouldnt fit into the size fractionated jars. Hardly any sample between 500-1000um and 500-200 um from 150um net.
# Half salps. OOI Pioneer cruise.
# Got SALPED. Removed salps prior to processing because there were way too many. 335 um approx. 700 mL of salp removed. 150 um approx. 1L of salp removed. Not sure if there is enough >1000 for SIA but saved some anyways barely anything in 100 um size fraction.
# Depth 82m. Salpy but able to split. Change ETOH at 01:20 UTC on 21JUL.
# CTD worked went down to 200m. Very salpy sample. Some myctophiids in sample.
# 335 split into 3 jars (all salps and jelly). Dumped 21/2 150 um purely salps.
# 11 calanus from 335. Lots of salps. Winds 15-20 kts. ETOH samples smelling bad salps?
salps_abundant <- c("AR38_L4_B14", "AR77_L10_B6", "EN712_L10_B7", "EN720_L4_B6",
                    "AR28B_L11_R12", "AR34B_L8_R9", "AR38_L3_B16",
                    "EN668_L10_B8", "EN668_L6_B18", "EN687_L4_B5",
                    "EN706_L10_B10", "EN715_L10_B7")

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = case_when(
      sample_name %in% salps_abundant ~ str_squish(paste(coalesce(secondary_flag, ""), "Salps abundant.")),
      TRUE ~ secondary_flag
    ),
    primary_flag = if_else(sample_name %in% c(salps_abundant),
                           pmax(coalesce(primary_flag, 1L), 3L), primary_flag)
  )

## --- add "Some sample lost." --- ##
lost_335 <- c(
  "AR38_L9_B10",     # "335 um 5-10 ml spilled."
  "EN695_L10_B4",    # "Spilled small amount of 335 net... <100ml."
  "AR77_L1_B1",      # "Net sat at surface...335um cod end spilled 1/6 sample."
  "AR88_L9_B15"      # "Spilled 2mL of 335 micron net..."
)

lost_150 <- c(
  "EN608_L2_B4",     # "Spilled about 5% of 150um sample..."
  "AR32_L3_B2",      # "Dumped 1/2 of 150 um..."
  "AR32_L5_B3",      # "Part of 150 um spilled about 5%..."
  "AR38_L3_B2",      # "5% of 150 um spilled."
  "EN687_L6_B18",    # "Spilled 1/2 of 150, used for SIA and taxa..."
  "HRS2303_L3_B11"   # "Lost 5mL of 150um sample."
)

lost_generic <- c(
  "AR32_L6_B4"      # "Dumped 1/2 of sample." (no mesh named)
)

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = case_when(
      sample_name %in% lost_335     ~ str_squish(paste(coalesce(secondary_flag, ""), "Partial 335 micron sample spilled.")),
      sample_name %in% lost_150     ~ str_squish(paste(coalesce(secondary_flag, ""), "Partial 150 micron sample spilled.")),
      sample_name %in% lost_generic ~ str_squish(paste(coalesce(secondary_flag, ""), "Partial sample spilled.")),
      TRUE ~ secondary_flag
    ),
    primary_flag = if_else(sample_name %in% c(lost_335, lost_150, lost_generic),
                           pmax(coalesce(primary_flag, 1L), 3L), primary_flag)
  )
##!!!!! CHECK AR88 L9 B15 getting double lbel.
tow_meta %>%
  filter(sample_name %in% c(lost_335, lost_150, lost_generic,
                            "EN661_L11_B10", "AT46_L7_B14", "AR88_L9_B15")) %>%
  select(sample_name, primary_flag, secondary_flag, comments) %>%
  print(n = Inf, width = Inf)

# ----- hit bottom kept vs hit bottom no sample 
# hit bottom kept = 3; many of these already have 3 good, but hit bottom should change to Hit bottom sample kept.
## --- hit bottom, no sample -> 9 --- ##
hb_nosample <- c(
  "EN655_L9_B15",# "No sample. Hit bottom. No time to re do."(change from 3->9; change secondary from hit bottom to Hit bottom no sample.)
  "EN712_L6_B5"  # "Hit bottom. Collected a lot of silty sediment..." (bare -> replace)
)

## --- hit bottom, sample kept -> 3 --- ##
hb_kept <- c(
  "EN687_L8_B16",     # "Programmed TDR... Hit bottom."        (has volume note -> append)
  "AR77_L8_B16", # "Hit bottom. Sand and shells in cod end. Ring net sample suspect - mostly sand and silt."
  "AR77_L7_B17",      # "Sand in sample. Hit bottom."          (bare "hit bottom." -> replace)
  "EN720_L3_B20",     # "Hit bottom. Sieved out as much sand as possible..." (bare "hit bottom." -> replace)
  "EN720_MVCO_B24",    # "Sand in the sample. TDR depth not near bottom..." (NA -> set),
  "EN655_L7_B11", # 335 net skimmed the bottom. kept 150 only
  "AT46_L4_B2",    #"335 Hit botttom. some sample kept"
  "EN627_L1_B3" # Hit bottom bongo; only 20um size-fraction sample kept; Station resampled.
)

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = case_when(
      sample_name %in% hb_nosample ~ "Hit bottom. No sample collected.",
      sample_name %in% hb_kept & (is.na(secondary_flag) | secondary_flag == "hit bottom.") ~ "Hit bottom. Sample kept.",
      sample_name %in% hb_kept ~ str_squish(paste(coalesce(secondary_flag, ""), "Hit bottom; some sample kept.")),
      TRUE ~ secondary_flag
    ),
    primary_flag = case_when(
      sample_name %in% hb_nosample ~ 9,
      sample_name %in% hb_kept     ~ pmax(coalesce(primary_flag, 1L), 3L),
      TRUE ~ primary_flag
    )
  )

## --- CALIBRATION = 9 --- #
# For flowmeter calibration (and bluefin larvae). Tow yo to 10 m for at least a nm and bridge marked when net in and out. First tow (A) in one direction and second tow (B) in opposite direction. Distance of this tow was 1.099 nm, which is 2035 m. BUT, as I was putting all the numbers together, I realized that the 2nd tow was throwing things off, and it was because they towed with the wind and much faster through the water than the more typical tow (which tow A was). So we are scrapping the calibration from these stations and will tow for a known distance at MVCO (just once because of little current).
# Flowmeter calibration.
# Flowmeter calibration tow (sample not kept). Again the 335 count was higher. But this time the 150 was so low (half the 335) that something must have made it get stuck.
calib_ids <- c("EN617_L11_B25A", "EN617_L11_B25B", "EN617_MVCO_B35B")

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = if_else(sample_name %in% calib_ids,
                             "Calibration tow; no sample collected.", secondary_flag),
    primary_flag   = if_else(sample_name %in% calib_ids, 9, primary_flag)
  )

tow_meta <- tow_meta %>%
  mutate(secondary_flag = if_else(secondary_flag == "many salps in this cruise.",
                                  "Salps abundant.", secondary_flag))

# EN715_L8_B14 should be 1 not 3
tow_meta <- tow_meta %>%
  mutate(
    primary_flag   = if_else(sample_name == "EN715_L8_B14", 1, primary_flag),
    secondary_flag = if_else(sample_name == "EN715_L8_B14", NA_character_, secondary_flag)
  )

## --- row-specific v2 changes
# EN608      : needs flags
# EN657 L1 B1: v2 didnt have TDR so used estimate; now have TDR (change flag 3 to 1)

tow_meta <- tow_meta %>%
  mutate(
    # EN608 - never assigned a flag in v2; fill NA -> 1
    primary_flag = if_else(cruise == "EN608" & is.na(primary_flag), 1, primary_flag),
    
    # EN657 L1 B1 - now have real TDR (v2 used estimate) -> 1, clear note
    secondary_flag = if_else(sample_name == "EN657_L1_B1", NA_character_, secondary_flag),
    primary_flag   = if_else(sample_name == "EN657_L1_B1", 1, primary_flag)
  )

## --- fix sentence-case secondary_flag --- ##
sentence_case_notes <- function(x) {
  if (is.na(x)) return(NA_character_)
  parts <- str_split(x, "(?<=\\.)\\s+")[[1]]                  # split after ". "
  parts <- str_replace(parts, "^\\s*(\\p{L})", \(m) toupper(m))
  str_squish(str_c(parts, collapse = " "))
}
tow_meta <- tow_meta %>%
  mutate(secondary_flag = map_chr(secondary_flag, sentence_case_notes))

tow_meta %>% count(primary_flag)
tow_meta %>% distinct(primary_flag, secondary_flag) %>%
  arrange(primary_flag) %>% print(n = Inf, width = Inf)



##v3
# didnt catch
# Pxsensor used; flowmeter for 150 um net is off; no ring net; 335um net had a leak, so some sample might have been lost
# 3
# 150um flowmeter issue.
#
# needs no sample 335 	; can keep what it has 
# Px sensor used; flowmeter for 150 um net is off; no ring net; 335um cod end broke off into the ocean during recovery, so no sample
#
## 

## Capitalize/sentence case the flags
# volume sampled calculated based on ship speed and winch wire angle not flowmeter.
# should this be changed to specifty net ....? 
## change non quantitative from 3 to 4 

# We hit bottom on the first deployment, but we were able to run bongo again. TDR was tested on CTD cast. CTD = 135.7m and TDR = 132.7m, so there is a 3m offset on TDR readings.
# currently has flag 3 == hit bottom = should be 1 

# double check:
# EN715 5, 6, 8
# check that sample lost is coherent == for when spilled not completely NA
# AR63 L2 and AR38 L6 = non quant flags 
# PX sensor stuff cant apply to old cruises
# check NA flags + secondary 
# what about corrected 335/150 flowmeter value & Fixed 150 um flo...
# v2 = many salps in this cruise sould be abundant salps 
# EN712 why so many vol sampled based on speeds
# check en720 L11B11 vol sampled i have speeds now why not flowmeter
##########################################################################################

## ========================================================================== ##
## v2 harmonization: net-specific calculated-volume notes
## ========================================================================== ##
# v2 = "...calculated based on ship speed..." note doesn't specify which net 
# v2 script volume_water_sampled_calculations.R L536-563
# substituted calculated volume per-net on explicit station lists
# EN627 = all stations

# add net specificity to comments/data flags 

# 335 substitutions (L536-544)
calc_335_keys <- tribble(
  ~cruise,    ~station,
  "AR38",     "L3",
  "EN617",    "L11",
  "EN715",    "L6",
  "HRS2303",  "L9",
  "EN720",    "L11",
  "EN712",    "L8",
  "EN668",    "L2",
  "EN668",    "L3",
  "EN668",    "L4"
)

# 150 substitutions (L549-560)
calc_150_keys <- tribble(
  ~cruise,    ~station,
  "EN617",    "L11",
  "HRS2303",  "L9",
  "EN720",    "L11",
  "EN712",    "L1",
  "EN712",    "L7",
  "EN712",    "L9",
  "EN712",    "L11",
  "EN712",    "L8",
  "EN715",    "L6",
  "EN706",    "L5",
  "EN668",    "L2",
  "EN668",    "L3",
  "EN668",    "L4",
  "EN687",    "L8",
  "EN657",    "MVCO"
)

.key <- function(cr, st) paste(cr, st)

tow_meta <- tow_meta %>%
  mutate(
    # per-net: was this net's v2 volume the calculated one?
    # explicit station list  OR  the EN627-all  OR  the DNA_335=="Y" NA-fill branch
    .v2_calc_335 = !cruise %in% new_cruises & net_type == "bongo" & (
      .key(cruise, station) %in% .key(calc_335_keys$cruise, calc_335_keys$station) |
        cruise == "EN627" |
        (DNA_335 == "Y" & !is.na(vol_filtered_335))     # L545 fallback (NA-vol rows it caught)
    ),
    .v2_calc_150 = !cruise %in% new_cruises & net_type == "bongo" & (
      .key(cruise, station) %in% .key(calc_150_keys$cruise, calc_150_keys$station) |
        cruise == "EN627" |
        (DNA_335 == "Y" & !is.na(vol_filtered_150))     # L561 fallback (note: keyed on DNA_335, not DNA_150 — matches v2)
    ),
    .v2_vol_note = case_when(
      .v2_calc_335 & .v2_calc_150 ~ "Volume sampled (335 and 150 um) calculated from ship speed and tow duration; flowmeter reading unreliable.",
      .v2_calc_335                ~ "Volume sampled (335 um) calculated from ship speed and tow duration; flowmeter reading unreliable.",
      .v2_calc_150                ~ "Volume sampled (150 um) calculated from ship speed and tow duration; flowmeter reading unreliable.",
      TRUE ~ NA_character_
    ),
    # swap the flat v2 string for the net-specific one
    secondary_flag = if_else(!is.na(.v2_vol_note),
                             .v2_vol_note,
                             secondary_flag)
  )

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
