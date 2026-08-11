################################################################################
## Script:  03_bongo_logs_merge.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Merge all bongo net event log datasheets from new cruises
##          (2024-2026) with published v2 inventory metadata, producing
##          a combined dataset for v3 EDI package (2018-2026 cruises).
##
##  This adds data from AE2426 (fall 2024) to CRUISENAME
##        v2 had up to EN720
##
## Inputs (data/raw/):
##   - bongo_logs/*.csv                          (new cruise bongo logsheets)
##   - nes-lter-zooplankton-tow-metadata-v2.csv  (EDI: knb-lter-nes.24.2)
##   - px_data_bongo_2026-06-18.rds              (EDI:)
##   - elog_zoop_tows_thruHRS2601_2026-08-10.csv (nes-lter-api-pulls.Rproj)
##   - tdr_data_no_offset_2026-08-11.rds         (EDI:)
##
## Outputs (data/processed/):
##   - nes-lter-bongologs-{last_cruise}-YYYYMMDD.csv / .rds
##
## Ring net only (no bongo): AR31A, AR39B, AR34B, AR28B, AR66B, AR61B
## New columns added starting EN720: ship_speed_kts, EtOHchanged
##
## Separate bongo and ring net tows starting: AR99 (winter 2026)
## PX sensor depth starting: AE2426 (fall 2024)
##
## v2 package downloaded: 07-NOV-2025
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2
## created JUN-2024 | updated MAY-2026
################################################################################

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(readr) 
library(dplyr)
library(lubridate)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

# --- data from inventory package v2 -----
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2
tow_meta_v2 <- read_csv(file.path("data", "raw",
                                  "nes-lter-zooplankton-tow-metadata-v2.csv"))

# --- newer data
# --- raw bongo data sheets for new cruises to add to v3 pack -----
# directory for CSV files with bongo logs
directory <- here("data", "raw", "bongo_logs")

csv_files <- list.files(directory, pattern = "\\.csv", full.names = TRUE)

# read and clean each file
read_and_clean_csv <- function(file) {
  df <- read_csv(file, na = "-")
  
  # list of columns to ensure are character type
  columns_to_convert <- c("cast", "dateUTC_yymmdd", "time_start_UTC")
  
  for (col in columns_to_convert) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  # ensure all columns are of consistent type
  df[] <- lapply(df, function(x) {
    if (is.numeric(x) && any(is.na(as.numeric(x)))) {
      return(as.character(x))
    } else if (is.character(x)) {
      return(x)
    } else if (inherits(x, "time")) {
      return(as.character(x))
    } else {
      return(as.character(x))
    }
  })
  
  return(df)
}

# merge all files
list_of_dataframes <- lapply(csv_files, read_and_clean_csv)

# make df
combined_dataframe <- bind_rows(list_of_dataframes)

class(combined_dataframe)
combined_dataframe <- as.data.frame(combined_dataframe)

## ------------------------------------------ ##
##  Clean combined_dataframe for rbind with tow_meta_v2
## ------------------------------------------ ##

## --- 1) remove blank rows --- ##
combined_dataframe <- combined_dataframe %>%
  filter(!is.na(cruise), cruise != "",
         !is.na(station), station != "")

## --- 2) rename columns to match tow_meta_v2 --- ##
combined_dataframe <- combined_dataframe %>%
  rename(
    latitude_start    = lat_start,
    longitude_start   = lon_start,
    max_wire_out      = max_wire_out_m,
    wire_rate_out     = wire_rate_out_m_min,
    wire_rate_in      = wire_rate_in_m_min,
    vol_filtered_335  = vol_filtered_m3_335,
    vol_filtered_150  = vol_filtered_m3_150
  )

## --- 3) build datetime_UTC_start and datetime_UTC_end --- ##
# dateUTC_yymmdd is "241106" = YYMMDD
combined_dataframe <- combined_dataframe %>%
  mutate(
    date_parsed = as.Date(dateUTC_yymmdd, format = "%y%m%d"),
    datetime_UTC_start = as.POSIXct(
      paste(date_parsed, time_start_UTC),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    ),
    datetime_UTC_end = as.POSIXct(
      paste(date_parsed, time_end_UTC),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    )
  ) %>%
  select(-date_parsed, -dateUTC_yymmdd, -time_start_UTC, -time_end_UTC)

## --- 4) add columns present in tow_meta_v2 but missing here + fill with NA ---
missing_cols <- setdiff(names(tow_meta_v2), names(combined_dataframe))
message(glue::glue("Adding {length(missing_cols)} NA columns: {paste(missing_cols, collapse = ', ')}"))

combined_dataframe[missing_cols] <- NA

## --- 5) drop columns not in tow_meta_v2 --- ##
extra_cols <- setdiff(names(combined_dataframe), names(tow_meta_v2))
message(glue::glue("Dropping {length(extra_cols)} extra columns: {paste(extra_cols, collapse = ', ')}"))
combined_dataframe <- combined_dataframe %>% select(-any_of(extra_cols))

## --- 6) reorder columns to match tow_meta_v2 --- ##
combined_dataframe <- combined_dataframe %>%
  select(all_of(names(tow_meta_v2)))

# sanity check
message(glue::glue("combined_dataframe: {nrow(combined_dataframe)} rows, {ncol(combined_dataframe)} cols"))
message(glue::glue("tow_meta_v2:        {nrow(tow_meta_v2)} rows, {ncol(tow_meta_v2)} cols"))
identical(names(combined_dataframe), names(tow_meta_v2))

## replace 	N/A for Rs NA in avg angle col
# replace "N/A", "NA", "", "0" strings and actual 0s with NA

combined_dataframe <- combined_dataframe %>%
  # replace text variants of NA
  mutate(across(where(is.character),
                ~ na_if(., "N/A") %>%
                  na_if("NA") %>%
                  na_if(""))) %>%
  # force numeric on flow/volume columns
  mutate(across(c(flow_start_335, flow_end_335, tot_flow_counts_335,
                  vol_filtered_335, flow_start_150, flow_end_150,
                  tot_flow_counts_150, vol_filtered_150),
                ~ suppressWarnings(as.numeric(.)))) %>%
  # replace 0s with NA in those same columns
  mutate(across(c(flow_start_335, flow_end_335, tot_flow_counts_335,
                  vol_filtered_335, flow_start_150, flow_end_150,
                  tot_flow_counts_150, vol_filtered_150),
                ~ if_else(.x == 0, NA_real_, .x)))

## couple of no-sample rows (bad weather,etc) need to add NAs to fields
# columns that can carry a value even on an otherwise-empty placeholder row
id_cols <- c("cruise", "station", "sample_name",
             "flowmeter_sn_335", "flowmeter_sn_150", "comments")

# TRUE where every OTHER column is NA
empty_row <- rowSums(!is.na(combined_dataframe[setdiff(names(combined_dataframe), id_cols)])) == 0
sum(empty_row)         

combined_dataframe <- combined_dataframe %>%
  mutate(across(c(sample_name, flowmeter_sn_335, flowmeter_sn_150),
                ~ if_else(empty_row, NA_character_, as.character(.))))

## ------------------------------------------ ##
##  check data
## ------------------------------------------ ##
## --- cruise and station counts ---
# expected ~12 per cruise 
combined_dataframe %>%
  count(cruise, sort = FALSE) 

## --- which stations per cruise ---
combined_dataframe %>%
  group_by(cruise) %>%
  summarise(stations = paste(sort(station), collapse = ", "),
            n = n(), .groups = "drop") 

## --- tow duration ---
# flag anything < 3 min or > 60 min
combined_dataframe %>%
  filter(!is.na(datetime_UTC_start), !is.na(datetime_UTC_end)) %>%
  mutate(
    duration_min = as.numeric(difftime(datetime_UTC_end, 
                                       datetime_UTC_start, units = "mins"))
  ) %>%
  select(cruise, station, cast, datetime_UTC_start, datetime_UTC_end, 
         duration_min) %>%
  arrange(duration_min) 

# end before start (negative duration)
neg_dur <- combined_dataframe %>%
  filter(!is.na(datetime_UTC_start), !is.na(datetime_UTC_end)) %>%
  mutate(duration_min = as.numeric(difftime(datetime_UTC_end,
                                            datetime_UTC_start,
                                            units = "mins"))) %>%
  filter(duration_min < 0)

if (nrow(neg_dur) > 0) {
  message("!! End before start:")
  neg_dur %>%
    select(cruise, station, cast, datetime_UTC_start, 
           datetime_UTC_end, duration_min) 
} else {
  message("No negative durations")
}
# ar88 fixed below 

# long tows > 60 min
long_tows <- combined_dataframe %>%
  filter(!is.na(datetime_UTC_start), !is.na(datetime_UTC_end)) %>%
  mutate(duration_min = as.numeric(difftime(datetime_UTC_end,
                                            datetime_UTC_start,
                                            units = "mins"))) %>%
  filter(duration_min > 60)

if (nrow(long_tows) > 0) {
  message("!! Suspiciously long tows (> 60 min):")
  long_tows %>%
    select(cruise, station, cast, datetime_UTC_start,
           datetime_UTC_end, duration_min) 
} else {
  message("No suspiciously long tows")
}

# missing datetimes
combined_dataframe %>%
  group_by(cruise) %>%
  summarise(
    n_missing_start = sum(is.na(datetime_UTC_start)),
    n_missing_end   = sum(is.na(datetime_UTC_end)),
    n_total         = n(),
    .groups = "drop"
  ) %>%
  filter(n_missing_start > 0 | n_missing_end > 0) %>%
  print(n = Inf)

# missing start but not end or vice versa
combined_dataframe %>%
  filter(xor(is.na(datetime_UTC_start), is.na(datetime_UTC_end))) %>%
  select(cruise, station, cast, datetime_UTC_start, datetime_UTC_end)

# depth checks
combined_dataframe %>%
  filter(!is.na(net_max_depth)) %>%
  group_by(cruise) %>%
  summarise(
    min_depth = min(net_max_depth, na.rm = TRUE),
    max_depth = max(net_max_depth, na.rm = TRUE),
    .groups = "drop"
  )

# station name check 
sort(unique(combined_dataframe$station))

# cast number check 
sort(unique(combined_dataframe$cast))

# print full table sorted by cruise and station for manual review
combined_dataframe %>%
  select(cruise, station, cast, datetime_UTC_start, datetime_UTC_end,
         latitude_start, longitude_start, net_max_depth) %>%
  arrange(cruise, station) %>%
  print()

lapply(combined_dataframe, unique)

## ------------------------------------------ ##
##    Fix col types 
## ------------------------------------------ ##
# combined_dataframe <- combined_dataframe %>%
#   mutate(across(c(latitude_start, longitude_start, latitude_end, longitude_end,
#                   depth_bottom, depth_target, depth_TDR, net_max_depth,
#                   avg_angle, max_wire_out, wire_rate_out, wire_rate_in,
#                   STW_start, SOG_start, STW_end, SOG_end,
#                   flowmeter_sn_335, flowmeter_sn_150,
#                   haul_factor_10m2_335, haul_factor_10m2_150,
#                   haul_factor_100m3_335, haul_factor_100m3_150),
#                 ~ suppressWarnings(as.numeric(.))))
## Coerce to match tow_meta_v2 column types 
num_cols <- c("latitude_start", "longitude_start", "latitude_end", "longitude_end",
              "depth_bottom", "depth_target", "depth_TDR", "net_max_depth",
              "avg_angle", "max_wire_out", "wire_rate_out", "wire_rate_in",
              "STW_start", "SOG_start", "STW_end", "SOG_end",
              "flowmeter_sn_335", "flowmeter_sn_150",
              "haul_factor_10m2_335", "haul_factor_10m2_150",
              "haul_factor_100m3_335", "haul_factor_100m3_150",
              "primary_flag")

chr_cols <- c("size_fract_20", "secondary_flag") 

combined_dataframe <- combined_dataframe %>%
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.))),
         across(all_of(chr_cols), as.character))

## ------------------------------------------ ##
##    Manual fixes 
## ------------------------------------------ ##
unique(combined_dataframe$cruise)

# fix timestamps; depth_target

#	AE2426_L5_B5; datetime_UTC_end should be = 2024-11-07 13:52:00 (incorrectly entered 2024-11-07 23:52:00)
#	AE2426_L11_B9 = CTD was CAST 10 = sample name is B10

#	AR88_L6_B10; datetime_UTC_end should be = 2025-04-27 00:00:00 (incorrectly entered 2025-04-26 00:00:00)
#	AR88_L3_B18 wire rate in should be 18

# AR92_L1_B1 = latitude_start should be 41.193702
# AR92_L3_B22 = morph_ID_150 should be Y  & wire_rate_in should be 16

# AR99_L8_B20 = target depth == 133

combined_dataframe <- combined_dataframe %>%
  mutate(
    datetime_UTC_end = case_when(
      # AE2426 L5 B5 — end time entered 23:52, should be 13:52
      cruise == "AE2426" & station == "L5" & cast == "5" ~
        as.POSIXct("2024-11-07 13:52:00", tz = "UTC"),
      # AR88 L6 B10 — end date entered 2025-04-26, should be 2025-04-27
      cruise == "AR88" & station == "L6" & cast == "10" ~
        as.POSIXct("2025-04-27 00:00:00", tz = "UTC"),
      TRUE ~ datetime_UTC_end
    ),
    # fix depth target typo
    depth_target = case_when(
      cruise == "AR99" & station == "L8" & cast == "20" ~ 133,
      TRUE ~ depth_target
    ),
    # fix wire rate in entries
    wire_rate_in = case_when(
      cruise == "AR88" & station == "L3" & cast == "18" ~ 18, # AR88 L3 B18; 18
      cruise == "AR92" & station == "L3" & cast == "22" ~ 16, # AR92 L3 B22; 16
      TRUE ~ wire_rate_in
    ),
    # AR92 L1 B1 — latitude_start should be 41.193702
    latitude_start = case_when(
      cruise == "AR92" & station == "L1" & cast == "1" ~ 41.193702,
      TRUE ~ latitude_start
    ),
    # AR92 L3 B22 — morph_ID_150 should be Y
    morph_ID_150 = case_when(
      cruise == "AR92" & station == "L3" & cast == "22" ~ "Y",
      TRUE ~ morph_ID_150
    )
  )

# check tow duration 
combined_dataframe %>%
  filter(!is.na(datetime_UTC_start), !is.na(datetime_UTC_end)) %>%
  mutate(
    duration_min = as.numeric(difftime(datetime_UTC_end,
                                       datetime_UTC_start,
                                       units = "mins"))
  ) %>%
  select(cruise, station, cast, datetime_UTC_start, datetime_UTC_end,
         duration_min) %>%
  arrange(duration_min) %>%
  as.data.frame() %>%
  print()

# confirm types still match v2 right 
identical(sapply(combined_dataframe[names(tow_meta_v2)], \(x) class(x)[1]),
          sapply(tow_meta_v2, \(x) class(x)[1]))   # should be TRUE

## ------------------------------------------ ##
##  AR99+ separate ring-net tows (from elog) 
## ------------------------------------------ ##
# Starting AR99, the 20-um ring net deploys SEPARATELY from the bongo (its own
# tow, own timestamp)
# These are in the event log, not the bongo logsheets.
# position + time + (TDR depth) only;
# non-quantitative (no flowmeter), so volume/haul stay NA.

# from nes-lter-api-pulls.Rproj download 10-AUG-2026
event_log <- read_csv(here("data", "raw",
                           "elog_zoop_tows_thruHRS2601_2026-08-10.csv"))

ring_cruises <- c("AR99") #hrs2601

ring_tows <- event_log %>%
  filter(cruise %in% ring_cruises, grepl("^R", cast)) %>%
  # collapse deploy/recover into one row per tow
  group_by(cruise, station, cast) %>%
  summarise(
    datetime_UTC_start = min(datetime8601[action == "deploy"],  na.rm = TRUE),
    datetime_UTC_end   = max(datetime8601[action == "recover"], na.rm = TRUE),
    latitude_start     = first(latitude[action == "deploy"]),
    longitude_start    = first(longitude[action == "deploy"]),
    latitude_end       = first(latitude[action == "recover"]),
    longitude_end      = first(longitude[action == "recover"]),
    comments           = first(na.omit(comment)),
    .groups = "drop"
  ) %>%
  mutate(
    sample_name = paste0(cruise, "_", station, "_", cast),  # AR99_L2_R3
    comments    = "Standalone vertical ring-net tow separate from bongo."
  ) %>%
  mutate(cast = gsub("^R", "", cast))   # "R3" -> "3", matches bongo cast

# add the v2 columns this doesn't have, fill NA, match order
ring_missing <- setdiff(names(tow_meta_v2), names(ring_tows))
ring_tows[ring_missing] <- NA
ring_tows <- ring_tows %>% select(all_of(names(tow_meta_v2)))

# coerce types to match (same num_cols as the bongo rows)
ring_tows <- ring_tows %>%
  mutate(across(any_of(num_cols), ~ suppressWarnings(as.numeric(.))),
         across(any_of(chr_cols), as.character))

## ------------------------------------------ ##
##  AR99 ring-net TDR depth 
## ------------------------------------------ ##

## --- TDR depth from nes-lter-tdr-bongo.Rproj --- ##
tdr <- readRDS(here("data", "raw", "tdr_data_no_offset_2026-08-11.rds"))

# max TDR depth for the standalone ring tows (depth_TDR is NA for them from elog)
tdr_ring_max <- tdr %>%
  filter(cruise == "AR99", grepl("^R", cast)) %>%   # ring casts only
  filter(!is.na(depth_m)) %>%                        
  group_by(cruise, station, cast) %>%
  summarise(depth_TDR_ring = max(depth_m, na.rm = TRUE), .groups = "drop") %>%
  mutate(cast = gsub("^R", "", cast))                # match stripped cast key

ring_tows <- ring_tows %>%
  left_join(tdr_ring_max, by = c("cruise", "station", "cast")) %>%
  mutate(depth_TDR = coalesce(depth_TDR, depth_TDR_ring)) %>%
  select(-depth_TDR_ring)

## ------------------------------------------ ##
##  Combine with tow_meta_v2
## ------------------------------------------ ##

# confirm column names match before binding
identical(names(combined_dataframe), names(tow_meta_v2))

tow_meta_v3 <- bind_rows(tow_meta_v2, combined_dataframe, ring_tows) %>%
  arrange(datetime_UTC_start)

message(glue::glue("tow_meta_v2: {nrow(tow_meta_v2)} rows"))
message(glue::glue("new cruises: {nrow(combined_dataframe)} rows"))
message(glue::glue("tow_meta_v3: {nrow(tow_meta_v3)} rows"))

# check all cruises present
tow_meta_v3 %>%
  count(cruise) %>%
  arrange(cruise) %>%
  print(n = Inf)

# no duplicates across cruise × station × cast
tow_meta_v3 %>%
  count(cruise, station, cast) %>%
  filter(n > 1)
# expected for ar99 due to separate ring net tow

# date range makes sense
range(tow_meta_v3$datetime_UTC_start, na.rm = TRUE)

# depth target shouldnt be > 200m
tow_meta_v3 %>%
  summarise(
    min_depth  = min(depth_target, na.rm = TRUE),
    max_depth  = max(depth_target, na.rm = TRUE),
    n_na       = sum(is.na(depth_target)),
    n_over_200 = sum(depth_target > 200, na.rm = TRUE)
  )

## ------------------------------------------ ##
##  Add net_type column
## ------------------------------------------ ##
# bongo = bongo-frame deployment (335 + 150 nets; historically frame-mounted ring)
# ring  = standalone ring-net tow (old + AR99+ separate)
# distinguished by sample name: R = ring, B = bongo
tow_meta_v3 <- tow_meta_v3 %>%
  mutate(
    net_type = case_when(
      grepl("_R\\d+[A-Z]?$", sample_name) ~ "ring",
      grepl("_B\\d+[A-Z]?$", sample_name) ~ "bongo",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(net_type, .after = sample_name)

## --- remove NON-tow entries --- ##
# 2 rows from AE2426 couldnt sample too rough
tow_meta_v3 <- tow_meta_v3 %>% filter(!is.na(sample_name))

## ------------------------------------------ ##
##  Check coordinates: logsheet/v2 vs elog START coords (all cruises) 
## ------------------------------------------ ##
elog_start_coords <- event_log %>%
  filter(action == "deploy") %>%
  mutate(net_type = if_else(grepl("^R", cast), "ring", "bongo")) %>%  
  group_by(cruise, station, cast, net_type) %>%
  summarise(lat_start_elog = first(latitude),
            lon_start_elog = first(longitude), .groups = "drop") %>%
  mutate(cast = gsub("^[BR]", "", cast))  # strip B or R to match metadata

start_check <- tow_meta_v3 %>%
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
  summarise(n_compared = sum(!is.na(dist_m)),
            n_no_elog   = sum(is.na(dist_m)),
            max_dist_m  = max(dist_m, na.rm = TRUE),
            mean_dist_m = mean(dist_m, na.rm = TRUE),
            n_over_500m = sum(dist_m > 500, na.rm = TRUE))

start_check %>%
  filter(dist_m > 500) %>%
  arrange(desc(dist_m)) %>%
  select(cruise, station, cast, latitude_start, lat_start_elog,
         longitude_start, lon_start_elog, dist_m) %>%
  print(n = Inf)

## --- fix bad coordinates in logsheet --- ##
# AR38   L6 use elog lat and lon starts
# AE2426 L2 use elog latitude_start  
# AR92   L6 use elog latitude_start
# AR95   MVCO use elog longitude_start
# AR95   L6 use elog longitude_start
# all bongo

tow_meta_v3 <- tow_meta_v3 %>%
  left_join(elog_start_coords %>%
              select(cruise, station, cast, net_type, lat_start_elog, lon_start_elog),
            by = c("cruise", "station", "cast", "net_type")) %>%
  mutate(
    # AR38 L6: both coordinates
    latitude_start = if_else(cruise == "AR38" & station == "L6",
                             lat_start_elog, latitude_start),
    longitude_start = if_else(cruise == "AR38" & station == "L6",
                              lon_start_elog, longitude_start),
    # latitude-only fixes
    latitude_start = if_else(
      (cruise == "AE2426" & station == "L2") |
        (cruise == "AR92"   & station == "L6"),
      lat_start_elog, latitude_start),
    # longitude-only fixes
    longitude_start = if_else(
      (cruise == "AR95" & station == "MVCO") |
        (cruise == "AR95" & station == "L6"),
      lon_start_elog, longitude_start)
  ) %>%
  select(-lat_start_elog, -lon_start_elog)

## ------------------------------------------ ##
##  End coordinates for new-cruise bongo tows (from elog) 
## ------------------------------------------ ##
# Logsheets carry only START coordinates
# END coords (recover) are in the elog
# Pull recover lat/lon per bongo tow and fill latitude_end/longitude_end.
# v2 rows already have end coords - coalesce keeps them, fills only NAs

elog_end_coords <- event_log %>%
  filter(action == "recover") %>%
  mutate(net_type = if_else(grepl("^R", cast), "ring", "bongo")) %>%
  group_by(cruise, station, cast, net_type) %>%
  summarise(latitude_end_elog  = first(latitude),
            longitude_end_elog = first(longitude), .groups = "drop") %>%
  mutate(cast = gsub("^[BR]", "", cast))

tow_meta_v3 <- tow_meta_v3 %>%
  left_join(elog_end_coords, by = c("cruise", "station", "cast", "net_type"),
            suffix = c("", "_j")) %>%
  mutate(
    latitude_end  = coalesce(latitude_end,  latitude_end_elog),
    longitude_end = coalesce(longitude_end, longitude_end_elog)
  ) %>%
  select(-latitude_end_elog, -longitude_end_elog)

## ------------------------------------------ ##
##  Add depth_PX column 
## ------------------------------------------ ##
px <- readRDS(here("data", "raw", "px_data_bongo_2026-06-18.rds"))

glimpse(px)          # column names + types
head(px, 20)
#  # rows per cast
px %>% count(cruise, station, cast) %>% arrange(desc(n)) %>% head()

px_max <- px %>%
  filter(!is.na(depth_m)) %>%                          
  group_by(cruise, station, cast) %>%                
  summarise(depth_PX = max(depth_m, na.rm = TRUE),     
            .groups = "drop") %>%
  mutate(cast = gsub("^[BR]", "", as.character(cast)))

tow_meta_v3 <- tow_meta_v3 %>%
  left_join(px_max, by = c("cruise", "station", "cast")) %>%
  relocate(depth_PX, .after = depth_TDR)

tow_meta_v3 %>%
  filter(cruise %in% c("AE2426","EN727","AR88","AR92","AR95","AR99","HRS2601")) %>%
  group_by(cruise) %>%
  summarise(n = n(),
            n_px = sum(!is.na(depth_PX)),
            .groups = "drop")

tow_meta_v3 %>%
  filter(!is.na(depth_PX)) %>%
  select(cruise, station, cast, depth_PX) %>%
  arrange(cruise, station) %>%
  print(n = Inf)

## add missing depth_PX 
# EN727 L9 = 200
# AR99 L2 = 44 & L11 = 199
# AR95 L6 = 91
tow_meta_v3 <- tow_meta_v3 %>%
  mutate(
    depth_PX = case_when(
      cruise == "EN727" & station == "L9"  ~ 200,
      cruise == "AR99"  & station == "L2"  ~ 44,
      cruise == "AR99"  & station == "L11" ~ 199,
      cruise == "AR95"  & station == "L6"  ~ 91,
      TRUE ~ depth_PX
    )
  )

# AE2426 L2, L7, L8 TDR column were actually PX depths, need to fix to actual TDR
# TDR DEPTHS FROM TDR DATA:
# L2 19 = 33.5 
# L7 15 = 104
# L8 13 = 131  
tow_meta_v3 <- tow_meta_v3 %>%
  mutate(
    depth_TDR = case_when(
      cruise=="AE2426" & station=="L2" & cast=="19" ~ 33.5,
      cruise=="AE2426" & station=="L7" & cast=="15" ~ 104,
      cruise=="AE2426" & station=="L8" & cast=="13" ~ 131,
      # I found the TDR file for EN657 L1 (it was nested in another file cast)
      # in v2 Net max depth was calculated based on wire information (cosine law)
      cruise=="EN657"  & station=="L1" & cast=="1"  ~ 15.78,
      TRUE ~ depth_TDR
    )
  )

## ------------------------------------------ ##
#            Save -----
## ------------------------------------------ ##
last_cruise <- tow_meta_v3 %>%
  filter(!is.na(datetime_UTC_start)) %>%
  slice_max(datetime_UTC_start, n = 1) %>%
  pull(cruise)

stamp <- glue::glue("{last_cruise}-{format(Sys.Date(), '%Y%m%d')}")

write_csv(tow_meta_v3, here("data", "processed",
                            glue::glue("nes-lter-bongologs-{stamp}.csv")))
saveRDS(tow_meta_v3,   here("data", "processed",
                            glue::glue("nes-lter-bongologs-{stamp}.rds")))
