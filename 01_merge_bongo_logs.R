################################################################################
## Script:  01_merge_bongo_logs.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Merge all bongo net event log datasheets from new cruises
##          (2024-2026) with published v2 inventory metadata, producing
##          a combined dataset for v3 EDI package (2018-2026 cruises).
##
##  This is NOT the final v3 metadata; still needs:
##                - TDR max depths
##                - Haul factors
##                - Primary/secondary flags
##                - AR92 and AR95 raw log sheets
##
## Inputs (data/raw/):
##   - bongo_logs/*.csv           (new cruise event log datasheets)
##   - nes-lter-zooplankton-tow-metadata-v2.csv  (EDI: knb-lter-nes.24.2)
##
## Outputs (data/processed/):
##   - all-nes-lter-bongologs-YYYYMMDD.csv / .rds
##
## Ring net only (no bongo): AR31A, AR39B, AR34B, AR28B, AR66B, AR61B
## New columns added starting EN720: ship_speed_kts, EtOHchanged
##
## v2 package downloaded: 07-NOV-2025
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2
## created JUN-2024 | updated MAY-2026
################################################################################

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(readr) #for read_csv (faster than read.csv)
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

## --- 4) add columns present in tow_meta_v2 but missing here — fill with NA ---
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

## ------------------------------------------ ##
##  check data
## ------------------------------------------ ##
## --- cruise and station counts — expected ~12 per cruise ---
combined_dataframe %>%
  count(cruise, sort = FALSE) 

## --- which stations per cruise ---
combined_dataframe %>%
  group_by(cruise) %>%
  summarise(stations = paste(sort(station), collapse = ", "),
            n = n(), .groups = "drop") 

## --- tow duration — flag anything < 3 min or > 60 min ---
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

## ------------------------------------------ ##
##    Manually fix bad timestamps
## ------------------------------------------ ##
unique(combined_dataframe$cruise)

#	AE2426_L5_B5; datetime_UTC_end should be = 2024-11-07 13:52:00 (incorrectly entered 2024-11-07 23:52:00)
#	AE2426_L11_B9 = CTD was CAST 10

#	AR88_L6_B10; datetime_UTC_end should be = 2025-04-27 00:00:00 (incorrectly entered 2025-04-26 00:00:00)

combined_dataframe <- combined_dataframe %>%
  mutate(
    datetime_UTC_end = case_when(
      # AE2426 L5 B5 — end time was 23:52, should be 13:52
      cruise == "AE2426" & station == "L5" & cast == "5" ~
        as.POSIXct("2024-11-07 13:52:00", tz = "UTC"),
      # AR88 L6 B10 — end date was 2025-04-26, should be 2025-04-27
      cruise == "AR88" & station == "L6" & cast == "10" ~
        as.POSIXct("2025-04-27 00:00:00", tz = "UTC"),
      TRUE ~ datetime_UTC_end
    ),
    # fix depth target typo
    depth_target = case_when(
      cruise == "AR99" & station == "L8" & cast == "20" ~ "133",
      TRUE ~ depth_target
    )
  )

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

combined_dataframe <- combined_dataframe %>%
  mutate(across(c(latitude_start, longitude_start, latitude_end, longitude_end,
                  depth_bottom, depth_target, depth_TDR, net_max_depth,
                  avg_angle, max_wire_out, wire_rate_out, wire_rate_in,
                  STW_start, SOG_start, STW_end, SOG_end,
                  flowmeter_sn_335, flowmeter_sn_150,
                  haul_factor_10m2_335, haul_factor_10m2_150,
                  haul_factor_100m3_335, haul_factor_100m3_150),
                ~ suppressWarnings(as.numeric(.))))

## ------------------------------------------ ##
##  Combine with tow_meta_v2
## ------------------------------------------ ##

# confirm column names match before binding
identical(names(combined_dataframe), names(tow_meta_v2))

tow_meta_v3 <- bind_rows(tow_meta_v2, combined_dataframe) %>%
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
#            Save -----
## ------------------------------------------ ##
today <- format(Sys.Date(), "%Y%m%d")

write_csv(tow_meta_v3, here("data", "output",
                            glue::glue("all-nes-lter-bongologs-{today}.csv")))
saveRDS(tow_meta_v3,   here("data", "output",
                            glue::glue("all-nes-lter-bongologs-{today}.rds")))
