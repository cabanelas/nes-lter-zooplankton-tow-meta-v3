################################################################################
## Script:  02_ship_speed_eventlog_merge.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Merge underway ship speed (STW / SOG) onto the bongo/ring-net
##          event log for the v3 EDI package (2018-2026 cruises, thru HRS2601)
##
## Inputs (data/):
##   - processed/raw_ship_speed_underwayrestapi_20260810.csv  (01_ship_speed_data.R)
##   - raw/elog_zoop_tows_thruHRS2601_2026-08-10.csv          (nes-lter-api-pulls.Rproj)
## Outputs (output/):
##   - shipspeed_eventlog_v3.csv
##
## Speeds kept: fwd only (STW/SOG); trans dropped
################################################################################

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)
library(lubridate)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

## --- SHIP SPEED DATA --- ##
# ship_speed_old <- read.csv(file.path("data/processed",
#                                  "raw_ship_speed_underwayrestapi_28SEP24.csv")) %>%
#   select(-X)
#created in ship_speed_data.R (ship speeds downloaded from rest api)
ship_speed <- read_csv(here("data", "processed",
                            "raw_ship_speed_underwayrestapi_20260810.csv"))

## --- EVENT LOG DATA --- ##
# event_log_old <- read.csv(file.path("data/raw",
#                                 "all_eventLogs_26SEP2024.csv"))
# from nes-lter-api-pulls.Rproj download 10-AUG-2026
event_log <- read_csv(here("data", "raw", 
                           "elog_zoop_tows_thruHRS2601_2026-08-10.csv"))

## ------------------------------------------ ##
#      Build minute-resolution join key ----
## ------------------------------------------ ##
# underway = 1/min, event log = 1/sec -> floor elog to the minute to match
event_log <- event_log %>%
  mutate(dt_min = floor_date(datetime8601, "minute"))

# EN720/EN727 underway feed logs 2 rows per minute (~0.1-0.3 kt apart)
# collapse to one row per cruise-minute by averaging 
# Older cruises are already 1/min (unaffected)
# keep fwd speeds (STW/SOG) + provenance; drop trans, unix, doy, gps lat/lon
ship_speed_merge <- ship_speed %>%
  mutate(dt_min = floor_date(date, "minute")) %>%
  group_by(cruise, dt_min) %>%
  summarize(
    speedlog_waterspeedfwd  = mean(speedlog_waterspeedfwd,  na.rm = TRUE),
    speedlog_groundspeedfwd = mean(speedlog_groundspeedfwd, na.rm = TRUE),
    speedlog_waterspeedfwd_source  = first(speedlog_waterspeedfwd_source),
    speedlog_groundspeedfwd_source = first(speedlog_groundspeedfwd_source),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# should be empty
ship_speed_merge %>% count(cruise, dt_min) %>% filter(n > 1)

## ------------------------------------------ ##
#            Merge ----
## ------------------------------------------ ##
merged_data <- event_log %>%
  left_join(ship_speed_merge,
            by = c("cruise", "dt_min"),
            relationship = "many-to-one") %>%   
  select(-dt_min)

## ------------------------------------------ ##
#            Check merge ----
## ------------------------------------------ ##
# event log rows with NO underway record at that cruise-minute
event_log %>%
  anti_join(ship_speed_merge, by = c("cruise", "dt_min")) %>%
  select(cruise, station, cast, action, datetime8601)
event_log %>%
  anti_join(ship_speed_merge, by = c("cruise", "dt_min")) %>%
  select(cruise) %>%
  distinct() %>% print(n=40)

new_cruises <- c("AE2426", "EN720", "EN727", "AR88", "AR92", "AR95", "AR99", "HRS2601")

# per-cruise coverage: matched but speed still NA = no data in underway
merged_data %>%
  filter(cruise %in% new_cruises) %>%
  group_by(cruise) %>%
  summarise(n = n(),
            stw_na = sum(is.na(speedlog_waterspeedfwd)),
            sog_na = sum(is.na(speedlog_groundspeedfwd)),
            .groups = "drop") %>%
  print(n = Inf)

merged_new <- merged_data %>% 
  filter(cruise %in% new_cruises) %>%
  rename(lat = latitude, lon = longitude)

## ------------------------------------------ ##
#            Combine ----
## ------------------------------------------ ##

# from shipspeed_eventlog_merge.R v2
ship_speedv2 <- read_csv(here("data", "raw",
                              "shipspeed_eventlog_v2.csv")) %>%
  rename(message_id   = message.ID,
         datetime8601 = dateTime8601) %>%
  select(-"...1")

# en720 
intersect(unique(ship_speedv2$cruise), new_cruises)

full_speed <- bind_rows(
  ship_speedv2 %>% filter(cruise != "EN720"), 
  merged_new)

setdiff(names(ship_speedv2), names(merged_new))  
setdiff(names(merged_new), names(ship_speedv2))  

full_speed %>% count(cruise) %>% print(n = Inf)   # EN720 should appear once, count 24
full_speed %>% filter(cruise == "EN720") %>% summarise(all_na = all(is.na(speedlog_waterspeedfwd)))  # FALSE now

## ------------------------------------------ ##
#     Endeavor STW -> NA (SOG mislabeled) ----
## ------------------------------------------ ##
# Endeavor speedlog reports GPS-SOG in BOTH channels; no true water speed.
# Confirmed EN608-EN727: stw == sog & tracks gps_smg at mean~0. NA the STW,
# keep SOG. Revises v2 published STW for these cruises - note in changelog.
endeavor <- full_speed %>% filter(str_starts(cruise, "EN")) %>% distinct(cruise) %>% pull()

full_speed <- full_speed %>%
  mutate(
    speedlog_waterspeedfwd_source = if_else(
      cruise %in% endeavor & !is.na(speedlog_waterspeedfwd),
      "NA_endeavor_sog_only", speedlog_waterspeedfwd_source),
    speedlog_waterspeedfwd = if_else(cruise %in% endeavor, NA_real_, speedlog_waterspeedfwd)
  )
# AR + AT46 = real STW (channels diverge) -> untouched. HRS/AE = no STW channel.

## --- add source col to v2 speeds ---
# it didnt have it
full_speed <- full_speed %>%
  mutate(
    speedlog_groundspeedfwd_source = if_else(
      is.na(speedlog_groundspeedfwd_source) & !is.na(speedlog_groundspeedfwd),
      "v2_published", speedlog_groundspeedfwd_source),
    speedlog_waterspeedfwd_source = if_else(
      is.na(speedlog_waterspeedfwd_source) & !is.na(speedlog_waterspeedfwd),
      "v2_published", speedlog_waterspeedfwd_source)
  )

## ------------------------------------------ ##
#     QA/QC ----
## ------------------------------------------ ##

# Endeavor STW actually NA'd everywhere = should be 0 non-NA
full_speed %>% filter(str_starts(cruise, "EN")) %>%
  summarise(stw_remaining = sum(!is.na(speedlog_waterspeedfwd)))

# high speeds?
full_speed %>% filter(speedlog_waterspeedfwd > 4 | speedlog_groundspeedfwd > 4) %>%
  select(cruise, station, cast, action, speedlog_waterspeedfwd, speedlog_groundspeedfwd)

# Speed ranges per cruise sane (STW/SOG roughly 0-3 kt, small negatives ok)
full_speed %>% group_by(cruise) %>%
  summarize(stw_min = min(speedlog_waterspeedfwd, na.rm=TRUE),
            stw_max = max(speedlog_waterspeedfwd, na.rm=TRUE),
            sog_max = max(speedlog_groundspeedfwd, na.rm=TRUE), .groups="drop") %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>% print(n=Inf)

# Every row has a datetime + cruise
full_speed %>% summarise(no_dt = sum(is.na(datetime8601)),
                         no_cruise = sum(is.na(cruise)))

# Source columns populated where speed exists (provenance not lost)
full_speed %>%
  summarise(stw_val_no_src = sum(!is.na(speedlog_waterspeedfwd) &
                                   is.na(speedlog_waterspeedfwd_source)),
            sog_val_no_src = sum(!is.na(speedlog_groundspeedfwd) &
                                   is.na(speedlog_groundspeedfwd_source)))

# Row count sanity: one deploy + one recover per tow mostly (spot odd cruises)
full_speed %>% count(cruise, station, cast) %>% filter(n != 2) %>% print(n=Inf)

# Expected cruise set present, no dupes/surprises
full_speed %>% distinct(cruise) %>% arrange(cruise) %>% pull()

## ------------------------------------------ ##
#            Write ----
## ------------------------------------------ ##
write_csv(full_speed, here("data", "processed", "shipspeed_eventlog_v3.csv"))
