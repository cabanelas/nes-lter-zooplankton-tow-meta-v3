################################################################################
## Script:  02_merge_ship_speed.R
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
## Join key: cruise + datetime floored to the minute
##           (underway = 1/min; event log = 1/sec)
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

# maybe i dnt need this here?
# files <- list.files(here::here("data", "processed"),
#                     pattern = "^nes-lter-bongologs-.*\\.rds$", full.names = TRUE)
# dates <- as.Date(stringr::str_extract(basename(files), "\\d{8}"), "%Y%m%d")
# latest_bongolog <- files[which.max(dates)]
# 
# tow_meta_v3 <- readRDS(latest_bongolog)

## ------------------------------------------ ##
#      Build minute-resolution join key ----
## ------------------------------------------ ##
# underway = 1/min, event log = 1/sec -> floor elog to the minute to match
event_log <- event_log %>%
  mutate(dt_min = floor_date(datetime8601, "minute"))

# EN720/EN727 underway feed logs 2 rows per minute (~0.1-0.3 kt apart)
# collapse to one row per cruise-minute by averaging so the join stays
# many-to-one. Older cruises are already 1/min (unaffected).
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

# guard should now be empty
ship_speed_merge %>% count(cruise, dt_min) %>% filter(n > 1)

## ------------------------------------------ ##
#            Merge ----
## ------------------------------------------ ##
merged_data <- event_log %>%
  left_join(ship_speed_merge,
            by = c("cruise", "dt_min"),
            relationship = "many-to-one") %>%   # errors if key not unique
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

# per-cruise coverage: matched but speed still NA = genuine underway gap
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
  rename(message_id = message.ID)

# en720 
intersect(unique(ship_speedv2$cruise), new_cruises)

full_speed <- bind_rows(
  ship_speedv2 %>% filter(cruise != "EN720"), 
  merged_new)

setdiff(names(ship_speedv2), names(merged_new))  # v2-only: ...1, dateTime, datetime_decimaldoy?
setdiff(names(merged_new), names(ship_speedv2))  # v3-only: _source cols, dt-related?

full_speed %>% count(cruise) %>% print(n = Inf)   # EN720 should appear once, count 24
full_speed %>% filter(cruise == "EN720") %>% summarise(all_na = all(is.na(speedlog_waterspeedfwd)))  # FALSE now

## ------------------------------------------ ##
#            Write ----
## ------------------------------------------ ##
# write_csv(merged_data, here("output", "shipspeed_eventlog_v3.csv"))


## ------------------------------------------ ##
#            OLD STUFF TO CHECK -----
## ------------------------------------------ ##

class(event_log$datetime8601)
class(ship_speed$date)
# not sure if need to add dateTime col to ship_speed or have a col in common? maybe alreadt do?

# save original dateTime values before rounding
event_log$dateTime_original <- event_log$dateTime
# this is so that after merging event logs and ship speed i can ultimately
# use (un)rounded time values 

# round dateTime to nearest minute (i need to round to merge because 
# ship speed underway data every minute [not second])
event_log$dateTime <- as.POSIXct(format(event_log$dateTime, 
                                        "%Y-%m-%d %H:%M"), 
                                 tz="UTC")
ship_speed$dateTime <- as.POSIXct(format(ship_speed$dateTime, 
                                        "%Y-%m-%d %H:%M"),
                                             tz="UTC")

class(event_log$dateTime)
class(ship_speed$dateTime)

# apply function to fix data
event_log <- correct_dateTime(event_log)

## ----------------------------------------------- ##
#               Merge dataframe -----
## ----------------------------------------------- ##

# merge the two data frames, keeping rows from ship_speed that match
merged_data <- left_join(
  event_log,
  ship_speed,
  by = c("dateTime", "cruise")  # merge by dateTime entries
)

head(merged_data)

# clean up columns = remove some
merged_data_clean <- merged_data %>%
  select(-c(date.x, datetime_unix, spd, sog, X))

## ----------------------------------------------- ##
#               Check merge -----
## ----------------------------------------------- ##

#check if there are any missing data/cruises
missing_dateTimes <- setdiff(event_log$dateTime, merged_data_clean$dateTime)

# Filter the event_log rows with those missing dateTimes
missing_rows <- event_log %>%
  filter(dateTime %in% missing_dateTimes) %>%
  select(cruise, Station, Cast, Action)
missing_rows


## ----------------------------------------------- ##
#               Tidy data -----
## ----------------------------------------------- ##

# rename columns 
merged_data_clean <- merged_data_clean %>%
  rename(date = date.y, 
         year = Year,
         month = Month,
         day = Day,
         instrument = Instrument,
         action = Action,
         station = Station,
         cast = Cast,
         lat = Latitude,
         lon = Longitude,
         comment = Comment,
         message.ID = Message.ID)

colnames(merged_data_clean)

# remove "_trans" speed data
merged_data_clean <- merged_data_clean %>%
  select(-c(speedlog_waterspeedtrans, speedlog_groundspeedtrans))
colnames(merged_data_clean)


#add original elog times back (not rounded to nearest minute)
merged_data_clean$dateTime <- merged_data_clean$dateTime_original
merged_data_clean$dateTime_original <- NULL

#merged_data_clean$dateTime <- format(merged_data_clean$dateTime, 
#                                     "%Y-%m-%d %H:%M:%S %z")

#write.csv(merged_data_clean, "output/shipspeed.csv")
