################################################################################
#############          NES - LTER Cruise           #############################
#############             SEP-2024                 #############################
#############     UNDERWAY SHIP SPEED DATA       #############################
## by: Alexandra Cabanelas 
################################################################################
# event log + ship speed merge
# adding ship speed data to event log data for NES LTER transect cruises

# fixing issues/errors with data event log entries 

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

# SHIP SPEED DATA 
ship_speed <- read.csv(file.path("output",
                                 "raw_ship_speed_underwayrestapi_28SEP24.csv")) %>%
  select(-X)
#created in ship_speed_data.R (ship speeds downloaded from rest api)

# EVENT LOG DATA 
event_log <- read.csv(file.path("raw",
                                "all_eventLogs_26SEP2024.csv")) 
#created in RESTAPICRUISE > eventLOGs.R (event logs downloaded from rest api)


## ------------------------------------------ ##
#            Handle time entries -----
## ------------------------------------------ ##
class(event_log$dateTime)
class(ship_speed$date)

# Create "dateTime" column 

# convert dateTime8601 to date format (posixct)
event_log$dateTime <- as.POSIXct(event_log$dateTime8601, 
                              format="%Y-%m-%d %H:%M:%S", 
                              tz="UTC")

ship_speed$dateTime <- as.POSIXct(ship_speed$date, 
                                             format="%Y-%m-%d %H:%M:%S", 
                                             tz="UTC")

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


## ----------------------------------------------- ##
#      Manual fix times (dateTime) on event log entries -----
## ----------------------------------------------- ##

#some odd values, need to fix

# EN695 L6B2 recover logged as 04:35 however speed too high and time not recorded
# on bongo log sheet so possibly wrong recovery time? should probably be 04:22

# AR31A L6R1 recover time is wrong on event log, should be 08:56
# there are two readings for the deploy on L6R1 but exactly the same data (a few secs off)

# AR28B L1R1 has wrong time (20:59)
# should be 20:52

# AR38 L6 date should be 2019-09-21 (not 2019-09-20)

# EN627 L11 NA is a calibration; 19:33 was marked as the start of cal1 but it 
# probably is 19:46 - 20:14

# EN655 
#   L5 deploy time should be 19:39
#   L6 B16 recover and deploy have same times on elog should be 02:28-02:38

# HRS2303 - recover time should be
#   L4    19:23
#   L5    04:58
#   L9    14:21 (not 14:36)
#   L10   19:35-19:55
#   L11   00:41

# EN617 L10 deploy 22:38

## ---

# AR38 entry with no station, no cast at 2019-09-20 18:46:00 == no sample
# logsheet says too much fishing gear

# AR61B L10R7 times seem off ? at least based on ship speed data; but everything else
#has that time on it so idk 

# AT46 L1 B1 says 19:30; this was action == Other == microscope - not a sample

# i checked AT46 L6 B6 but it seems okay


# Function to correct dateTime values 
#(update both dateTime and dateTime_original at once)

correct_dateTime <- function(event_log) {
  event_log <- event_log %>%
    dplyr::mutate(
      dateTime = case_when(
        # EN695 L6 B2 recover
        cruise == "EN695" & 
          Station == "L6" & 
          Cast == "B2" & 
          Action == "recover" ~ as.POSIXct("2023-01-12 04:22:00", tz="UTC"),
        # AR31A L6 R1 recover
        cruise == "AR31A" & 
          Station == "L6" & 
          Cast == "R1" & 
          Action == "recover" ~ as.POSIXct("2018-10-21 08:56:00", tz="UTC"),
        # AR28B L1 R1 
        cruise == "AR28B" & 
          Station == "L1" & 
          Cast == "R1" ~ as.POSIXct("2018-04-03 20:52:00", tz="UTC"),
        # AR38 L6 date should be 2019-09-21 (not 2019-09-20)
        cruise == "AR38"  & 
          Station == "L6" & 
          Cast == "B8" & 
          Action == "deploy" ~ as.POSIXct("2019-09-21 05:06:00", tz="UTC"),
        # EN627 L11 - Calibration 1 start time 
        cruise == "EN627" & 
          Station == "L11" & 
          Comment == "Flowmeter Calibration 1 Start" ~ as.POSIXct("2019-02-04 19:46:00", tz="UTC"),
        # EN655 
        #   L5 deploy time should be 19:39
        #   L6 B16 recover and deploy have same times on elog should be 02:28-02:38
        cruise == "EN655" & 
          Station == "L5" & 
          Action == "deploy" ~ as.POSIXct("2020-07-26 19:39:00", tz="UTC"),
        cruise == "EN655" & 
          Station == "L6" & 
          Cast == "B16" & 
          Action == "deploy" ~ as.POSIXct("2020-07-28 02:28:00", tz="UTC"),
        cruise == "EN655" & 
          Station == "L6" & 
          Cast == "B16" & 
          Action == "recover" ~ as.POSIXct("2020-07-28 02:38:00", tz="UTC"),
        # HRS2303 - recover time should be
        #   L4    19:23
        #   L5    04:58
        #   L9    14:21 (not 14:36)
        #   L10   19:35-19:55
        #   L11   00:41
        cruise == "HRS2303" & 
          Station == "L4" & 
          Action == "recover" ~ as.POSIXct("2023-05-04 19:23:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L5" & 
          Action == "recover" ~ as.POSIXct("2023-05-03 04:58:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L9" & 
          Cast == "B6" & 
          Action == "recover" ~ as.POSIXct("2023-05-03 14:21:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L10" & 
          Action == "deploy" ~ as.POSIXct("2023-05-03 19:35:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L10" & 
          Action == "recover" ~ as.POSIXct("2023-05-03 19:55:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L11" & 
          Action == "recover" ~ as.POSIXct("2023-05-04 00:41:00", tz="UTC"),
        # EN617 L10 deploy 22:38
        cruise == "EN617" & 
          Station == "L10" & 
          Action == "deploy" ~ as.POSIXct("2018-07-22 22:38:00", tz="UTC"),
        TRUE ~ dateTime
      ),
      # update dateTime_original with the corrected dateTime values
      dateTime_original = case_when(
        # EN695 L6 B2 recover
        cruise == "EN695" & 
          Station == "L6" & 
          Cast == "B2" & 
          Action == "recover" ~ as.POSIXct("2023-01-12 04:22:00", tz="UTC"),
        # AR31A L6 R1 recover
        cruise == "AR31A" & 
          Station == "L6" & 
          Cast == "R1" & 
          Action == "recover" ~ as.POSIXct("2018-10-21 08:56:00", tz="UTC"),
        # AR28B L1 R1 
        cruise == "AR28B" & 
          Station == "L1" & 
          Cast == "R1" ~ as.POSIXct("2018-04-03 20:52:00", tz="UTC"),
        # AR38 L6 date should be 2019-09-21 (not 2019-09-20)
        cruise == "AR38"  & 
          Station == "L6" & 
          Cast == "B8" & 
          Action == "deploy" ~ as.POSIXct("2019-09-21 05:06:00", tz="UTC"),
        # EN627 L11 - Calibration 1 start time 
        cruise == "EN627" & 
          Station == "L11" & 
          Comment == "Flowmeter Calibration 1 Start" ~ as.POSIXct("2019-02-04 19:46:00", tz="UTC"),
        # EN655 
        #   L5 deploy time should be 19:39
        #   L6 B16 recover and deploy have same times on elog should be 02:28-02:38
        cruise == "EN655" & 
          Station == "L5" & 
          Action == "deploy" ~ as.POSIXct("2020-07-26 19:39:00", tz="UTC"),
        cruise == "EN655" & 
          Station == "L6" & 
          Cast == "B16" & 
          Action == "deploy" ~ as.POSIXct("2020-07-28 02:28:00", tz="UTC"),
        cruise == "EN655" & 
          Station == "L6" & 
          Cast == "B16" & 
          Action == "recover" ~ as.POSIXct("2020-07-28 02:38:00", tz="UTC"),
        # HRS2303 - recover time should be
        #   L4    19:23
        #   L5    04:58
        #   L9    14:21 (not 14:36)
        #   L10   19:35-19:55
        #   L11   00:41
        cruise == "HRS2303" & 
          Station == "L4" & 
          Action == "recover" ~ as.POSIXct("2023-05-04 19:23:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L5" & 
          Action == "recover" ~ as.POSIXct("2023-05-03 04:58:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L9" & 
          Cast == "B6" & 
          Action == "recover" ~ as.POSIXct("2023-05-03 14:21:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L10" & 
          Action == "deploy" ~ as.POSIXct("2023-05-03 19:35:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L10" & 
          Action == "recover" ~ as.POSIXct("2023-05-03 19:55:00", tz="UTC"),
        cruise == "HRS2303" & 
          Station == "L11" & 
          Action == "recover" ~ as.POSIXct("2023-05-04 00:41:00", tz="UTC"),
        # EN617 L10 deploy 22:38
        cruise == "EN617" & 
          Station == "L10" & 
          Action == "deploy" ~ as.POSIXct("2018-07-22 22:38:00", tz="UTC"),
        TRUE ~ dateTime_original
      )  
    )
  
  return(event_log)
}

# apply function to fix data
event_log <- correct_dateTime(event_log)


## ----------------------------------------------- ##
#            Manual fix "Action" on event log entries -----
## ----------------------------------------------- ##

# need to fix action in some event log entries

## ---

# AR34B L11 R10 (both entries were marked as "deploy" originally)
#     dateTime == 2019-04-18 06:01:00 == action = deploy
#     dateTime == 2019-04-18 06:14:00 == action = recover


# AR34B L11 R10
event_log <- event_log %>%
  mutate(Action = case_when(
    dateTime == as.POSIXct("2019-04-18 06:01:00", tz = "UTC") ~ "deploy",
    dateTime == as.POSIXct("2019-04-18 06:14:00", tz = "UTC") ~ "recover",
    TRUE ~ Action
  ))


## ----------------------------------------------- ##
#    Manual fix "Cast" on event log entries -----
## ----------------------------------------------- ##

# EN617 L11
#     dateTime == 2018-07-24 08:19:00 == cast == B25A == action == deploy
#     dateTime == 2018-07-24 08:48:00 == cast == B25A == action == recover
#     dateTime == 2018-07-24 09:24:00 == cast == B25B == action == deploy
#     dateTime == 2018-07-24 09:53:00 == cast == B25B == action == recover

# EN617 L11
event_log <- event_log %>%
  mutate(Cast = case_when(
    dateTime == as.POSIXct("2018-07-24 08:19:00", tz = "UTC") ~ "B25A",
    dateTime == as.POSIXct("2018-07-24 08:48:00", tz = "UTC") ~ "B25A",
    dateTime == as.POSIXct("2018-07-24 09:24:00", tz = "UTC") ~ "B25B",
    dateTime == as.POSIXct("2018-07-24 09:53:00", tz = "UTC") ~ "B25B",
    TRUE ~ Cast  # keep the original cast for other rows
  ))

## ----------------------------------------------- ##
#    Delete no sample entries (hit bottom; other probs..) -----
## ----------------------------------------------- ##

# AR31A L6 R1 duplicated = keep 1 
# Keep only one entry for AR31A L6 R1 (remove duplicates)
event_log <- event_log %>%
  filter(dateTime != as.POSIXct("2018-10-21 08:35:00", tz = "UTC"))

# AR77 L2 B2 = aborted tow = redeployed later
#     dateTime == 2023-10-12 02:38:28 == delete
#     dateTime == 2023-10-12 02:50:00 == delete
#bongo was deployed twice due to issues with hydraulics
#delete first tow == no sample
event_log <- event_log %>%
  dplyr::filter(!(dateTime %in% as.POSIXct(c("2023-10-12 02:38:00", 
                                             "2023-10-12 02:50:00"), 
                                           tz = "UTC")))

# EN617 MVCO B35 (time is a bit off on elog)
#     dateTime == 2018-07-25 02:10:00 == action = calibration == delete
event_log <- event_log %>%
  filter(dateTime != as.POSIXct("2018-07-25 02:10:00", tz = "UTC"))

# delete EN627 L1 B3 hit bottom; L1 not sampled
event_log <- event_log %>%
  filter(!(cruise == "EN627" & 
             Station == "L1" & 
             Cast == "B3"))


# delete EN644 L9 B12 hit bottom those 2 times and had sample at another cast (18)
event_log <- event_log %>%
  filter(!(cruise == "EN644" & 
           Station == "L9" & 
           Cast == "B12"))

# AR38 L6 B8 - deploy entry has wrong date (fixed above), but now here updating
#the Day column 
event_log <- event_log %>%
  mutate(Day = 
           case_when(
      cruise == "AR38" & 
      Station == "L6" & 
      Cast == "B8" & 
      Action == "deploy" ~ 21,
    TRUE ~ Day  # keep all other Day values unchanged
  ))

head(event_log)


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
