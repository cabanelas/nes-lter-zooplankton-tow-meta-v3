################################################################################
#############           NES LTER Cruises           #############################
#############     NES BONGO EVENT DATASHEETS       #############################
################################################################################
# by: Alexandra Cabanelas 
# created JUN-2024
# updated NOV-2025:: additions for inventory data package v3
################################################################################
# merging all available bongo event log datasheets 
# exporting for zooplankton inventory data package update v3
# 2018-2025 cruises
# ring net only: AR31A, AR39B, AR34B, AR28B, AR66B, AR61B
# downloaded v2 package on 07-NOV-2025
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.24.2

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)
#library(readr)
library(lubridate)

library(ggplot2)
library(maps)
library(sf)
library(plotly)
#library(tidyr)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
bongo_logs <- read.csv(file.path("output","all_zp_net_bongo_logs_01OCT2024.csv"),
                       header = T) %>%
  select(-X)
# created in all_zp_net_bongo_logs.R script

## ------------------------------------------ ##
#           Add winch data -----
## ------------------------------------------ ##
winch_data <- read.csv(file.path("raw","winch","cruise_winch_data.csv"),
                       header = T)

bongo_winch <- left_join(bongo_logs, 
                         winch_data,
                         by = c("cruise", "station", "cast"))

## ------------------------------------------ ##
#      Add event long and ship speed data -----
## ------------------------------------------ ##
ship_speed <- read.csv(file.path("output","shipspeed.csv"),
                       header = T) %>%
  select(dateTime, cruise, station, cast, instrument, action, lat, lon,
         speedlog_waterspeedfwd, speedlog_groundspeedfwd
         ) 
# cruise info in this data comes from event logs and underway data
# created in shipspeed_eventlog_merge.R script 

## ------------------------------------------ ##
#       Tidy -----
## ------------------------------------------ ##

# remove the 'B' or 'R' of cast
ship_speed <- ship_speed %>%
  mutate(cast = gsub("^[BR]", "", cast))

## ------------------------------------------ ##
#       Pivot from long to wide -----
## ------------------------------------------ ##

#check for duplicates before transforming
ship_speed |>
  dplyr::summarise(n = dplyr::n(), 
                   .by = c("cruise", "instrument", "station", "cast", "action")) |>
  dplyr::filter(n > 1L)

ship_speed <- ship_speed %>%
  filter(action %in% c("deploy", "recover")) %>%
  # Pivot lat, lon, datetime, and speed columns to create deploy and recover columns
  pivot_wider(
    names_from = action,
    values_from = c(lat, 
                    lon, 
                    dateTime, 
                    speedlog_waterspeedfwd, 
                    speedlog_groundspeedfwd),
    names_glue = "{.value}_{action}",
    values_fill = NA
  )

## ------------------------------------------ ##
#       Merge -----
## ------------------------------------------ ##

bongo_shipspeed <- bongo_winch %>%
  left_join(ship_speed, by = c("cruise", "station", "cast"))

bongo_shipspeed <- bongo_shipspeed %>%
  rename(STW_start = speedlog_waterspeedfwd_deploy,
         STW_end = speedlog_waterspeedfwd_recover,
         SOG_start = speedlog_groundspeedfwd_deploy,
         SOG_end = speedlog_groundspeedfwd_recover)

# rename 
bongo_shipspeed <- bongo_shipspeed %>%
  rename(dateTime_start_UTC = dateTime_deploy,
         dateTime_end_UTC = dateTime_recover)

## ------------------------------------------ ##
#           Add corrected TDR depth -----
## ------------------------------------------ ##
tdr <- read.csv(file.path("raw","max_depth_tdr_corrected.csv"),
                       header = T)

tdr <- tdr %>%
  mutate(cast = gsub("B", "", cast))

bongo_tdr <- bongo_shipspeed %>%
  left_join(tdr %>% select(cruise, station, cast, corrected_max_depth), 
            by = c("cruise", "station", "cast"))

## ------------------------------------------ ##
#           Calculate net depth (when TDR missing) -----
## ------------------------------------------ ##
# Columns:
# max_depth = max depth recorded by TDR or CTD mounted on bongo wire
# corrected_max_depth = corrected max depth values (after TDR offset)

## Straight Cosine Law -- 
# Z = L cos_a
#Z = calculated tow depth (in meters)
#L = maximum wire out (in meters)
#Cos_a = cosine of the wire angle at maximum wire out.  Wire angle is measured between towing wire and the vertical.

######## DID NOT USE THE FOLLOWING (FROM ECOMON MANUAL):
######## I FOUND THAT IT GAVE NEGATIVE VALUES AND DIDNT WORK WELL
## Regression Equation -- appropriate for tow depths >50 m 
# Z = (-3.7 + 0.756) / (L)
#Z =  calculated tow depth (in meters)
#L = maximum wire out (in meters)

## These equations aren't great w strong currents

bongo_netdepth <- bongo_tdr %>%
  mutate(
    avg_angle_radians = avg_angle * (pi / 180),  # convert avg_angle to radians
    tow_depth_calc = case_when(
      # vertical tow for Ring Net
      instrument == "Ring Net" & !is.na(max_wire_out_m) ~ max_wire_out_m,  # use max_wire_out_m if not NA
      instrument == "Ring Net" & is.na(max_wire_out_m) & !is.na(depth_target) ~ depth_target,  # use depth_target if max_wire_out_m is NA
      TRUE ~ max_wire_out_m * cos(avg_angle_radians) # Cosine Law
      #depth_target <= 50 ~ max_wire_out_m * cos(avg_angle_radians), # Cosine Law for depths <= 50m
      #depth_target > 50 ~ (-3.7 + 0.756) / max_wire_out_m # Regression Equation for depths > 50m
    )
)

# now assign one of these depths as the final net depth 
bongo_netdepth2 <- bongo_netdepth %>%
  mutate(
    net_max_depth_m = case_when(
      !is.na(depth_TDR) & is.na(corrected_max_depth) ~ depth_TDR, # use depth_TDR if it's not NA & corrected_max_depth is NA
      !is.na(corrected_max_depth) ~ corrected_max_depth, # use corrected_max_depth if it's not NA
      is.na(depth_TDR) & is.na(corrected_max_depth) & !is.na(tow_depth_calc) ~ tow_depth_calc,  # use tow_depth_calc if both depth_TDR and corrected_max_depth are NA
      is.na(depth_TDR) & is.na(corrected_max_depth) & is.na(tow_depth_calc) & !is.na(depth_target) ~ depth_target,  # use depth_target only if all others are NA
      TRUE ~ NA_real_ # if all are NA, set net_max_depth_m to NA
    )
  )

# Check if net_max_depth_m is ever greater than depth_bottom
bongo_netdepth2 %>%
  filter(net_max_depth_m > depth_bottom) %>%
  mutate(difference = net_max_depth_m - depth_bottom) %>%
  select(cruise, station, cast, depth_bottom, depth_TDR, corrected_max_depth, 
         tow_depth_calc, net_max_depth_m, difference)
# its okay for a few to be a bit deeper due to depth changes while towing 
# only one that looks sus is EN695 L8 , but leaving as is

#####    add data flag
#secondary_flag
# actual measurement of depth recorder (TDR) not available. 
# Net max. depth was calculated based on wire information and bottom max depth

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

bongo_netdepth3 <- bongo_netdepth2 %>%
  mutate(
    # create initial secondary_flag based on conditions
    secondary_flag = case_when(
      grepl("hit bottom", comments, ignore.case = TRUE) ~ "hit bottom.", 
      is.na(depth_TDR) & !is.na(tow_depth_calc) ~ "Depth recorder (TDR) data not available. Net max depth was calculated based on wire information (cosine law).",
      is.na(depth_TDR) & is.na(tow_depth_calc) & !is.na(depth_target) ~ "Target depth used for net max depth due to unavailable TDR data and wire information.",
      instrument == "Ring Net" & is.na(max_wire_out_m) & !is.na(depth_target) ~ "Target depth used for net max depth due to unavailable wire data for Ring Net.",
      TRUE ~ NA_character_  # default case if none of the conditions are met
    )
  )

bongo_netdepth3 <- bongo_netdepth3 %>%
  mutate(
    # append additional comments to secondary_flag 
    secondary_flag = str_trim(
      str_c(
        coalesce(secondary_flag, ""),  # replace NA with an empty string
        ifelse(grepl("tons of small salps", comments, ignore.case = TRUE), 
               "many salps in this cruise.", ""),
        ifelse(grepl("cod end broke.", comments, ignore.case = TRUE), 
               "cod end broke.", ""),
        ifelse(grepl("cod end was tangled", comments, ignore.case = TRUE), 
               "tangled cod end.", ""),
        #ifelse(grepl("non quantitative", comments, ignore.case = TRUE), 
        #       "non quantitative.", ""),
        ifelse(grepl("Non quantitative 150 micron sample", comments, ignore.case = TRUE), 
               "non quantitative 150 micron sample.", ""), 
        ifelse(grepl("150um sample probably non quantitative", comments, ignore.case = TRUE), 
               "non quantitative 150 micron sample.", ""),
        ifelse(grepl("Non quantitative 335", comments, ignore.case = TRUE), 
               "non quantitative 335 micron sample.", ""), 
        ifelse(grepl("Deployed and recovered without sample", comments, ignore.case = TRUE), 
               "no sample.", ""), 
        ifelse(grepl("forgot to get flow start", comments, ignore.case = TRUE), 
               "flowmeter issue.", ""),
        ifelse(grepl("Flowmeter calibration", comments, ignore.case = TRUE), 
               "no sample.", ""),
        ifelse(grepl("no 20um ring net sample", comments, ignore.case = TRUE), 
               "no 20um ring net sample.", ""),
        ifelse(grepl("flowmeter reading is off", comments, ignore.case = TRUE), 
               "flowmeter issue.", ""),
        ifelse(grepl("missing flowmeter numbers for 335um net", comments, ignore.case = TRUE), 
               "flowmeter issue.", ""),
        sep = " "
      ) %>%
        str_replace_all("\\bNA\\b", "") %>%  # remove standalone NAs
        str_squish()  # remove any leading or trailing whitespace
    )
  )

bongo_netdepth3 <- bongo_netdepth3 %>%
  mutate(
    primary_flag = ifelse(!is.na(secondary_flag) & secondary_flag != "", 3, 1),
    secondary_flag = ifelse(secondary_flag == "", NA, secondary_flag)
  )

## ------------------------------------------ ##
#           Calculate vol filtered -----
## ------------------------------------------ ##
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

bongo_vol <- bongo_netdepth3 %>%
  mutate(vol_filtered_m3_335 = ((tot_flow_counts_335/10) * 0.26873 * 0.2922),
         vol_filtered_m3_150 = ((tot_flow_counts_150/10) * 0.26873 * 0.2922))
         #haul_factor_10m2_335 = (net_max_depth_m*10)/vol_filtered_m3_335,
         #haul_factor_10m2_150 = (net_max_depth_m*10)/vol_filtered_m3_150,
         #haul_factor_100m3_335 = 100 / vol_filtered_m3_335,
         #haul_factor_100m3_150 = 100 / vol_filtered_m3_150)
         #VOL_FILT_335_c = ((pi * (0.305)^2) * ((tot_flow_counts_335 * 26873)/999999)), #gives the same
         #VOL_FILT_150_c = ((pi * (0.305)^2) * ((tot_flow_counts_150 * 26873)/999999)))

## ------------------------------------------ ##
#            time -----
## ------------------------------------------ ##
## only keep elog time entries 
# first fix missing elog time entries

# Convert dateUTCyymmdd into proper date format 
bongo_vol$dateUTCyymmdd <- ymd(bongo_vol$dateUTCyymmdd)

# Combine dateUTCyymmdd with time_start_UTC and time_end_UTC to create full POSIXct datetime values
bongo_vol$date_time_start_UTC <- as.POSIXct(paste(bongo_vol$dateUTCyymmdd, 
                                                  bongo_vol$time_start_UTC), 
                                        format = "%Y-%m-%d %H:%M:%S", 
                                        tz = "UTC")

bongo_vol$date_time_end_UTC <- as.POSIXct(paste(bongo_vol$dateUTCyymmdd, 
                                                bongo_vol$time_end_UTC), 
                                      format = "%Y-%m-%d %H:%M:%S", 
                                      tz = "UTC")

#Replace missing dateTime_start_UTC with date_time_start_UTC
bongo_vol$dateTime_start_UTC <- ifelse(is.na(bongo_vol$dateTime_start_UTC), 
                                       as.character(bongo_vol$date_time_start_UTC), 
                                             bongo_vol$dateTime_start_UTC)

# Replace missing dateTime_end_UTC with date_time_end_UTC
bongo_vol$dateTime_end_UTC <- ifelse(is.na(bongo_vol$dateTime_end_UTC), 
                                             as.character(bongo_vol$date_time_end_UTC), 
                                             bongo_vol$dateTime_end_UTC)

bongo_vol <- bongo_vol %>%
  rename(lat_start = lat_deploy,
         lat_end = lat_recover,
         lon_start = lon_deploy,
         lon_end = lon_recover)

## ------------------------------------------ ##
#            ship speed -----
## ------------------------------------------ ##
# make sure ship speed is positive 
# sometimes negative values for ring net samples when ship speed == 0
#since those were vertical tows
bongo_vol <- bongo_vol %>%
  mutate(
    STW_start = abs(STW_start),
    SOG_start = abs(SOG_start),
    STW_end = abs(STW_end),
    SOG_end = abs(SOG_end)
  )

#en720 ship speed not available yet (too new)
# will make ship speed 2knots since that's what we had for that cruise
#so, estimated ship speed for now
bongo_vol <- bongo_vol %>%
  mutate(
    SOG_start = if_else(cruise == "EN720", 2, SOG_start),
    SOG_end = if_else(cruise == "EN720", 2, SOG_end)
  )

## ------------------------------------------ ##
#            select columns -----
## ------------------------------------------ ##

nes_tow_metadata <- bongo_vol %>%
  dplyr::select(
    cruise, station, cast, sample_name, instrument, 
    #dateUTCyymmdd, 
    #time_start_UTC, time_end_UTC, 
    dateTime_start_UTC, 
    dateTime_end_UTC, 
    #lat_start_decdeg, lon_start_decdeg, 
    lat_start, lon_start, 
    lat_end, lon_end, 
    depth_bottom, depth_target, depth_TDR, net_max_depth_m, avg_angle, 
    max_wire_out_m, wire_rate_out_m_min, wire_rate_in_m_min,
    STW_start, SOG_start, 
    STW_end, SOG_end, 
    flowmeter_sn_335, flow_start_335, flow_end_335, tot_flow_counts_335, 
    vol_filtered_m3_335,
    NOAA_335, DNA_335, 
    flowmeter_sn_150, flow_start_150, flow_end_150, tot_flow_counts_150, 
    vol_filtered_m3_150,
    morph_ID_150, DNA_150, size_fract_150, taxa_pick_150, size_fract_20,
    primary_flag, secondary_flag, comments, 
  ) %>%
  rename(date_time_start_UTC = dateTime_start_UTC, 
         date_time_end_UTC = dateTime_end_UTC)

## ------------------------------------------ ##
#            QA/QC -----
## ------------------------------------------ ##

colnames(nes_tow_metadata)
str(nes_tow_metadata)

unique(nes_tow_metadata$cruise)
unique(nes_tow_metadata$station)
unique(nes_tow_metadata$cast)
unique(nes_tow_metadata$sample_name)
unique(nes_tow_metadata$instrument)
unique(nes_tow_metadata$flowmeter_sn_335)
unique(nes_tow_metadata$morph_ID_150)
unique(nes_tow_metadata$DNA_150)
unique(nes_tow_metadata$size_fract_150)
unique(nes_tow_metadata$taxa_pick_150)
unique(nes_tow_metadata$size_fract_20)

nes_tow_metadata %>%
  summarise(
    min_lat = min(lat_start, na.rm = TRUE),
    max_lat = max(lat_start, na.rm = TRUE),
    min_lon = min(lon_start, na.rm = TRUE),
    max_lon = max(lon_start, na.rm = TRUE),
    min_depth_bottom = min(depth_bottom, na.rm = TRUE),
    max_depth_bottom = max(depth_bottom, na.rm = TRUE),
    min_depth_target = min(depth_target, na.rm = TRUE),
    max_depth_target = max(depth_target, na.rm = TRUE),
    min_avg_angle = min(avg_angle, na.rm = TRUE),
    max_avg_angle = max(avg_angle, na.rm = TRUE),
    min_depth_TDR = min(depth_TDR, na.rm = TRUE),
    max_depth_TDR = max(depth_TDR, na.rm = TRUE),
    min_net_max_depth_m = min(net_max_depth_m, na.rm = TRUE),
    max_net_max_depth_m = max(net_max_depth_m, na.rm = TRUE),
    min_avg_angle = min(avg_angle, na.rm = TRUE),
    max_avg_angle = max(avg_angle, na.rm = TRUE),
    min_wire_out_m = min(max_wire_out_m, na.rm = TRUE),
    max_wire_out_m = max(max_wire_out_m, na.rm = TRUE),
    min_wire_rate_out_m_min = min(wire_rate_out_m_min, na.rm = TRUE),
    max_wire_rate_out_m_min = max(wire_rate_out_m_min, na.rm = TRUE),
    min_flow_start_335 = min(flow_start_335, na.rm = TRUE),
    max_flow_start_335 = max(flow_start_335, na.rm = TRUE),
    min_flow_end_335 = min(flow_end_335, na.rm = TRUE),
    max_flow_end_335 = max(flow_end_335, na.rm = TRUE),
    min_tot_flow_counts_335 = min(tot_flow_counts_335, na.rm = TRUE),
    max_tot_flow_counts_335 = max(tot_flow_counts_335, na.rm = TRUE),
    min_vol_filtered_m3_335 = min(vol_filtered_m3_335, na.rm = TRUE),
    max_vol_filtered_m3_335 = max(vol_filtered_m3_335, na.rm = TRUE),
    min_flow_start_150 = min(flow_start_150, na.rm = TRUE),
    max_flow_start_150 = max(flow_start_150, na.rm = TRUE),
    min_flow_end_150 = min(flow_end_150, na.rm = TRUE),
    max_flow_end_150 = max(flow_end_150, na.rm = TRUE),
    min_tot_flow_counts_150 = min(tot_flow_counts_150, na.rm = TRUE),
    max_tot_flow_counts_150 = max(tot_flow_counts_150, na.rm = TRUE),
    min_vol_filtered_m3_150 = min(vol_filtered_m3_150, na.rm = TRUE),
    max_vol_filtered_m3_150 = max(vol_filtered_m3_150, na.rm = TRUE),
  ) %>%
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>%
  print(n = Inf)

## ------------------------------------------ ##
#            QA/QC coordinates -----
## ------------------------------------------ ##
nes_tow_metadata1 <- nes_tow_metadata %>%
  filter(!is.na(lon_start) & !is.na(lat_start)) %>%
  rename(lon = lon_start,
         lat = lat_start)

# Convert your data frame to an sf object (Spatial DataFrame)
sf_data <- st_as_sf(nes_tow_metadata1, 
                    coords = c("lon", "lat"), 
                    crs = 4326, 
                    agr = "constant")

# Map
ggplot() +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_sf(data = sf_data, color = "blue", size = 1) +    # Add your points
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +     # Set limits for the map
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude")

ggplot() +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_sf(data = sf_data, color = "blue", size = 1) +    # Add your points
  coord_sf(xlim = c(-74, -68), ylim = c(38, 44)) +     # Set limits for the map
  theme_minimal() +
  labs(x = "lon",
       y = "lat")


ggplot() +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_sf(data = sf_data, color = "blue", size = 1) +    # Add your points
  coord_sf(xlim = c(-72, -69), ylim = c(41, 43)) +     # Set limits for the map
  theme_minimal() +
  labs(x = "lon",
       y = "lat")



# Interactive maps

sf_data2 <- cbind(sf_data, st_coordinates(sf_data))

p <- ggplot(sf_data2, aes(x = X, y = Y, text = paste("Sample:", sample_name))) +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_point(color = "blue", size = 3) +                 # Add your points
  coord_sf(xlim = c(-72, -69), ylim = c(41, 43)) +       # Set limits for the map
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude")

#interactive
ggplotly(p, tooltip = "text")

p1 <- ggplot(sf_data2, aes(x = X, y = Y, text = paste("Sample:", sample_name))) +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_point(color = "blue", size = 3) +                 # Add your points
  coord_sf(xlim = c(-74, -68), ylim = c(38, 44)) +       # Set limits for the map
  theme_minimal() +
  labs(x = "lon",
       y = "lat")

#interactive
ggplotly(p1, tooltip = "text")


p2 <- ggplot(sf_data2, aes(x = X, y = Y, color = station, text = paste("Sample:", sample_name))) +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_point(size = 3) +                                 # Add your points
  coord_sf(xlim = c(-74, -68), ylim = c(39, 43)) +       # Set limits for the map
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude")

#interactive
ggplotly(p2, tooltip = "text")

# EN655 L6, AR34B L10, EN649 L6, EN644 L11 seem off but double checked on elog

#write.csv(nes_tow_metadata, "output/nes-lter-zooplankton-event-log-v2.csv")