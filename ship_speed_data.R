################################################################################
## Script:  01_ship_speed_data.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Get underway ship speed data from NES-LTER cruises via 
##           NES LTER REST API. Included in the underway data. 
##
##  This adds data starting AE2426 (fall 2024)
##        v2 had up to EN720
##
################################################################################
# OOI;no bongo, no ring: AR34A, AR22, AR24a, AR24c, AR28A, AR31C, AR39A, AR44, AR48A, AR48B
# NOTE: AE2426 and HRS2601 have no through-water speed in the underway
# only wind speed/GPS-derived SOG. speedlog_waterspeedfwd is NA for these 
# STW-based volume unavailable for tows on these two cruises

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(dplyr)
library(listviewer)
library(purrr)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

## --- Speed log in rest api underway data --- ##
url_prefix <- "https://nes-lter-api.whoi.edu"
file_urls <- paste0(url_prefix, "/api/underway/",
                    c("en720", #was not available as of 27-SEP; so doing now
                      "ae2426","en727","ar88","ar92","ar95","ar99",
                      "hrs2601"), #hrs2609; ar105
                    ".csv")

## --- to see which cols are available --- ##
# read just the header row (fast) for each cruise
headers <- map(file_urls, ~ names(read.csv(.x, nrows = 1, check.names = FALSE)))
names(headers) <- toupper(gsub(".csv$", "", basename(file_urls)))
# union of every column that appears anywhere
sort(unique(unlist(headers)))

# speedlog_waterspeedfwd = fore-aft water speed; STW
# spd     = STW?
# sog_kts = SOG in knots
# sog     = SOG, units not stated in name
# speedlog_groundspeedfwd = ground speed from the speed log
# speed_kt = GPS-derived speed in knots
# gps_furuno_smg / gps_furuno170_smg / gps_garmin741_smg = speed made good from GPS = SOG = last-resort fallback

# select columns
all_columns <- c("date", "datetime_unix", "datetime_decimaldoy", "cruise",
                 "latitude", "longitude",
                 "speedlog_waterspeedfwd",  "speedlog_groundspeedfwd",
                 "speedlog_waterspeedtrans", "speedlog_groundspeedtrans") #athwartships component sideway drift
# "spd","sog", "sog_kts"; #coordinates?

## --- Read each CSV file --- ##
# function to read each CSV file from rest API and keep only relevant columns
# underway data from different cruises have different column names and column #s
read_and_add_cruise <- function(url) {
  cruise_name <- toupper(gsub("\\.csv$", "", basename(url)))
  data <- read.csv(url, stringsAsFactors = FALSE, check.names = FALSE)
  
  selected_data <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(all_columns)))
  colnames(selected_data) <- all_columns
  selected_data$cruise <- cruise_name
  
  column_map <- list(
    date                = "date",
    datetime_unix       = "datetime_unix",
    datetime_decimaldoy = "datetime_decimaldoy",
    latitude            = c("latitude", "dec_lat", "gps_furuno_latitude"),
    longitude           = c("longitude", "dec_lon", "gps_furuno_longitude"),
    speedlog_waterspeedfwd    = c("speedlog_waterspeedfwd", "spd"),
    speedlog_groundspeedfwd   = c("sog_kts", "speedlog_groundspeedfwd", "sog", 
                                  "sog_knots", "speed_kt"),
    speedlog_waterspeedtrans  = "speedlog_waterspeedtrans",
    speedlog_groundspeedtrans = "speedlog_groundspeedtrans"
  )
  
  for (col_name in names(column_map)) {
    present_col <- column_map[[col_name]][column_map[[col_name]] %in% names(data)]
    if (length(present_col) > 0) {
      selected_data[[col_name]] <- data[[present_col[1]]]
      selected_data[[paste0(col_name, "_source")]] <- present_col[1]
    }
  }
  
  # --- coerce numeric fields so every cruise returns consistent types ---
  num_fields <- c("latitude", "longitude",
                  "speedlog_waterspeedfwd",  "speedlog_groundspeedfwd",
                  "speedlog_waterspeedtrans", "speedlog_groundspeedtrans")
  for (f in num_fields) {
    selected_data[[f]] <- suppressWarnings(as.numeric(selected_data[[f]]))
  }
  
  selected_data
}

cruiseDat <- lapply(file_urls, read_and_add_cruise)
ship_speed <- bind_rows(cruiseDat)
#jsonedit(cruiseDat)

head(ship_speed)

ship_speed %>%
  distinct(cruise,
           speedlog_waterspeedfwd_source,
           speedlog_groundspeedfwd_source,
           latitude_source, longitude_source)

# NO ship speeds available for HRS2601

## --- Save csv --- ##
write.csv(ship_speed,
          here::here("data", "output", 
                     paste0("raw_ship_speed_underwayrestapi_", 
                            Sys.Date(), ".csv")),
          row.names = FALSE)