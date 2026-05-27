################################################################################
#############          NES - LTER Cruise           #############################
#############             SEP-2024                 #############################
#############       UNDERWAY SHIP SPEED DATA       #############################
## by: Alexandra Cabanelas 
################################################################################
# download ship speed data from NES LTER REST API
# for NES LTER transect cruises
# its part of the underway data

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(dplyr)
library(listviewer)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

#############################################
#speed log in rest api underway data
#############################################
file_urls <- c(
  'https://nes-lter-data.whoi.edu/api/underway/en608.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en617.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en627.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en644.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en649.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en655.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en657.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en661.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en668.csv',
  'https://nes-lter-data.whoi.edu/api/underway/at46.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en687.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en695.csv',
  'https://nes-lter-data.whoi.edu/api/underway/hrs2303.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en706.csv',
  'https://nes-lter-data.whoi.edu/api/underway/en712.csv', 
  'https://nes-lter-data.whoi.edu/api/underway/en715.csv', 
  #'https://nes-lter-data.whoi.edu/api/underway/en720.csv',#not available as of 27-SEP
  'https://nes-lter-data.whoi.edu/api/underway/ar77.csv',
  'https://nes-lter-data.whoi.edu/api/underway/ar32.csv',
  'https://nes-lter-data.whoi.edu/api/underway/ar38.csv',
  'https://nes-lter-data.whoi.edu/api/underway/ar61b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/underway/ar66b.csv', #ring net only / elog not available through rest api but downloaded directly
  'https://nes-lter-data.whoi.edu/api/underway/ar28b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/underway/ar31a.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/underway/ar34b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/underway/ar39b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/underway/ar63.csv' 
)
# OOI? no bongo, no ring :
# AR34A, AR22, AR24a, AR24c, AR28A, AR31C, AR39A, AR44, AR48A, AR48B

all_columns <- c("date", "datetime_unix", "datetime_decimaldoy", "cruise", 
                 "speedlog_waterspeedfwd", "speedlog_groundspeedfwd", 
                 "speedlog_waterspeedtrans", "speedlog_groundspeedtrans",
                 "spd","sog")
#should i add coordinates?

# function to read each CSV file from rest API and keep only relevant columns
# i have to do this because the underway data from different cruises have different
#column names and column #s
read_and_add_cruise <- function(url) {
  cruise_name <- toupper(gsub(".csv", "", basename(url)))
  data <- read.csv(url, stringsAsFactors = FALSE)  
  
  # initialize a data frame with NA values 
  selected_data <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(all_columns)))
  colnames(selected_data) <- all_columns
  selected_data$cruise <- cruise_name  
  
  # map alternative column names to the final column names
  column_map <- list(
    date = "date",
    datetime_unix = "datetime_unix",
    datetime_decimaldoy = "datetime_decimaldoy",
    #for some cruises the speed is under speedlog_.. while in others its spd or sog
    speedlog_waterspeedfwd = c("speedlog_waterspeedfwd", "spd"),
    speedlog_groundspeedfwd = c("speedlog_groundspeedfwd", "sog", "sog_knots"),
    speedlog_waterspeedtrans = "speedlog_waterspeedtrans",
    speedlog_groundspeedtrans = "speedlog_groundspeedtrans"
  )
  
  # populate the selected_data data frame based on available columns
  for (col_name in names(column_map)) {
    alternatives <- column_map[[col_name]]
    
    # if the alternative is a vector, check which is present
    if (is.vector(alternatives)) {
      present_col <- alternatives[alternatives %in% names(data)]
      if (length(present_col) > 0) {
        selected_data[[col_name]] <- data[[present_col[1]]]
      }
    } else {
      # if it's a single name, check directly
      if (alternatives %in% names(data)) {
        selected_data[[col_name]] <- data[[alternatives]]
      }
    }
  }
  
  return(selected_data)
}

cruiseDat <- lapply(file_urls, read_and_add_cruise)

jsonedit(cruiseDat)

ship_speed <- do.call(rbind, cruiseDat)

head(ship_speed)

#write.csv(ship_speed, "output/raw_ship_speed_underwayrestapi_28SEP24.csv")
