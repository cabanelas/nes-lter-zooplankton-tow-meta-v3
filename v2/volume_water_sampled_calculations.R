################################################################################
#############          NES - LTER Cruise           #############################
#############             SEP-2024                 #############################
#############     NES BONGO EVENT DATASHEETS       #############################
## by: Alexandra Cabanelas 
################################################################################
# script to check and evaluate volume water sampled values 
# determining whether to use flowmeter vs calculated volume sampled 
#(via ship speed and wire angle)

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)
library(tidyr)

#for plotting
library(RColorBrewer)
library(ggthemes)
library(ggpubr)
library(cowplot)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
#has time start and time end 
towData <- read.csv(file.path("output",
                               "nes-lter-zooplankton-event-log-v2.csv"),
                     header = T)
#created in bongoEventLogMerge.R

## ------------------------------------------ ##
#            colors -----
## ------------------------------------------ ##
mycolors = c(brewer.pal(name="Dark2", n = 8), 
             brewer.pal(name="Paired", n = 6))
mycol <- c("#691CEB", "#EB1CE0", "#AD1CEB", "#261CEB", "#EB89AB",
           "#C46AEB", "#E9D960", "#D61F7F", "#9F3461", "#5A2558",
           "#AEA508", "#B716EA", "#4381BA", "#F06B57", "#2F790D",
           "#6CBC83", "#2AA57E", "#A5E09B", "#E39CE2", "#000000")

## ------------------------------------------ ##
#    calculate volume of water filtered -----
## ------------------------------------------ ##

## ------------------------------------------ ##
# Need to convert time to seconds 

# convert TIME columns to POSIXct format (use any date since only time is relevant)
towData$date_time_start_UTC <- as.POSIXct(towData$date_time_start_UTC, 
                                           format="%Y-%m-%d %H:%M:%S", 
                                           tz="UTC")
towData$date_time_end_UTC <- as.POSIXct(towData$date_time_end_UTC, 
                                         format="%Y-%m-%d %H:%M:%S", 
                                         tz="UTC")

# calculate elapsed time in seconds per tow
towData$seconds_elapsed <- ifelse(
  towData$date_time_end_UTC < towData$date_time_start_UTC, 
  as.numeric(difftime(towData$date_time_end_UTC + 24 * 60 * 60, 
                      towData$date_time_start_UTC, 
                      units = "secs")),
  as.numeric(difftime(towData$date_time_end_UTC, 
                      towData$date_time_start_UTC, 
                      units = "secs"))
)

## ------------------------------------------ ##
#    Plot seconds elapsed -----
## ------------------------------------------ ##
# to make sure things look alright...

# define the order of stations
towData$station <- factor(towData$station, 
                           levels = c("MVCO", "L1", "L2", "L3", "L4", "L5", 
                                      "L6", "L7", "L8", "L9", "L10", "L11", "u11c"))

# create plots for each cruise
unique_cruises <- unique(towData$cruise)

for (cruise in unique_cruises) {
  cruise_plot <- ggplot(towData %>% filter(cruise == !!cruise), 
                        aes(x = station, y = seconds_elapsed)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    labs(title = paste("Cruise:", cruise), x = "Station", 
         y = "Elapsed Time (seconds)", fill = "Elapsed Time Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
  print(cruise_plot)
}

ggplot(subset(towData, station != "u11c"), 
       aes(x = station, y = seconds_elapsed, color = cruise)) +
  geom_point(size = 2.3) +
  theme_minimal() +
  labs(title = "Tow total time", y = "Elapsed Time (seconds)") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        plot.title = element_text(size = 18, face = "bold", color = "black")) 

ggplot(subset(towData, station != "u11c"), 
       aes(x = station, y = seconds_elapsed)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Tow total time", y = "Elapsed Time (seconds)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        plot.title = element_text(size = 18, face = "bold", color = "black")) 

ggplot(towData %>% 
         filter(!is.na(seconds_elapsed), station != "u11c"), 
       aes(x = station, 
           y = seconds_elapsed)) +
  geom_bar(stat = "identity", 
           position = position_dodge()) +
  theme_minimal() +
  labs(y = "Elapsed Time (seconds)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  facet_wrap(~ cruise, scales = "free_y", ncol = 5) 

## ------------------------------------------ ##
#    Calculate Volume of water sampled -----
## ------------------------------------------ ##

# Constants
diameter_m <- 0.61  # diameter in meters
radius_m <- diameter_m / 2  # radius in meters
A <- pi * radius_m^2  # area of the net mouth in square meters


# Calculate volume V in cubic meters
# Using ship speed & sampling time
# V <- A * T * S

#where:
# V = calculated volume of water filtered (in meters cubed)
# A = area of net mouth (in meters squared) = 0.2922
# T = duration of tow (in seconds)
# S = ship speed during tow (in meters per second)

towTimes <- towData %>%
  filter(instrument == "Bongo Net") %>%  #ring nets arent quantitative 
  mutate(
    # Use STW_end if STW_start is NA, otherwise use STW_start
    # Calculate GEAR_VOLUME_CALCULATED using the m/sec speed
    GEAR_VOLUME_CALCULATED = A * seconds_elapsed * (ifelse(is.na(STW_start), 
                                                          ifelse(is.na(STW_end), 1.5, abs(STW_end)), 
                                                          abs(STW_start)) * 0.514444)
  )

## ------------------------------------------ ##
#    Plot volume -----
## ------------------------------------------ ##

# volume filtered (based on flowmeter) for 335 micron net 
ggplot(towTimes %>% filter(!is.na(vol_filtered_m3_335) & 
                             vol_filtered_m3_335 > 0 & 
                             station != "u11c"), 
       aes(x = station, y = vol_filtered_m3_335)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cruise, scales = "free") +
  theme_minimal() +
  labs(y = "Volume Filtered 335 µm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggplot(towTimes %>% filter(!is.na(vol_filtered_m3_335) & 
                             vol_filtered_m3_335 > 0 & 
                             station != "u11c"), 
       aes(x = station, y = vol_filtered_m3_335)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cruise) +
  theme_minimal() +
  labs(y = "Volume Filtered 335 µm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

# volume filtered (based on flowmeter) for 150 micron net 
ggplot(towTimes %>% filter(!is.na(vol_filtered_m3_150) & 
                             vol_filtered_m3_150 > 0 & 
                             station != "u11c"), 
       aes(x = station, y = vol_filtered_m3_150)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cruise, scales = "free") +
  theme_minimal() +
  labs(y = "Volume Filtered 150 µm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggplot(towTimes %>% filter(!is.na(vol_filtered_m3_150) & 
                             vol_filtered_m3_150 > 0 & 
                             station != "u11c"), 
       aes(x = station, y = vol_filtered_m3_150)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cruise) +
  theme_minimal() +
  labs(y = "Volume Filtered 150 µm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

## ------------------------------------------ ##
#    Comparing volumes -----
## ------------------------------------------ ##

# comparing volume sampled of both flowmeters (should be relatively similar)

# reshape from wide to long format for plotting 
towTimes_long <- towTimes %>%
  pivot_longer(cols = c(vol_filtered_m3_335, vol_filtered_m3_150), 
               names_to = "volume_type", 
               values_to = "volume")

# compare flowmeter volume on 150micron vs 335 micron nets 
ggplot(towTimes_long, aes(x = station, y = volume, fill = volume_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +  
  facet_wrap(~ cruise, scales = "free") +
  theme_minimal() +
  labs(x = "Station", y = "Total Flow (m³)", fill = "Volume Type") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# same as above but one panel/plot per cruise
#comparing flowmeter volume on 150 and 335 nets
unique_cruises <- unique(towTimes_long$cruise)

for (cruise in unique_cruises) {
  cruise_plot <- ggplot(towTimes_long %>% filter(cruise == !!cruise), 
                        aes(x = station, y = volume, fill = volume_type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    labs(title = paste("Cruise:", cruise), x = "Station", 
         y = "Total Flow (m³)", fill = "Volume Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  #ggsave(filename = paste0("cruise_plot_", gsub(" ", "_", cruise), ".png"), 
  #       plot = cruise_plot, 
  #       width = 10, height = 6, dpi = 300, bg = "white")

  print(cruise_plot)
}

## ------------------------------------------ ##
#    Comparing volumes -----
## ------------------------------------------ ##

## comparing volume of flowmeter with geometrically calculated volume
towTimes_long_melted <- towTimes %>%
  pivot_longer(cols = c(vol_filtered_m3_335, 
                        vol_filtered_m3_150, 
                        GEAR_VOLUME_CALCULATED), 
               names_to = "volume_type", 
               values_to = "value") %>%
  mutate(volume_type = recode(volume_type, 
                              vol_filtered_m3_150 = "150 µm", 
                              vol_filtered_m3_335 = "335 µm"))

unique_cruises <- unique(towTimes_long_melted$cruise)

# bar plots
for (cruise in unique_cruises) {
  cruise_plot <- ggplot(towTimes_long_melted %>% 
                          filter(cruise == !!cruise), 
                        aes(x = station, y = value, fill = volume_type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    theme_minimal() +
    labs(title = paste("Cruise:", cruise), 
         x = "Station", 
         y = "Total Volume (m³)", 
         fill = "Volume Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  #ggsave(filename = paste0("cruise_plot2_", gsub(" ", "_", cruise), ".png"), 
  #       plot = cruise_plot, 
  #       width = 10, height = 6, dpi = 300, bg = "white")
  
  print(cruise_plot)
}

## ------------------------------------------ ##
#    Determining which volumes look off -----
## ------------------------------------------ ##
##### NICE PLOTS
# side by side scatter and bar plots of volumes

# scatter
towTimes_long2 <- towTimes %>%
  pivot_longer(cols = c(vol_filtered_m3_150, vol_filtered_m3_335), 
               names_to = "volume_type", 
               values_to = "vol_filtered_m3") %>%
  mutate(volume_type = recode(volume_type, 
                              vol_filtered_m3_150 = "150 µm", 
                              vol_filtered_m3_335 = "335 µm"))


unique_cruises1 <- unique(towTimes_long2$cruise)

for (cruise in unique_cruises) {
  
  # scatter plot - left panel
  scatter_plot <- ggplot(towTimes_long2 %>% filter(cruise == !!cruise & 
                                                     !is.na(vol_filtered_m3)), 
                         aes(x = GEAR_VOLUME_CALCULATED, 
                             y = vol_filtered_m3, 
                             color = volume_type, 
                             shape = volume_type)) +
    geom_abline(slope = 1, intercept = 0, 
                linetype = "dashed", color = "darkgrey", size = 1.2) +
    geom_point(size = 3.2) +
    scale_y_continuous(limits = c(0, 600)) +  
    scale_x_continuous(limits = c(0, 600)) + 
    scale_color_manual(values = c("#17356E", "#00BFB2")) +#"#755675", "#6ECC6C", "#78DF77", "#2F452E",
    labs(title = paste("Cruise:", cruise), 
         x = "Gear Volume Calculated (m³)",
         y = "Volume Filtered (m³)") +
    theme_few() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 15, color = "black"),
          axis.title = element_text(size = 15, color = "black"), 
          axis.text = element_text(size = 14, color = "black"),
          legend.position = "bottom")
  
  # bar plot - right panel
  bar_plot <- ggplot(towTimes_long_melted %>% filter(cruise == !!cruise), 
                     aes(x = station, y = value, fill = volume_type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = c("#17356E", "#00BFB2", "#CD4942")) + 
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = paste("Cruise:", cruise), 
         y = "Volume Filtered (m³)") +
    theme_few() +
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(),
          axis.text = element_text(size = 14, color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = 15, color = "black"),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
          )
  
  # combine plots
  combined_plot <- plot_grid(scatter_plot, bar_plot, ncol = 2)

  #ggsave(filename = paste0("cruise_plot2_", gsub(" ", "_", cruise), ".png"), 
  #       plot = cruise_plot, 
  #       width = 10, height = 6, dpi = 300, bg = "white")
  print(combined_plot) 
  
}

## ------------------------------------------ ##
#    Looking at spread and max/min values  -----
## ------------------------------------------ ##

ggplot(subset(towTimes_long_melted, station != "u11c"), 
       aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        plot.title = element_text(size = 18, face = "bold", color = "black")) 


ggplot(subset(towTimes_long_melted, station != "u11c"), 
       aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  geom_text(data = subset(towTimes_long_melted, station != "u11c" & value >= 850), 
            aes(label = cruise), 
            vjust = -1,  # adjust vertical position of labels
            size = 4,    # font size for labels
            color = "black") +  # text color 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        plot.title = element_text(size = 18, face = "bold", color = "black"))


ggplot(subset(towTimes_long_melted, station != "u11c" & value < 850), 
       aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        plot.title = element_text(size = 18, face = "bold", color = "black"))

filtered_data <- subset(towTimes_long_melted, 
                        station != "u11c" & 
                        volume_type != "GEAR_VOLUME_CALCULATED" &
                        cruise!= "EN627" & 
                        value < 850)

# identify the largest and smallest points for each station
labels_data <- filtered_data %>%
  group_by(station) %>%
  summarize(
    max_value = max(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    cruise_max = cruise[which.max(value)],
    cruise_min = cruise[which.min(value)],
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c("max_value", "min_value"), 
               names_to = "value_type", 
               values_to = "value") %>%
  mutate(cruise = ifelse(value_type == "max_value", cruise_max, cruise_min))

# scatter plot
ggplot(filtered_data, aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  # add labels for the largest and smallest points
  geom_text(data = labels_data, 
            aes(label = cruise), 
            vjust = -1,  # adjust vertical position of labels
            size = 4,    # font size for labels
            color = "black") +  # text color 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        plot.title = element_text(size = 18, face = "bold", color = "black"))



filtered_data1 <- subset(towTimes_long_melted, 
                         station != "u11c" & value < 850 & cruise != "EN627")

labels_data <- filtered_data1 %>%
  group_by(station) %>%
  summarize(
    largest_value = max(value),  # max value for labeling
    smallest_value = min(value),  # min value for labeling
    cruise_largest = cruise[which.max(value)],  # cruise for largest point
    cruise_smallest = cruise[which.min(value)],  # cruise for smallest point
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = starts_with("cruise"), names_to = "label_type", values_to = "cruise") %>%
  left_join(filtered_data1, by = c("station", "cruise" = "cruise")) %>%
  filter(value == largest_value | value == smallest_value) %>%
  select(station, value, cruise)  

# scatter plot
ggplot(filtered_data1, aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  # add labels for the largest and smallest points
  geom_text(data = labels_data, 
            aes(label = cruise), 
            vjust = -1,  # adjust vertical position of labels
            size = 4,    # font size for labels
            color = "black") +  # text color 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.x = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        plot.title = element_text(size = 18, face = "bold", color = "black"))




## ------------------------------------------ ##
#    Choose final volume values -----
## ------------------------------------------ ##

# AR38 L3 - for 335 um volume use calculated (non flowmeter value)
# EN617 L11 - use use calculated (non flowmeter value) for both nets
# HRS2303 L9 - use use calculated (non flowmeter value) for both nets 
# EN720 L11 - use calculated (non flowmeter value) for both nets
# EN715 L6 - use calculated (non flowmeter value) for both nets
# EN712 L1, L7, L9, L11 - for 150 um volume use calculated (non flowmeter value)
# EN712 L8 - use calculated (non flowmeter value) for both nets
# EN706 L5 - for 150 um volume use calculated (non flowmeter value)
# EN687 L8 - for 150 um volume use calculated (non flowmeter value)
# EN668 L2, L3, L4 - use calculated (non flowmeter value) for both nets
# EN657 MVCO - for 150 um volume use calculated (non flowmeter value)
# EN627 - use calculated (non flowmeter value) for both nets for all stations

# if flowmeters failed (not flow available) use calculated

###########
# need to calculate again because I had removed the RingNet data which
# is needed for EDI package

# Calculate volume V in cubic meters
# Using ship speed & sampling time
# V <- A * T * S

#where:
# V = calculated volume of water filtered (in meters cubed)
# A = area of net mouth (in meters squared) = 0.2922
# T = duration of tow (in seconds)
# S = ship speed during tow (in meters per second)

towData_edit <- towData %>%
  #filter(instrument == "Bongo Net") %>%  #ring nets arent quantitative 
  mutate(
    # Use STW_end if STW_start is NA, otherwise use STW_start
    # Calculate GEAR_VOLUME_CALCULATED using the m/sec speed
    GEAR_VOLUME_CALCULATED = A * seconds_elapsed * (ifelse(is.na(STW_start), 
                                                           ifelse(is.na(STW_end), 1.5, abs(STW_end)), 
                                                           abs(STW_start)) * 0.514444)
  )

towData_edit %>%
  filter(
    (is.na(vol_filtered_m3_150) | is.na(vol_filtered_m3_335)) & 
      instrument == "Bongo Net"  
  )

towData_edit2 <- towData_edit %>%
  mutate(
    # update volume filtered values for 335 um net based on conditions
    vol_filtered_m3_335 = case_when(
      (cruise == "AR38" & station == "L3") | 
        (cruise == "EN617" & station == "L11") |
        (cruise == "EN715" & station == "L6") |
        (cruise == "HRS2303" & station == "L9") |
        (cruise == "EN720" & station == "L11") |
        (cruise == "EN712" & station == "L8") |
        (cruise == "EN668" & station %in% c("L2", "L3", "L4")) |
        (cruise == "EN627") ~ GEAR_VOLUME_CALCULATED,
      is.na(vol_filtered_m3_335) & DNA_335 == "Y" ~ GEAR_VOLUME_CALCULATED,  # replace NA if DNA_335 is "Y"
      TRUE ~ vol_filtered_m3_335  # keep original if no condition is met
    ),
    
    vol_filtered_m3_150 = case_when(
      (cruise == "EN617" & station == "L11") |
        (cruise == "HRS2303" & station == "L9") |
        (cruise == "EN720" & station == "L11") |
        (cruise == "EN712" & station %in% c("L1", "L7", "L9", "L11")) |
        (cruise == "EN712" & station == "L8") |
        (cruise == "EN715" & station == "L6") |
        (cruise == "EN706" & station == "L5") |
        (cruise == "EN668" & station %in% c("L2", "L3", "L4")) |
        (cruise == "EN687" & station == "L8") |
        (cruise == "EN657" & station == "MVCO") |
        (cruise == "EN627") ~ GEAR_VOLUME_CALCULATED,
      is.na(vol_filtered_m3_150) & DNA_335 == "Y" ~ GEAR_VOLUME_CALCULATED, 
      TRUE ~ vol_filtered_m3_150  # keep original if no condition is met
    ),
    
    # add secondary flag for calculated volumes
    secondary_flag = ifelse(
     # !is.na(vol_filtered_m3_335) & vol_filtered_m3_335 == GEAR_VOLUME_CALCULATED,
      (!is.na(vol_filtered_m3_335) & vol_filtered_m3_335 == GEAR_VOLUME_CALCULATED) | 
        (!is.na(vol_filtered_m3_150) & vol_filtered_m3_150 == GEAR_VOLUME_CALCULATED),
      paste("volume sampled calculated based on ship speed and winch wire angle not flowmeter."),
      secondary_flag  # retain existing if not replaced
    ),
    
    # set primary_flag == 3 if GEAR_VOLUME_CALCULATED is used
    primary_flag = ifelse(
      #!is.na(vol_filtered_m3_335) & vol_filtered_m3_335 == GEAR_VOLUME_CALCULATED,
      (!is.na(vol_filtered_m3_335) & vol_filtered_m3_335 == GEAR_VOLUME_CALCULATED) | 
        (!is.na(vol_filtered_m3_150) & vol_filtered_m3_150 == GEAR_VOLUME_CALCULATED),
      3,  
      primary_flag  # retain existing if not replaced
    ),
    
    # for Ring Net samples, keep volumes as NA
    vol_filtered_m3_335 = ifelse(instrument == "Ring Net", NA, vol_filtered_m3_335),
    vol_filtered_m3_150 = ifelse(instrument == "Ring Net", NA, vol_filtered_m3_150)
  )


## ------------------------------------------ ##
#    Add haul factor -----
## ------------------------------------------ ##

# haul factor as specified for EcoMon & CalCOFI cruises

towData_fin <- towData_edit2 %>%
  mutate(haul_factor_10m2_335 = (net_max_depth_m*10)/vol_filtered_m3_335,
         haul_factor_10m2_150 = (net_max_depth_m*10)/vol_filtered_m3_150,
         haul_factor_100m3_335 = 100 / vol_filtered_m3_335,
         haul_factor_100m3_150 = 100 / vol_filtered_m3_150)


## ------------------------------------------ ##
#    Checking final volume values -----
## ------------------------------------------ ##
# side by side scatter and bar plots of volumes
uniq_cruises <- unique(towData_fin$cruise)

for (cruise in uniq_cruises) {
  
  # Filter the data for the current cruise, ensuring both vol_filtered_m3_150 and vol_filtered_m3_335 are not NA
  cruise_data <- towData_fin %>%
    filter(cruise == !!cruise & 
             !is.na(vol_filtered_m3_150) & 
             !is.na(vol_filtered_m3_335))
  
  # Create scatter plot with vol_filtered_m3_150 on x-axis and vol_filtered_m3_335 on y-axis
  scatter_plot <- ggplot(cruise_data, 
                         aes(x = vol_filtered_m3_150, 
                             y = vol_filtered_m3_335)) +
    geom_abline(slope = 1, intercept = 0, 
                linetype = "dashed", color = "darkgrey", size = 1.2) +
    geom_point(size = 3.2, color = "#00BFB2") +
    scale_y_continuous(limits = c(0, 600)) +  
    scale_x_continuous(limits = c(0, 600)) + 
    labs(title = paste("Cruise:", cruise), 
         x = "Volume Filtered 150 µm (m³)",
         y = "Volume Filtered 335 µm (m³)") +
    theme_few() +
    theme(axis.title = element_text(size = 15, color = "black"), 
          axis.text = element_text(size = 14, color = "black"))
  
  # Print the plot
  print(scatter_plot)
}


## ------------------------------------------ ##
## ------------------------------------------ ##
#    Select columns for final NES-LTER-TOW-Data -----
## ------------------------------------------ ##
zp_towmetadata_fin <- towData_fin %>%
  mutate(
    date_start_UTC = as.Date(date_time_start_UTC),  # Extract the date
    time_start_UTC = format(as.POSIXct(date_time_start_UTC), format="%H:%M:%S"),  # Extract the time
    date_end_UTC = as.Date(date_time_end_UTC),  # Extract the date
    time_end_UTC = format(as.POSIXct(date_time_end_UTC), format="%H:%M:%S"),  # Extract the time
    date_time_start_UTC = NULL,  # Remove original combined column if no longer needed
    date_time_end_UTC = NULL     # Remove original combined column if no longer needed
  )


zp_towmetadata_fin <- zp_towmetadata_fin %>%
  dplyr::select(cruise, station, cast, sample_name, 
                date_start_UTC, time_start_UTC, date_end_UTC, time_end_UTC,
                lat_start, lon_start, lat_end, lon_end,
                depth_bottom, depth_target, depth_TDR, net_max_depth_m, avg_angle,
                max_wire_out_m, wire_rate_out_m_min, wire_rate_in_m_min, STW_start,
                SOG_start, STW_end, SOG_end, flowmeter_sn_335, flow_start_335,
                flow_end_335, tot_flow_counts_335, vol_filtered_m3_335, 
                NOAA_335, DNA_335, flowmeter_sn_150, flow_start_150,
                flow_end_150, tot_flow_counts_150, vol_filtered_m3_150,
                morph_ID_150, DNA_150, size_fract_150, taxa_pick_150, size_fract_20, 
                haul_factor_10m2_335, haul_factor_10m2_150,
                haul_factor_100m3_335, haul_factor_100m3_150,
                comments, primary_flag, secondary_flag)

sapply(zp_towmetadata_fin, class)

zp_towmetadata_fin <- zp_towmetadata_fin %>%
  mutate(
    tot_flow_counts_150 = as.integer(tot_flow_counts_150)
  )

#write.csv(zp_towmetadata_fin, "output/nes-lter-zooplankton-tow-metadata-v2.csv")