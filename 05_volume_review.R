###############################################################################
##  NES-LTER Zooplankton Tow Metadata v3 — volume review (visual QA)
##  Project: nes-lter-zooplankton-tow-meta-v3
##  Script:  05_volume_review.R
##
##  Volume, the calculated-volume fallback, haul factors, ring-net NA-ing and
##  flags are all COMPUTED in 04_tow_metadata_assemble.R. This script does NOT
##  recompute them — it reads the published-ready CSV and produces the visual
##  checks (elapsed time, 335 vs 150, flowmeter vs calculated) that let you
##  eyeball which volumes look off. Numeric/structural QA lives in 06.
###############################################################################

library(here)
library(tidyverse)
library(ggthemes)   # theme_few
library(cowplot)    # plot_grid

## ------------------------------------------ ##
#   Load most recent published-ready CSV -----
## ------------------------------------------ ##
tow_files <- list.files(here("data", "processed"),
                        pattern = "^nes-lter-zooplankton-tow-metadata-v3-.*\\.csv$",
                        full.names = TRUE)
if (length(tow_files) == 0) stop("No nes-lter-zooplankton-tow-metadata-v3-*.csv in data/processed/")
tow_dates <- as.Date(str_extract(basename(tow_files), "\\d{8}"), "%Y%m%d")
tow <- read_csv(tow_files[which.max(tow_dates)], show_col_types = FALSE)
message("Volume review on: ", basename(tow_files[which.max(tow_dates)]))

## ------------------------------------------ ##
#   Constants + recompute the calculated (gear) volume FOR COMPARISON ONLY -----
## ------------------------------------------ ##
# This is NOT written anywhere — it's only so the plots can show, per tow,
# what the ship-speed*time volume WOULD be next to the published flowmeter one.
A_MOUTH  <- 0.2922      # net-mouth area (m^2), 0.61 m diameter
KT_TO_MS <- 0.514444

station_levels <- c("MVCO", "L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11","u11c")

tow <- tow %>%
  mutate(
    station = factor(station, levels = union(station_levels, unique(station))),
    dur_s = as.numeric(difftime(datetime_UTC_end, datetime_UTC_start, units = "secs")),
    dur_s = if_else(!is.na(dur_s) & dur_s < 0, dur_s + 24*3600, dur_s),
    speed_kt = coalesce(STW_start, STW_end, SOG_start, SOG_end),  # for comparison plot only
    gear_vol_calc = A_MOUTH * dur_s * abs(speed_kt) * KT_TO_MS
  )

bongo <- tow %>% filter(net_type == "bongo")

## ------------------------------------------ ##
#   1. Elapsed time per tow (sanity: tows ~4-6 min) -----
## ------------------------------------------ ##
ggplot(bongo %>% filter(!is.na(dur_s)),
       aes(x = station, y = dur_s)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ cruise, scales = "free_y", ncol = 5) +
  theme_minimal() +
  labs(y = "Elapsed time (s)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

ggplot(bongo %>% filter(!is.na(dur_s)),
       aes(x = station, y = dur_s)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Tow duration", y = "Elapsed time (s)") +
  theme(axis.title.x = element_blank())

## ------------------------------------------ ##
#   2. Published volume per net, per cruise -----
## ------------------------------------------ ##
ggplot(bongo %>% filter(!is.na(vol_filtered_335), vol_filtered_335 > 0),
       aes(x = station, y = vol_filtered_335)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cruise, scales = "free") +
  theme_minimal() +
  labs(y = "Volume filtered 335 µm (m³)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())

ggplot(bongo %>% filter(!is.na(vol_filtered_150), vol_filtered_150 > 0),
       aes(x = station, y = vol_filtered_150)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cruise, scales = "free") +
  theme_minimal() +
  labs(y = "Volume filtered 150 µm (m³)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())

## ------------------------------------------ ##
#   3. 335 vs 150 volume -----
## ------------------------------------------ ##
# should track 1:1 (same tow, both flowmeters)
for (cr in unique(bongo$cruise)) {
  d <- bongo %>% filter(cruise == cr, !is.na(vol_filtered_335), !is.na(vol_filtered_150))
  if (!nrow(d)) next
  print(
    ggplot(d, aes(x = vol_filtered_150, y = vol_filtered_335)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
      geom_point(size = 3, color = "#00BFB2") +
      coord_equal(xlim = c(0, 600), ylim = c(0, 600)) +
      labs(title = paste("Cruise:", cr),
           x = "Volume 150 µm (m³)", y = "Volume 335 µm (m³)") +
      theme_few()
  )
}

## ------------------------------------------ ##
#   4. Flowmeter vs calculated (gear) volume 
## ------------------------------------------ ##
# points off the 1:1 line are tows where the flowmeter and speed*time disagree 
# (flag-3 rowsor something to check)
vol_long <- bongo %>%
  pivot_longer(c(vol_filtered_335, vol_filtered_150),
               names_to = "mesh", values_to = "vol_filtered") %>%
  mutate(mesh = recode(mesh, vol_filtered_335 = "335 µm", vol_filtered_150 = "150 µm"))

for (cr in unique(bongo$cruise)) {
  d <- vol_long %>% filter(cruise == cr, !is.na(vol_filtered), !is.na(gear_vol_calc))
  if (!nrow(d)) next

  scatter <- ggplot(d, aes(x = gear_vol_calc, y = vol_filtered, color = mesh, shape = mesh)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
    geom_point(size = 3) +
    coord_equal(xlim = c(0, 600), ylim = c(0, 600)) +
    scale_color_manual(values = c("335 µm" = "#17356E", "150 µm" = "#00BFB2")) +
    labs(title = paste("Cruise:", cr), x = "Gear volume calculated (m³)", y = "Volume filtered (m³)") +
    theme_few() + theme(legend.title = element_blank(), legend.position = "bottom")

  bar <- vol_long %>%
    filter(cruise == cr) %>%
    ggplot(aes(x = station, y = vol_filtered, fill = mesh)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = c("335 µm" = "#17356E", "150 µm" = "#00BFB2")) +
    labs(title = paste("Cruise:", cr), y = "Volume filtered (m³)") +
    theme_few() +
    theme(axis.title = element_blank(), legend.title = element_blank(),
          legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

  print(plot_grid(scatter, bar, ncol = 2))
}

## ------------------------------------------ ##
#   5. Flag overview  -----
## ------------------------------------------ ##
# where volumes were calculated vs measured
# calculated-volume tows carry a flag 3 + secondary note from 04; show them
tow %>%
  filter(net_type == "bongo") %>%
  count(cruise, primary_flag) %>%
  pivot_wider(names_from = primary_flag, values_from = n, names_prefix = "flag_") %>%
  print(n = Inf)

# quick numeric peek: volume range per cruise (Inf-safe)
bongo %>%
  group_by(cruise) %>%
  summarise(min_v335 = min(vol_filtered_335, na.rm = TRUE),
            max_v335 = max(vol_filtered_335, na.rm = TRUE),
            min_v150 = min(vol_filtered_150, na.rm = TRUE),
            max_v150 = max(vol_filtered_150, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>%
  print(n = Inf)
