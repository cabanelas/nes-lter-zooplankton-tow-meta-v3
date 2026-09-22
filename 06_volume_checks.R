################################################################################
## Script:  06_volume_checks.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Diagnostic plots to evaluate volume filtered (flowmeter) against the
##          geometric estimate (V = A*T*S), and record decisions on which volume
##          to use where a flowmeter looks bad.
##          Plots + decision table 
##
##    1. tow duration       (T; per cruise / station)
##    2. calculated volume  (V = A*T*S, same rules as 05 section 6)
##    3. flowmeter volumes  (335 and 150 by station / cruise)
##    4. flowmeter vs calc  (per-cruise comparison plots + check table)
##    5. spread + extremes  (all cruises by station)
##    6. decisions          (flowmeter vs calc)  ### PLACEHOLDER - TO DO
##    7. final checks       (150 vs 335 per cruise)
##
## Inputs (data/processed/):
##   - nes-lter-zooplankton-tow-metadata-v3-YYYYMMDD.csv (05_tow_metadata_assemble.R)
##
## Outputs:
##   - figures/06_volume_checks/*.png           (only if SAVE_FIGS = TRUE)
##   - data/processed/volume_decisions_v3.csv   (section 6, once filled in)
################################################################################

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)
library(ggthemes)   # theme_few()
library(cowplot)    # plot_grid()

## ------------------------------------------ ##
#            Constants -----
## ------------------------------------------ ##
# identical to 05
NET_DIAM_M       <- 0.61                    # 61-cm bongo mouth
A_MOUTH          <- pi * (NET_DIAM_M/2)^2   # 0.2922 m^2
KT_TO_MS         <- 0.514444                # knots -> m/s
FLOW_M_PER_COUNT <- 0.026873                # raw counts -> m (see 05 section 6)
SPEED_MIN_KT     <- 1                       # plausible tow-speed window (kt)
SPEED_MAX_KT     <- 4

# station order for plots (any station not listed is appended at the end)
STATION_LEVELS <- c("MVCO", "L1", "L2", "L3", "L4", "L5",
                    "L6", "L7", "L8", "L9", "L10", "L11", "u11c")

# volume labels + colors (same colors as v2 plots)
VOL_LABELS <- c(vol_filtered_150 = "150 µm",
                vol_filtered_335 = "335 µm",
                vol_calc         = "Calculated")
VOL_COLORS <- c("150 µm" = "#17356E", "335 µm" = "#00BFB2", 
                "Calculated" = "#CD4942")

# check-table tolerances (section 4) - starting points, adjust after looking
TOL_150_335  <- 1.25   # the two flowmeters should agree within 25%
TOL_FLOW_CALC <- 2     # flowmeter within 2x of A*T*S (ship speed != net speed)

# figures
SAVE_FIGS <- FALSE
FIG_DIR   <- here("figures", "06_volume_checks")

## ------------------------------------------ ##
#            Helpers -----
## ------------------------------------------ ##
# keep a speed only if it's in the plausible window, else NA (coalesce moves on)
valid_speed <- function(x) {
  if_else(!is.na(x) & abs(x) >= SPEED_MIN_KT & abs(x) <= SPEED_MAX_KT,
          abs(x), NA_real_)
}

# print a plot; also save it when SAVE_FIGS = TRUE
show_fig <- function(p, name, width = 10, height = 6) {
  print(p)
  if (SAVE_FIGS) {
    dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(FIG_DIR, paste0(name, ".png")), p,
           width = width, height = height, dpi = 300, bg = "white")
  }
  invisible(p)
}

# highest and lowest value per station (for labelling extremes)
station_extremes <- function(df) {
  bind_rows(
    df %>% group_by(station) %>% slice_max(value, n = 1, with_ties = FALSE),
    df %>% group_by(station) %>% slice_min(value, n = 1, with_ties = FALSE)
  ) %>%
    ungroup()
}

# shared theme for the station-by-cruise point plots
theme_spread <- theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text  = element_text(size = 11, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "black"),
        axis.text.x  = element_text(size = 15, color = "black"),
        axis.text.y  = element_text(size = 12, color = "black"),
        plot.title   = element_text(size = 18, face = "bold", color = "black"))

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
## --- latest tow metadata from 05 --- ##
files <- list.files(here("data", "processed"),
                    pattern = "^nes-lter-zooplankton-tow-metadata-v3-\\d{8}\\.csv$",
                    full.names = TRUE)
if (length(files) == 0) {
  stop("No nes-lter-zooplankton-tow-metadata-v3-*.csv in data/processed/ - run 05 first")
}
dates  <- as.Date(str_extract(basename(files), "\\d{8}"), "%Y%m%d")
latest <- files[which.max(dates)]
message("Reading tow metadata: ", basename(latest))
tow_meta <- read_csv(latest, show_col_types = FALSE)

## --- bongo tows only (ring nets: no flowmeter, not quantitative by volume) --- ##
tows <- tow_meta %>%
  filter(net_type == "bongo") %>%
  mutate(station = factor(station,
                          levels = union(STATION_LEVELS, sort(unique(station)))))

## ========================================================================== ##
## 1) TOW DURATION   (T)
## ========================================================================== ##
tows <- tows %>%
  mutate(
    dur_s = as.numeric(difftime(datetime_UTC_end, datetime_UTC_start,
                                units = "secs")),
    # negative = tow crossed midnight but end date wasn't rolled forward
    dur_s = if_else(dur_s < 0, dur_s + 24 * 3600, dur_s)
  )

# tows with no duration (missing start or end time) -> no calculated volume
tows %>% filter(is.na(dur_s)) %>% count(cruise)

## ------------------------------------------ ##
#     Duration per cruise (one plot each) -----
## ------------------------------------------ ##
for (cr in unique(tows$cruise)) {
  p <- ggplot(filter(tows, cruise == cr), aes(x = station, y = dur_s)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = paste("Cruise:", cr), x = "Station",
         y = "Elapsed Time (seconds)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  show_fig(p, paste0("01_duration_", cr))
}

## ------------------------------------------ ##
#     Duration: all cruises by station -----
## ------------------------------------------ ##
p <- ggplot(filter(tows, station != "u11c"),
            aes(x = station, y = dur_s, color = cruise)) +
  geom_jitter(size = 2.3) +
  labs(title = "Tow total time", y = "Elapsed Time (seconds)") +
  theme_spread
show_fig(p, "01_duration_all_points")

p <- ggplot(filter(tows, station != "u11c"), aes(x = station, y = dur_s)) +
  geom_boxplot() +
  labs(title = "Tow total time", y = "Elapsed Time (seconds)") +
  theme_spread
show_fig(p, "01_duration_all_boxplot")

p <- ggplot(filter(tows, !is.na(dur_s), station != "u11c"),
            aes(x = station, y = dur_s)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
  labs(y = "Elapsed Time (seconds)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  facet_wrap(~ cruise, scales = "free_y", ncol = 5)
show_fig(p, "01_duration_facet", width = 14, height = 12)

## ========================================================================== ##
## 2) CALCULATED VOLUME   (V = A * T * S)
## ========================================================================== ##
#   A = net mouth area (m^2)  -> A_MOUTH
#   T = tow duration (s)      -> dur_s (section 1)
#   S = tow speed (m/s)       -> best valid ship speed, converted from knots
# Speed priority (same as 05): STW_start -> STW_end -> SOG_start -> SOG_end
tows <- tows %>%
  mutate(
    speed_kt  = coalesce(valid_speed(STW_start), valid_speed(STW_end),
                         valid_speed(SOG_start), valid_speed(SOG_end)),
    speed_src = case_when(
      !is.na(valid_speed(STW_start)) ~ "STW_start",
      !is.na(valid_speed(STW_end))   ~ "STW_end",
      !is.na(valid_speed(SOG_start)) ~ "SOG_start",
      !is.na(valid_speed(SOG_end))   ~ "SOG_end",
      TRUE                           ~ NA_character_
    ),
    vol_calc = A_MOUTH * dur_s * speed_kt * KT_TO_MS,

    # volume each flowmeter actually recorded (independent of any substitution;
    # differs from vol_filtered_* where a calculated volume was used instead)
    vol_flow_335 = tot_flow_counts_335 * FLOW_M_PER_COUNT * A_MOUTH,
    vol_flow_150 = tot_flow_counts_150 * FLOW_M_PER_COUNT * A_MOUTH
  )

# which speed each cruise relied on (Endeavor = SOG only)
tows %>% count(cruise, speed_src) %>% print(n = Inf)

# rows with no usable speed or duration -> no calculated volume
tows %>%
  filter(is.na(vol_calc)) %>%
  select(sample_name, dur_s, STW_start, STW_end, SOG_start, SOG_end)

## ------------------------------------------ ##
#     Long tables for plotting -----
## ------------------------------------------ ##
# all three volumes (335, 150, calculated)
vol_long <- tows %>%
  pivot_longer(cols = c(vol_filtered_150, vol_filtered_335, vol_calc),
               names_to = "volume_type", values_to = "value") %>%
  mutate(volume_type = factor(recode(volume_type, !!!VOL_LABELS),
                              levels = unname(VOL_LABELS)))

# flowmeter volumes only, with vol_calc kept as its own column (for scatter)
flow_long <- tows %>%
  pivot_longer(cols = c(vol_filtered_150, vol_filtered_335),
               names_to = "volume_type", values_to = "vol_filtered") %>%
  mutate(volume_type = factor(recode(volume_type, !!!VOL_LABELS),
                              levels = unname(VOL_LABELS)[1:2]))

## ========================================================================== ##
## 3) FLOWMETER VOLUMES   (335 and 150)
## ========================================================================== ##
plot_vol_bars <- function(col, ylab, scales) {
  ggplot(filter(tows, !is.na(.data[[col]]), .data[[col]] > 0, station != "u11c"),
         aes(x = station, y = .data[[col]])) +
    geom_col() +
    facet_wrap(~ cruise, scales = scales) +
    theme_minimal() +
    labs(y = ylab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())
}

## --- 335 um: free scales, then fixed scales --- ##
show_fig(plot_vol_bars("vol_filtered_335", "Volume Filtered 335 µm", "free"),
         "03_vol335_facet_free", width = 14, height = 12)
show_fig(plot_vol_bars("vol_filtered_335", "Volume Filtered 335 µm", "fixed"),
         "03_vol335_facet_fixed", width = 14, height = 12)

## --- 150 um: free scales, then fixed scales --- ##
show_fig(plot_vol_bars("vol_filtered_150", "Volume Filtered 150 µm", "free"),
         "03_vol150_facet_free", width = 14, height = 12)
show_fig(plot_vol_bars("vol_filtered_150", "Volume Filtered 150 µm", "fixed"),
         "03_vol150_facet_fixed", width = 14, height = 12)

## ------------------------------------------ ##
#     150 vs 335 (should be similar) -----
## ------------------------------------------ ##
p <- ggplot(flow_long, aes(x = station, y = vol_filtered, fill = volume_type)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = VOL_COLORS) +
  facet_wrap(~ cruise, scales = "free") +
  theme_minimal() +
  labs(x = "Station", y = "Total Flow (m³)", fill = "Volume Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
show_fig(p, "03_vol150v335_facet", width = 14, height = 12)

# same, one plot per cruise
for (cr in unique(flow_long$cruise)) {
  p <- ggplot(filter(flow_long, cruise == cr),
              aes(x = station, y = vol_filtered, fill = volume_type)) +
    geom_col(position = position_dodge()) +
    scale_fill_manual(values = VOL_COLORS) +
    theme_minimal() +
    labs(title = paste("Cruise:", cr), x = "Station",
         y = "Total Flow (m³)", fill = "Volume Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  show_fig(p, paste0("03_vol150v335_", cr))
}

## ========================================================================== ##
## 4) FLOWMETER vs CALCULATED
## ========================================================================== ##

## ------------------------------------------ ##
#     Bars: 335, 150, calculated per cruise -----
## ------------------------------------------ ##
for (cr in unique(vol_long$cruise)) {
  p <- ggplot(filter(vol_long, cruise == cr),
              aes(x = station, y = value, fill = volume_type)) +
    geom_col(position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = VOL_COLORS) +
    theme_minimal() +
    labs(title = paste("Cruise:", cr), x = "Station",
         y = "Total Volume (m³)", fill = "Volume Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  show_fig(p, paste0("04_flow_vs_calc_bars_", cr))
}

## ------------------------------------------ ##
#     Scatter (1:1) + bars side by side per cruise -----
## ------------------------------------------ ##
# points off the dashed 1:1 line = flowmeter disagrees with A*T*S
# (coord_cartesian zooms to 0-600 without dropping points outside it)
for (cr in unique(tows$cruise)) {

  # left panel: flowmeter vs calculated
  scatter_plot <- ggplot(filter(flow_long, cruise == cr, !is.na(vol_filtered)),
                         aes(x = vol_calc, y = vol_filtered,
                             color = volume_type, shape = volume_type)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "darkgrey", linewidth = 1.2) +
    geom_point(size = 3.2) +
    coord_cartesian(xlim = c(0, 600), ylim = c(0, 600)) +
    scale_color_manual(values = VOL_COLORS) +
    labs(title = paste("Cruise:", cr),
         x = "Gear Volume Calculated (m³)",
         y = "Volume Filtered (m³)") +
    theme_few() +
    theme(legend.title = element_blank(),
          legend.text  = element_text(size = 15, color = "black"),
          axis.title   = element_text(size = 15, color = "black"),
          axis.text    = element_text(size = 14, color = "black"),
          legend.position = "bottom")

  # right panel: bars by station
  bar_plot <- ggplot(filter(vol_long, cruise == cr),
                     aes(x = station, y = value, fill = volume_type)) +
    geom_col(position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = VOL_COLORS) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = paste("Cruise:", cr), y = "Volume Filtered (m³)") +
    theme_few() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text    = element_text(size = 14, color = "black"),
          legend.title = element_blank(),
          legend.text  = element_text(size = 15, color = "black"),
          legend.position = "bottom",
          axis.text.x  = element_text(angle = 45, hjust = 1))

  combined_plot <- plot_grid(scatter_plot, bar_plot, ncol = 2)
  show_fig(combined_plot, paste0("04_flow_vs_calc_combined_", cr),
           width = 14, height = 6)
}

## ------------------------------------------ ##
#     Check table: which volumes look off? -----
## ------------------------------------------ ##
# ratios use the RAW flowmeter volumes (vol_flow_*), so tows where a calculated
# volume was already substituted can still be judged on what the meter said.
#   r_150_335      : the two flowmeters vs each other (expect ~1)
#   r_flow335_calc : 335 flowmeter vs A*T*S (noisy; ship speed != net speed)
#   r_flow150_calc : 150 flowmeter vs A*T*S
#   subst_335/150  : reported volume differs from what the flowmeter recorded
#                    (calculated volume used, or a corrected flowmeter value)
outside <- function(r, tol) coalesce(abs(log(r)) > log(tol), FALSE)

vol_check <- tows %>%
  transmute(
    sample_name, cruise, station, speed_src, dur_s, speed_kt,
    vol_filtered_335, vol_flow_335, vol_filtered_150, vol_flow_150, vol_calc,
    r_150_335      = vol_flow_150 / vol_flow_335,
    r_flow335_calc = vol_flow_335 / vol_calc,
    r_flow150_calc = vol_flow_150 / vol_calc,
    subst_335 = coalesce(abs(vol_filtered_335 - vol_flow_335) > 0.5, FALSE),
    subst_150 = coalesce(abs(vol_filtered_150 - vol_flow_150) > 0.5, FALSE),
    check = outside(r_150_335, TOL_150_335) |
            outside(r_flow335_calc, TOL_FLOW_CALC) |
            outside(r_flow150_calc, TOL_FLOW_CALC),
    comments, secondary_flag
  )

# tows worth a look (outside tolerance), worst meter disagreement first
vol_check %>%
  filter(check) %>%
  arrange(desc(abs(log(r_150_335)))) %>%
  select(sample_name, r_150_335, r_flow335_calc, r_flow150_calc,
         subst_335, subst_150, comments) %>%
  print(n = Inf, width = Inf)

# where a calculated/corrected volume is already in use
vol_check %>%
  filter(subst_335 | subst_150) %>%
  select(sample_name, subst_335, subst_150,
         vol_filtered_335, vol_flow_335, vol_filtered_150, vol_flow_150,
         vol_calc, secondary_flag) %>%
  print(n = Inf, width = Inf)

# View(vol_check)

## ========================================================================== ##
## 5) SPREAD + EXTREMES   (all cruises by station)
## ========================================================================== ##
vol_long_st <- filter(vol_long, station != "u11c", !is.na(value))

## --- all volumes, by station --- ##
p <- ggplot(vol_long_st, aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  theme_spread
show_fig(p, "05_spread_all")

## --- label the very large volumes (>= 850 m3) --- ##
p <- ggplot(vol_long_st, aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  geom_text(data = filter(vol_long_st, value >= 850),
            aes(label = cruise), vjust = -1, size = 4, color = "black") +
  theme_spread
show_fig(p, "05_spread_all_labeled_850")

## --- without the very large volumes --- ##
p <- ggplot(filter(vol_long_st, value < 850),
            aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  theme_spread
show_fig(p, "05_spread_under850")

## --- flowmeter only, no EN627, < 850: label max/min per station --- ##
flow_only <- vol_long_st %>%
  filter(volume_type != "Calculated", cruise != "EN627", value < 850)

p <- ggplot(flow_only, aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  geom_text(data = station_extremes(flow_only),
            aes(label = cruise), vjust = -1, size = 4, color = "black") +
  theme_spread
show_fig(p, "05_extremes_flowmeter")

## --- all volumes incl. calculated, no EN627, < 850: label max/min --- ##
all_under850 <- vol_long_st %>%
  filter(cruise != "EN627", value < 850)

p <- ggplot(all_under850, aes(x = station, y = value, color = cruise)) +
  geom_point(size = 2.3) +
  geom_text(data = station_extremes(all_under850),
            aes(label = cruise), vjust = -1, size = 4, color = "black") +
  theme_spread
show_fig(p, "05_extremes_all")

## ========================================================================== ##
## 6) DECISIONS: flowmeter vs calculated volume   ### PLACEHOLDER - TO DO
## ========================================================================== ##
## Rule of thumb: if a flowmeter failed or reads clearly wrong -> use calculated.
##
## --- v2 decisions (already applied in published v2; for provenance) --- ##
# AR38    L3               : 335 -> calculated
# EN617   L11              : both nets -> calculated
# HRS2303 L9               : both nets -> calculated
# EN720   L11              : both nets -> calculated
# EN715   L6               : both nets -> calculated
# EN712   L1, L7, L9, L11  : 150 -> calculated
# EN712   L8               : both nets -> calculated
# EN706   L5               : 150 -> calculated
# EN687   L8               : 150 -> calculated
# EN668   L2, L3, L4       : both nets -> calculated
# EN657   MVCO             : 150 -> calculated
# EN627   all stations     : both nets -> calculated
# v2 calculated volumes used STW with a 1.5 kt fallback; v3 recalculation
# would differ slightly - decide whether to keep v2 values as published.
#
## --- v3 new cruises: review these after the plots above --- ##
# [ ] AR99 (all bongos)  : 150 flowmeter logged as "off"
# [ ] AR95 L1, L2, L4, L5, L10, L11 : 150 flowmeter "not working properly"
# [ ] HRS2601 L4, HRS2609 L4        : 150 flowmeter "off"
# [ ] EN727 L3, L9, MVCO : windmilling of flowmeters noted
# [ ] AR88 L8 B17        : flowmeters spun ~30 s in air
# [ ] anything else flagged in the section 4 check table
#
## --- open v2 questions --- ##
# [ ] EN712: why so many calculated volumes
# [ ] EN720 L11 B11: speeds exist now - why no flowmeter volume
#
## --- decision table --- ##
# one row per net that should use the calculated volume
#   net      = "335" or "150"
#   decision = "use_calc" (only option for now)
volume_decisions <- tribble(
  ~sample_name,   ~net,  ~decision,  ~reason
  # "AR99_L2_B3", "150", "use_calc", "150 flowmeter logged as off",
)

# HOW THESE GET APPLIED (to set up once decisions exist):
#   05 section 6 reads this file and, for each listed net, replaces the
#   flowmeter volume with .vol_calc (same A*T*S as here). Section 7 then
#   recomputes that net's haul factors, and section 8 writes the
#   "flowmeter reading unreliable" note.
#   Run order: 05 -> 06 (look + decide) -> 05 again.
if (nrow(volume_decisions) > 0) {
  write_csv(volume_decisions,
            here("data", "processed", "volume_decisions_v3.csv"))
}

## ========================================================================== ##
## 7) FINAL CHECKS   (150 vs 335 per cruise)
## ========================================================================== ##
# after decisions are applied in 05 and it is re-run, points should sit near
# the 1:1 line; tows far off it still need a look
for (cr in unique(tows$cruise)) {
  cruise_data <- tows %>%
    filter(cruise == cr, !is.na(vol_filtered_150), !is.na(vol_filtered_335))

  p <- ggplot(cruise_data, aes(x = vol_filtered_150, y = vol_filtered_335)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "darkgrey", linewidth = 1.2) +
    geom_point(size = 3.2, color = "#00BFB2") +
    coord_cartesian(xlim = c(0, 600), ylim = c(0, 600)) +
    labs(title = paste("Cruise:", cr),
         x = "Volume Filtered 150 µm (m³)",
         y = "Volume Filtered 335 µm (m³)") +
    theme_few() +
    theme(axis.title = element_text(size = 15, color = "black"),
          axis.text  = element_text(size = 14, color = "black"))
  show_fig(p, paste0("07_final_150v335_", cr), width = 7, height = 6)
}
