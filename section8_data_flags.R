## ========================================================================== ##
## 8) DATA FLAGS
## ========================================================================== ##
## QARTOD primary levels (IOC 54:V3):
##    1 = Good
##    3 = Suspect (derived value or partial loss)
##    4 = Bad (failed critical)
##    9 = Missing
##    (2 = not evaluated; avoid)
##
## primary_flag   = WORST level of any condition on the row
## secondary_flag = the notes for every condition on the row, in a fixed order
##
## Layout
##   8a  NOTE TABLE  every condition -> note text + level (edit wording ONLY here)
##   8b  DETECT      one TRUE/FALSE column per condition (.c_*)
##   8c  COMPOSE     notes + worst level -> fill flags for NEW cruises
##   8d  V2 EDITS    hand-checked fixes to published rows (all cruises)
##   8e  TIDY + QA
##
## Requires from earlier sections (run the script top to bottom, not this
## section on its own - appends are not safe to re-run):
##   .depth_src                       section 3 (which source filled net_max_depth)
##   .vol335_was_na, .vol150_was_na   section 6 (flowmeter volume missing)
##   .flow150_bad                     section 6 (150 flowmeter logged as off)
## -------------------------------------------------------------------------- ##

needed_cols  <- c(".depth_src", ".vol335_was_na", ".vol150_was_na", ".flow150_bad")
missing_cols <- setdiff(needed_cols, names(tow_meta))
if (length(missing_cols) > 0) {
  stop("Section 8 needs these columns from sections 3/6: ",
       paste(missing_cols, collapse = ", "))
}

tow_meta <- tow_meta %>%
  mutate(primary_flag = as.integer(primary_flag),   # one type for all flag math
         .row         = row_number())                # safe join key

## ========================================================================== ##
## 8a) NOTE TABLE
## ========================================================================== ##
## Row order = the order notes appear in secondary_flag.
## To add a condition: add a row here AND a matching .c_<cond> column in 8b.
flag_notes <- tribble(
  ~cond,              ~level, ~note,
  # --- depth provenance ---
  "depth_wire",        3L, "Depth recorder (TDR) and PX sensor data not available. Net max depth calculated from wire information (cosine law).",
  "depth_wire_ring",   3L, "Depth recorder (TDR) and PX sensor data not available. Net max depth taken from wire out (vertical tow).",
  "depth_target",      3L, "Target depth used for net max depth (TDR, PX, and wire data unavailable).",
  "hit_bottom",        3L, "Hit bottom.",
  # --- sample handling ---
  "cod_end",           3L, "Cod end issue.",
  "no_335",            3L, "No 335 micron sample (cod end lost).",
  "no_150",            3L, "No 150 micron sample (net ripped).",
  "spill_335",         3L, "Partial 335 micron sample spilled.",
  "spill_150",         3L, "Partial 150 micron sample spilled.",
  "salps",             3L, "Salps abundant.",
  # --- flowmeter / volume ---
  "windmill",          3L, "Flowmeter readings suspect (windmilling).",
  "vol_calc_both",     3L, "Volume sampled (335 and 150 um) calculated from ship speed and tow duration (flowmeter unavailable).",
  "vol_calc_335",      3L, "Volume sampled (335 um) calculated from ship speed and tow duration (flowmeter unavailable).",
  "vol_calc_150",      3L, "Volume sampled (150 um) calculated from ship speed and tow duration (flowmeter unavailable).",
  "flow150_bad",       3L, "Volume sampled (150 um) calculated from ship speed and tow duration; flowmeter reading unreliable."
)
# Missing data (9) is handled separately in 8c: it only applies when no other
# note does, so it never hides a more informative flag.
# Non-quantitative (4) is a global rule in 8d (applies to v2 rows too).

## ========================================================================== ##
## 8b) DETECT - one TRUE/FALSE column per condition
## ========================================================================== ##

## --- hand-mapped new-cruise rows (comments too tangled for regex) --- ##
spill_335 <- c("EN727_L9_B11", "AR88_L9_B15", "AR92_L11_B11", "AR95_L1_B1")
spill_150 <- c("AR88_L3_B18", "AR88_L2_B19", "AR95_L9_B16", "AR99_L1_B2",
               "HRS2609_L2_B2")
no_335    <- c("AR99_L3_B18")   # 335 cod end broke off during recovery
no_150    <- c("AR95_L6_B11")   # 150 net ripped, sample not processed

## --- comment patterns --- ##
rx_hit_bottom <- "(?<!didn't )(?<!did not )(?<!not )hit (the )?bottom"
rx_cod_end    <- "cod end (broke|came off)|\\bleak|loose mesh"
rx_windmill   <- "windmill"
rx_salps      <- "lots of salps|many salps|salpy|salped"

has <- function(x, rx) grepl(rx, x, ignore.case = TRUE, perl = TRUE)

tow_meta <- tow_meta %>%
  mutate(
    # 150 volume MISSING (not the "flowmeter off" case, which has its own note)
    .v150 = .vol150_was_na & !.flow150_bad,

    # --- depth provenance (from the source that actually filled net_max_depth)
    .c_depth_wire      = .depth_src %in% "wire" & net_type == "bongo",
    .c_depth_wire_ring = .depth_src %in% "wire" & net_type == "ring",
    .c_depth_target    = .depth_src %in% "target",
    # "hit bottom on the first deployment ... ran again" = clean redo, not flagged
    .c_hit_bottom      = has(comments, rx_hit_bottom) &
                         !has(comments, "first deployment"),

    # --- sample handling
    .c_cod_end   = has(comments, rx_cod_end) & !sample_name %in% no_335,
    .c_no_335    = sample_name %in% no_335,
    .c_no_150    = sample_name %in% no_150,
    .c_spill_335 = sample_name %in% spill_335,
    .c_spill_150 = sample_name %in% spill_150,
    .c_salps     = has(comments, rx_salps),

    # --- flowmeter / volume
    .c_windmill      = has(comments, rx_windmill),
    .c_vol_calc_both = .vol335_was_na & .v150,
    .c_vol_calc_335  = .vol335_was_na & !.v150,
    .c_vol_calc_150  = .v150 & !.vol335_was_na,
    .c_flow150_bad   = .flow150_bad,

    # --- missing data (used in 8c)
    .c_missing = coalesce(is.na(net_max_depth) |
                            (net_type == "bongo" & is.na(vol_filtered_335) &
                               is.na(vol_filtered_150)),
                          FALSE)
  )

## --- preview: new-cruise rows hit by each condition --- ##
tow_meta %>%
  filter(cruise %in% new_cruises) %>%
  summarise(across(starts_with(".c_"), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "n_rows") %>%
  print(n = Inf)

## ========================================================================== ##
## 8c) COMPOSE - build notes + worst level, fill NEW cruises only
## ========================================================================== ##
cond_cols <- paste0(".c_", flag_notes$cond)

new_flags <- tow_meta %>%
  filter(cruise %in% new_cruises) %>%
  select(.row, all_of(cond_cols)) %>%
  pivot_longer(-.row, names_to = "cond", values_to = "hit",
               names_prefix = "\\.c_") %>%
  filter(hit) %>%
  left_join(flag_notes, by = "cond") %>%
  arrange(.row, match(cond, flag_notes$cond)) %>%   # notes in table order
  group_by(.row) %>%
  summarise(.sec = str_c(note, collapse = " "),
            .sev = max(level),
            .groups = "drop")

tow_meta <- tow_meta %>%
  select(-any_of(c(".sec", ".sev"))) %>%
  left_join(new_flags, by = ".row") %>%
  mutate(
    .sev = coalesce(.sev, 1L),
    # missing data -> 9, only if nothing else was noted
    .sec = if_else(.c_missing & is.na(.sec),
                   "Required measurement missing (net max depth or volume filtered).",
                   .sec),
    .sev = if_else(.c_missing & .sev == 1L, 9L, .sev),
    # fill new cruises; v2 values untouched
    secondary_flag = if_else(cruise %in% new_cruises & is.na(secondary_flag),
                             .sec, secondary_flag),
    primary_flag   = if_else(cruise %in% new_cruises & is.na(primary_flag),
                             .sev, primary_flag)
  )

## --- review new-cruise flags --- ##
new_flags_review <- tow_meta %>%
  filter(cruise %in% new_cruises) %>%
  select(sample_name, net_type, primary_flag, secondary_flag, comments) %>%
  arrange(desc(primary_flag), sample_name)

new_flags_review %>%
  count(primary_flag, secondary_flag) %>%
  print(n = Inf, width = Inf)
# View(new_flags_review)

## ========================================================================== ##
## 8d) V2 EDITS - hand-checked fixes to published rows
## ========================================================================== ##

## ------------------------------------------ ##
#     8d.1 Wording clean-ups (all rows) -----
## ------------------------------------------ ##
rx_no20      <- regex("\\s*no 20\\s*um ring net sample\\.?", ignore_case = TRUE)
rx_manysalps <- regex("many salps in this cruise\\.?",       ignore_case = TRUE)

tow_meta <- tow_meta %>%
  mutate(
    secondary_flag = str_remove(secondary_flag, rx_no20),
    secondary_flag = str_replace(secondary_flag, rx_manysalps, "Salps abundant."),
    secondary_flag = na_if(str_squish(secondary_flag), "")
  )

## ------------------------------------------ ##
#     8d.2 v2 calculated-volume note -> net-specific -----
## ------------------------------------------ ##
# v2's note didn't say which net. Station lists come from the v2 script
# volume_water_sampled_calculations.R L536-563; EN627 = all stations.
# Only rows that ALREADY carry the old v2 note are rewritten.
rx_old_vol <- regex(
  "volume sampled calculated based on ship speed and winch wire angle not flowmeter\\.?",
  ignore_case = TRUE)

calc_335_keys <- tribble(
  ~cruise,   ~station,
  "AR38",    "L3",
  "EN617",   "L11",
  "EN715",   "L6",
  "HRS2303", "L9",
  "EN720",   "L11",
  "EN712",   "L8",
  "EN668",   "L2",
  "EN668",   "L3",
  "EN668",   "L4"
)

calc_150_keys <- tribble(
  ~cruise,   ~station,
  "EN617",   "L11",
  "HRS2303", "L9",
  "EN720",   "L11",
  "EN712",   "L1",
  "EN712",   "L7",
  "EN712",   "L9",
  "EN712",   "L11",
  "EN712",   "L8",
  "EN715",   "L6",
  "EN706",   "L5",
  "EN668",   "L2",
  "EN668",   "L3",
  "EN668",   "L4",
  "EN687",   "L8",
  "EN657",   "MVCO"
)

key <- function(cr, st) paste(cr, st)

tow_meta <- tow_meta %>%
  mutate(
    .old_vol = str_detect(coalesce(secondary_flag, ""), rx_old_vol),
    .v2_335  = key(cruise, station) %in% key(calc_335_keys$cruise, calc_335_keys$station) |
               cruise == "EN627",
    .v2_150  = key(cruise, station) %in% key(calc_150_keys$cruise, calc_150_keys$station) |
               cruise == "EN627",
    .v2_vol_note = case_when(
      .v2_335 & .v2_150 ~ "Volume sampled (335 and 150 um) calculated from ship speed and tow duration; flowmeter reading unreliable.",
      .v2_335           ~ "Volume sampled (335 um) calculated from ship speed and tow duration; flowmeter reading unreliable.",
      .v2_150           ~ "Volume sampled (150 um) calculated from ship speed and tow duration; flowmeter reading unreliable.",
      TRUE              ~ "Volume sampled calculated from ship speed and tow duration; flowmeter reading unreliable."
    ),
    secondary_flag = if_else(.old_vol,
                             str_squish(str_replace(secondary_flag, rx_old_vol, .v2_vol_note)),
                             secondary_flag)
  )

# CHECK: old note but no station-list match -> got the generic (no-net) note
tow_meta %>%
  filter(.old_vol, !.v2_335, !.v2_150) %>%
  select(sample_name, secondary_flag, comments)
# CHECK: on a station list but v2 never noted a calculated volume
tow_meta %>%
  filter(!cruise %in% new_cruises, net_type == "bongo",
         .v2_335 | .v2_150, !.old_vol) %>%
  select(sample_name, primary_flag, secondary_flag)

## ------------------------------------------ ##
#     8d.3 Non-quantitative -> 4 (all rows) -----
## ------------------------------------------ ##
# catches non-quant in comments that v2 missed; note is net-specific
tow_meta <- tow_meta %>%
  mutate(
    .nq_noted   = grepl("non.?quantitative", secondary_flag, ignore.case = TRUE),
    .nq_comment = grepl("non.?quantitative", comments,       ignore.case = TRUE),
    .nq_note = case_when(
      !.nq_comment | .nq_noted                        ~ NA_character_,
      grepl("335", comments) & grepl("150", comments) ~ "Non quantitative 335 and 150 micron sample.",
      grepl("335", comments)                          ~ "Non quantitative 335 micron sample.",
      grepl("150", comments)                          ~ "Non quantitative 150 micron sample.",
      TRUE                                            ~ "Non quantitative sample."
    ),
    secondary_flag = if_else(!is.na(.nq_note),
                             str_squish(paste(coalesce(secondary_flag, ""), .nq_note)),
                             secondary_flag),
    primary_flag   = if_else(.nq_noted | .nq_comment, 4L, primary_flag)
  )

# CHECK: rows that got a non-quant note added (expect AR63 L2, AR38 L6 among them)
tow_meta %>%
  filter(!is.na(.nq_note)) %>%
  select(sample_name, primary_flag, secondary_flag, comments) %>%
  print(n = Inf, width = Inf)

tow_meta <- tow_meta %>% select(-starts_with(".nq"))

## ------------------------------------------ ##
#     8d.4 Row-specific fixes -----
## ------------------------------------------ ##
# flag_rows(ids, note, level, mode):
#   "append"  add note (skipped if already there), level = max(current, level)
#   "replace" note and level set outright
#   "clear"   note removed, level set outright
#   strip     optional regex removed from the existing note first
# Warns if an id isn't found (catches typos in sample names).
flag_rows <- function(df, ids, note = NA_character_, level,
                      mode = c("append", "replace", "clear"), strip = NULL) {
  mode <- match.arg(mode)
  not_found <- setdiff(ids, df$sample_name)
  if (length(not_found) > 0) {
    warning("flag_rows(): sample_name not found: ",
            paste(not_found, collapse = ", "), call. = FALSE)
  }

  hit <- df$sample_name %in% ids
  old <- df$secondary_flag
  if (!is.null(strip)) old <- str_remove_all(old, regex(strip, ignore_case = TRUE))
  old <- na_if(str_squish(old), "")

  new_note <- switch(mode,
    append  = if_else(str_detect(coalesce(old, ""), fixed(note, ignore_case = TRUE)),
                      old,
                      str_squish(paste(coalesce(old, ""), note))),
    replace = rep(note, nrow(df)),
    clear   = rep(NA_character_, nrow(df))
  )
  new_level <- if (mode == "append") {
    pmax(coalesce(df$primary_flag, 1L), level)
  } else {
    rep(level, nrow(df))
  }

  df$secondary_flag <- if_else(hit, new_note, df$secondary_flag)
  df$primary_flag   <- if_else(hit, as.integer(new_level), df$primary_flag)
  df
}

ids_from_comment <- function(txt) tow_meta$sample_name[tow_meta$comments %in% txt]

## --- sample problems (append, 3) --- ##
salps_abundant <- c("AR38_L4_B14", "AR77_L10_B6", "EN712_L10_B7", "EN720_L4_B6",
                    "AR28B_L11_R12", "AR34B_L8_R9", "AR38_L3_B16",
                    "EN668_L10_B8", "EN668_L6_B18", "EN687_L4_B5",
                    "EN706_L10_B10", "EN715_L10_B7")

tangled_ids <- c("AR66B_L10_R6",    # twist between ring and cod end
                 "AR34B_L10_R21")   # ring cod end tangled around wire

lost_335 <- c("AR38_L9_B10",        # "335 um 5-10 ml spilled."
              "EN695_L10_B4",       # "Spilled small amount of 335 net... <100ml."
              "AR77_L1_B1")         # "335um cod end spilled 1/6 sample."
              # AR88_L9_B15 is a new cruise -> handled in 8b

lost_150 <- c("EN608_L2_B4",        # "Spilled about 5% of 150um sample..."
              "AR32_L3_B2",         # "Dumped 1/2 of 150 um..."
              "AR32_L5_B3",         # "Part of 150 um spilled about 5%..."
              "AR38_L3_B2",         # "5% of 150 um spilled."
              "EN687_L6_B18",       # "Spilled 1/2 of 150..."
              "HRS2303_L3_B11")     # "Lost 5mL of 150um sample."

lost_generic <- c("AR32_L6_B4")     # "Dumped 1/2 of sample." (no mesh named)

## --- hit bottom --- ##
hb_kept    <- c("EN687_L8_B16", "AR77_L8_B16", "AR77_L7_B17",
                "EN720_L3_B20", "EN720_MVCO_B24")
hb_partial <- c("EN655_L7_B11",     # 335 skimmed bottom, 150 kept
                "AT46_L4_B2",       # 335 hit bottom, some sample kept
                "EN627_L1_B3")      # only 20um size fraction kept
hb_nosample <- c("EN655_L9_B15",    # "No sample. Hit bottom."
                 "EN712_L6_B5")     # silty sediment, not processed

## --- no sample / calibration (replace, 9) --- ##
calib_ids       <- c("EN617_L11_B25A", "EN617_L11_B25B", "EN617_MVCO_B35B")
codend_nosample <- ids_from_comment("Cod end broke - no sample. OOI Pioneer cruise.")
rough_nosample  <- ids_from_comment("Deployed and recovered without sample. Conditions too rough to re do cast.")

## --- corrections (clear to 1) --- ##
redo_clean <- tow_meta$sample_name[grepl("3m offset on TDR readings",
                                         tow_meta$comments, fixed = TRUE)]
clear_ids <- c("EN715_L8_B14",      # flagged 3 in v2 by mistake
               "EN657_L1_B1",       # real TDR now available (v2 used estimate)
               redo_clean)          # hit bottom 1st deployment, clean redo

tow_meta <- tow_meta %>%
  flag_rows(salps_abundant,  "Salps abundant.",                       3L) %>%
  flag_rows(tangled_ids,     "Tangled cod end.",                      3L) %>%
  flag_rows(lost_335,        "Partial 335 micron sample spilled.",    3L) %>%
  flag_rows(lost_150,        "Partial 150 micron sample spilled.",    3L) %>%
  flag_rows(lost_generic,    "Partial sample spilled.",               3L) %>%
  flag_rows(hb_kept,         "Hit bottom. Sample kept.",              3L,
            strip = "hit bottom\\.?") %>%
  flag_rows(hb_partial,      "Hit bottom. Some sample kept.",         3L,
            strip = "hit bottom\\.?") %>%
  flag_rows(hb_nosample,     "Hit bottom. No sample collected.",      9L, mode = "replace") %>%
  flag_rows(codend_nosample, "Cod end issue. No sample collected.",   9L, mode = "replace") %>%
  flag_rows(rough_nosample,  "No sample collected.",                  9L, mode = "replace") %>%
  flag_rows(calib_ids,       "Calibration tow; no sample collected.", 9L, mode = "replace") %>%
  flag_rows(clear_ids,       level = 1L,                                  mode = "clear")

## ------------------------------------------ ##
#     8d.5 Cruise-level fixes -----
## ------------------------------------------ ##
# EN608 was never flagged in v2
tow_meta <- tow_meta %>%
  mutate(primary_flag = if_else(cruise == "EN608" & is.na(primary_flag),
                                1L, primary_flag))

## ========================================================================== ##
## 8e) TIDY + QA
## ========================================================================== ##

## --- sentence case each note --- ##
sentence_case_notes <- function(x) {
  if (is.na(x)) return(NA_character_)
  parts <- str_split(x, "(?<=\\.)\\s+")[[1]]                    # split after ". "
  parts <- str_replace(parts, "^\\s*(\\p{L})", \(m) toupper(m))
  str_squish(str_c(parts, collapse = " "))
}
tow_meta <- tow_meta %>%
  mutate(secondary_flag = map_chr(secondary_flag, sentence_case_notes))

## --- checks --- ##
# 1. every row flagged (QARTOD: flag everything)             -> expect 0 rows
tow_meta %>% filter(is.na(primary_flag)) %>% count(cruise)

# 2. every non-1 flag explains itself                        -> expect 0 rows
tow_meta %>%
  filter(primary_flag != 1L, is.na(secondary_flag)) %>%
  select(sample_name, primary_flag, comments)

# 3. flag 1 with a note (v2 may have a few informational ones; eyeball)
tow_meta %>%
  filter(primary_flag == 1L, !is.na(secondary_flag)) %>%
  select(sample_name, secondary_flag)

# 4. overall distribution + every distinct note
tow_meta %>% count(primary_flag)
tow_meta %>%
  count(primary_flag, secondary_flag) %>%
  arrange(primary_flag) %>%
  print(n = Inf, width = Inf)

## --- still to check --- ##
# [ ] EN715 L5, L6, L8 flags
# [ ] AR63 L2 and AR38 L6 carry the non-quant 4 (see 8d.3 check)
# [ ] v2 "Corrected 335/150 flowmeter value" / "Fixed 150 um flowmeter" rows: flag needed?
# [ ] EN712: why so many calculated volumes
# [ ] EN720 L11 B11: speeds exist now - why no flowmeter volume
# [ ] redo_clean row: confirm its only v2 note was "hit bottom" before clearing
# [ ] AR88_L8_B17 (flowmeters spun in air): flowmeter volume vs .vol_calc
# [ ] AR88_L4_B2 (tiny hole in 150 net): flag or not

## ------------------------------------------ ##
#     Drop working columns -----
## ------------------------------------------ ##
tow_meta <- tow_meta %>% select(-starts_with("."))
