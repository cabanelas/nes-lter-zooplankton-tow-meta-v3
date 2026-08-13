tdr <- read_csv(here("data", "raw",
                           "nes-lter-bongo-tdr-offsets.csv"))

tow_meta_v2 %>%
  select(cruise, station, cast, depth_TDR) %>%
  inner_join(
    tdr %>% mutate(cast = stringr::str_remove(cast, "^B")),
    by = c("cruise", "station", "cast")
  ) %>%
  mutate(
    d_raw       = depth_TDR - tdr_max_depth_m,               # ≈ 0  → v2 stored the RAW reading
    d_corrected = depth_TDR - (tdr_max_depth_m + offset_m)   # ≈ 0  → v2 stored the CORRECTED value
  ) %>%
  select(cruise, station, cast, depth_TDR, tdr_max_depth_m, offset_m, d_raw, d_corrected)


comparison <- tow_meta_v2 %>%
  select(cruise, station, cast, depth_TDR) %>%
  inner_join(tdr %>% mutate(cast = stringr::str_remove(cast, "^B")),
             by = c("cruise","station","cast")) %>%
  mutate(d_raw       = depth_TDR - tdr_max_depth_m,
         d_corrected = depth_TDR - (tdr_max_depth_m + offset_m))


comparison %>%
  filter(offset_m != 0, !is.na(d_raw)) %>%
  group_by(cruise) %>%
  summarise(
    n            = n(),
    offset       = paste(unique(offset_m), collapse = ","),
    med_abs_raw  = median(abs(d_raw)),
    med_abs_corr = median(abs(d_corrected)),
    verdict      = if_else(median(abs(d_raw)) < median(abs(d_corrected)),
                           "RAW", "CORRECTED"),
    .groups = "drop"
  )




preview <- comparison %>%
  mutate(
    good_depth = round(tdr_max_depth_m + offset_m, 2),
    delta      = round(good_depth - depth_TDR, 2),
    status = case_when(
      offset_m == 0                  ~ "no offset",
      abs(d_raw) <= abs(d_corrected) ~ "was RAW",
      TRUE                           ~ "was CORRECTED"
    )
  ) %>%
  select(cruise, station, cast, depth_TDR, tdr_max_depth_m,
         offset_m, good_depth, delta, status)

# only rows that actually move
preview %>% filter(abs(delta) > 0.1) %>% arrange(desc(abs(delta))) %>% print(n = Inf)
