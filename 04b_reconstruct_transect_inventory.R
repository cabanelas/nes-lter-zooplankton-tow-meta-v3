################################################################################
## Script:  04b_reconstruct_transect_inventory.R
## Purpose: Rebuild nes-lter-zooplankton-transect-inventory-v3 from the raw
##          long-format sample inventory logsheets (cruise/station/cast/
##          cast_type/net_type/mesh/purpose/...), since the v3 working copy
##          was lost. Pivots long -> wide to match the lost file's structure.
##          Manual QA/fixes still required after this runs - this gets you
##          most of the way, not all the way.
################################################################################

library(here)
library(tidyverse)
library(readxl)
library(janitor)

## ------------------------------------------ ##
#            Read raw long-format logsheets -----
## ------------------------------------------ ##
## adjust path/pattern to wherever these raw long-format files actually live -
## may be the SAME files 04_sample_inventory_combine.R reads, or a different,
## more granular source. Check which.
raw_dir <- here("data", "raw", "sample_inventory")  # adjust if different

raw_files <- list.files(raw_dir, pattern = "\\.xlsx$|\\.csv$",
                        full.names = TRUE, recursive = TRUE)

raw_list <- raw_files %>%
  set_names(basename(.)) %>%
  map(~ {
    if (str_ends(.x, "\\.csv")) read_csv(.x) else read_excel(.x)
  } %>% clean_names())

raw_all <- bind_rows(raw_list, .id = "source_file") %>%
  mutate(across(c(cruise, station, cast, cast_type, net_type, mesh, purpose),
                as.character))

## sanity check columns actually present before proceeding
glimpse(raw_all)

## ------------------------------------------ ##
#      Build the mesh+purpose -> target-column key -----
## ------------------------------------------ ##
## This mapping is the crux of the reconstruction - it says which
## mesh/net_type/purpose combo becomes which wide column. Based on the
## column names you already use downstream (mesh_335_noaa, mesh_335_tar_dna,
## mesh_150_morphid, mesh_150_tar_dna, mesh_150_taxa_pick, mesh_150_size_fract,
## mesh_20_size_fract), infer the mapping - ADJUST to match what you actually
## remember the logsheet's mesh/purpose values were.
target_key <- tribble(
  ~mesh, ~net_type, ~purpose,          ~target_col,
  "335", "Bongo",   "NOAA",             "mesh_335_noaa",
  "335", "Bongo",   "DNA",              "mesh_335_tar_dna",
  "150", "Bongo",   "morphID",          "mesh_150_morphid",
  "150", "Bongo",   "DNA",              "mesh_150_tar_dna",
  "150", "Bongo",   "taxapicking",      "mesh_150_taxa_pick",
  "150", "Bongo",   "stableisotope",    "mesh_150_size_fract",
  "20",  "Ring",    "stableisotope",    "mesh_20_size_fract"
)

## flag any raw mesh/net_type/purpose combos that DON'T match the key above -
## these are either a mapping gap to fix, or a legitimately new category
raw_all %>%
  distinct(mesh, net_type, purpose) %>%
  anti_join(target_key, by = c("mesh", "net_type", "purpose")) %>%
  arrange(mesh, net_type, purpose) %>%
  print(n = Inf)
## >>> REVIEW this output before proceeding - unmapped combos won't appear
## >>> in the final wide table unless you add them to target_key above.

## ------------------------------------------ ##
#      Collapse to per-cast indicator (0/1) -----
## ------------------------------------------ ##
## a cast "has" a given target column if ANY row for that
## cruise/station/cast/mesh/net_type/purpose combo exists with jars > 0
## (adjust the "has sample" logic if no_of_jars isn't the right test - e.g.
## some rows might exist as a placeholder even with 0 jars)
indicator_long <- raw_all %>%
  inner_join(target_key, by = c("mesh", "net_type", "purpose")) %>%
  mutate(has_sample = if_else(as.numeric(no_of_jars) > 0, 1L, 0L,
                              missing = 0L)) %>%
  group_by(cruise, station, cast, target_col) %>%
  summarize(has_sample = max(has_sample, na.rm = TRUE), .groups = "drop")

## pivot to wide - one row per cast, one column per target_col
inventory_wide <- indicator_long %>%
  pivot_wider(names_from = target_col, values_from = has_sample, values_fill = 0)

## ------------------------------------------ ##
#      Reattach comments (collapse duplicates per cast) -----
## ------------------------------------------ ##
comments_by_cast <- raw_all %>%
  filter(!is.na(comments_logsheets) | !is.na(comments_re_missing_sample)) %>%
  group_by(cruise, station, cast) %>%
  summarize(
    comments = paste(
      na.omit(unique(comments_logsheets)),
      na.omit(unique(comments_re_missing_sample)),
      collapse = "; "
    ) %>% na_if(""),
    .groups = "drop"
  )

inventory_wide <- inventory_wide %>%
  left_join(comments_by_cast, by = c("cruise", "station", "cast")) %>%
  mutate(source_file = cruise) %>%
  relocate(source_file, cruise, station, cast)

## ------------------------------------------ ##
#      Ensure all target columns exist even if unused -----
## ------------------------------------------ ##
missing_cols <- setdiff(target_key$target_col, names(inventory_wide))
if (length(missing_cols) > 0) {
  inventory_wide[missing_cols] <- 0L
  message("Added missing target columns with 0: ", paste(missing_cols, collapse = ", "))
}

## ------------------------------------------ ##
#      QA -----
## ------------------------------------------ ##
## every cast should appear exactly once
inventory_wide %>% count(cruise, station, cast) %>% filter(n > 1)

## cast normalization - match your existing B/R-strip + decimal-strip logic
inventory_wide <- inventory_wide %>%
  mutate(cast = cast %>% str_remove("^[BR]") %>% str_remove("\\.0+$"))

## ------------------------------------------ ##
#      Cross-check against tow_meta's Y/N columns -----
## ------------------------------------------ ##
## tow_meta already has NOAA_335, DNA_335, morph_ID_150, DNA_150,
## size_fract_150, taxa_pick_150, size_fract_20 as Y/N - compare against the
## reconstructed 0/1 columns as a sanity check on the mapping above
cross_check <- tow_meta %>%
  select(cruise, station, cast, NOAA_335, DNA_335, morph_ID_150, DNA_150,
         size_fract_150, taxa_pick_150, size_fract_20) %>%
  left_join(inventory_wide, by = c("cruise", "station", "cast"),
            suffix = c("_towmeta", "_recon"))

## flag mismatches: tow_meta says Y but reconstruction says 0, or vice versa
cross_check %>%
  mutate(
    mismatch_noaa335 = (NOAA_335 == "Y")       != (mesh_335_noaa == 1),
    mismatch_dna335  = (DNA_335 == "Y")        != (mesh_335_tar_dna == 1),
    mismatch_morph   = (morph_ID_150 == "Y")   != (mesh_150_morphid == 1),
    mismatch_dna150  = (DNA_150 == "Y")        != (mesh_150_tar_dna == 1),
    mismatch_sf150   = (size_fract_150 == "Y") != (mesh_150_size_fract == 1),
    mismatch_taxa150 = (taxa_pick_150 == "Y")  != (mesh_150_taxa_pick == 1),
    mismatch_sf20    = (size_fract_20 == "Y")  != (mesh_20_size_fract == 1)
  ) %>%
  filter(if_any(starts_with("mismatch_"), ~ .x == TRUE)) %>%
  select(cruise, station, cast, starts_with("mismatch_")) %>%
  print(n = Inf)

## ------------------------------------------ ##
#      Write -----
## ------------------------------------------ ##
stamp <- format(Sys.Date(), "%Y%m%d")
write_csv(inventory_wide, here("data", "processed",
                               glue::glue("nes-lter-zooplankton-transect-inventory-v3-reconstructed-{stamp}.csv")))