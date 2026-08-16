################################################################################
## Script:  sample_inventory_combine.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Read the per-cruise sample-inventory workbooks, clean names, stack
##          them into one long table, and write a combined CSV. Downstream this
##          feeds the size_fract_20 column in the tow-metadata objects.
## Including all cruises to date 
## Inputs  (data/raw/sample_inventory/):
##   - <CRUISE>_SampleInventory.xlsx   (one per cruise)
## Output  (data/processed/):
##   - sample_inventory_combined-YYYYMMDD.csv
################################################################################
# no sample inventory file available for: AR28B, AR34B, AR39B, AR66B

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(tidyverse)
library(readxl)
library(janitor)

## ------------------------------------------ ##
#            Read -----
## ------------------------------------------ ##
inv_dir <- here("data", "raw", "sample_inventory")

inv_files <- list.files(inv_dir, pattern = "_SampleInventory\\.xlsx$",
                        full.names = TRUE, recursive = TRUE)
message("Found ", length(inv_files), " inventory files:\n  ",
        paste(basename(inv_files), collapse = "\n  "))

## read each, clean names, keep the source file for traceability
inv_list <- inv_files %>%
  set_names(str_remove(basename(.), "_SampleInventory\\.xlsx$")) %>%
  map(~ read_excel(.x) %>%
        clean_names() %>%
        rename(any_of(c(cast = "tow"))) %>%   # EN644 logsheet used 'tow'
        mutate(across(everything(), as.character)))   # avoid type clashes on bind

## ------------------------------------------ ##
#      Column-consistency check -----
## ------------------------------------------ ##
# stack is only clean if every file has the same columns. report any that don't
# before binding, so a differing column doesn't silently become NA.
col_sets <- map(inv_list, names)
common   <- reduce(col_sets, intersect)

walk2(names(col_sets), col_sets, function(nm, cols) {
  extra   <- setdiff(cols, common)
  missing <- setdiff(common, cols)
  if (length(extra) || length(missing)) {
    message("\n[", nm, "] column mismatch vs common set:")
    if (length(extra))   message("  extra:   ", paste(extra, collapse = ", "))
    if (length(missing)) message("  missing: ", paste(missing, collapse = ", "))
  }
})

## ------------------------------------------ ##
#            Combine -----
## ------------------------------------------ ##
# bind_rows (not rbind): fills NA for any non-shared column rather than erroring.
inv_all <- bind_rows(inv_list, .id = "source_file")

inv_all <- inv_all %>%
  rename(
    mesh_335_noaa        = x335_mm_noaa,
    mesh_335_tar_dna     = x335_mm_tar_dna,
    mesh_150_morphid     = x150_mm_llopiz_morph_id,
    mesh_150_tar_dna     = x150_mm_tar_dna,
    mesh_150_taxa_pick   = x150_mm_taxa_picking,
    mesh_150_size_fract  = x150_mm_for_size_fractions,
    mesh_20_size_fract   = x20_mm_size_fractions
  )

glimpse(inv_all)
lapply(inv_all, unique)

## ------------------------------------------ ##
#      Clean cast: drop X rows, strip B/R prefix + decimal -----
## ------------------------------------------ ##
# cast is mixed: "1.0", "B1", "X", "1"
# Normalize to a bare number as character ("B19" -> "19", "4.0" -> "4")
# Drop cast == "X" == no valid tow
inv_all <- inv_all %>%
  filter(cast != "X") %>%
  mutate(cast = cast %>%
           str_remove("^[BR]") %>%    # "B19" -> "19"
           str_remove("\\.0+$"))      # "4.0" -> "4"

## ------------------------------------------ ##
#      Hand fix -----
## ------------------------------------------ ##
# AR32: 20um size-fraction samples were not taken -> force all to 0
# AT46: no 100-200um size fraction this cruise, so 20um "2" -> "1" (leave 0 as is)
inv_all <- inv_all %>%
  mutate(mesh_20_size_fract = case_when(
    cruise == "AR32"                          ~ "0",
    cruise == "AT46" & mesh_20_size_fract == "2" ~ "1",
    TRUE ~ mesh_20_size_fract
  ))

## EN627 L1 B3: bottom-hit bongo tow; only the 20um size-fraction sample was
## kept (station later resampled; B44)
## Was included in the published v2 inventory (knb-lter-nes.24.2) but not here
## nor tow_meta 
inv_all <- inv_all %>%
  add_row(
    source_file         = "EN627",   
    cruise              = "EN627",
    station             = "L1",
    cast                = "3",
    mesh_335_noaa       = "0",
    mesh_335_tar_dna    = "0",
    mesh_150_morphid    = "0",
    mesh_150_tar_dna    = "0",
    mesh_150_taxa_pick  = "0",
    mesh_150_size_fract = "0",
    mesh_20_size_fract  = "2",
    comments            = "Hit bottom bongo; only 20um size-fraction sample kept; Station resampled."
  )

# EN657 L1 B1: both 335 samples (NOAA + DNA) were taken; correct to 1. nonquant
inv_all <- inv_all %>%
  mutate(
    mesh_335_noaa    = if_else(cruise == "EN657" & station == "L1" & cast == "1",
                               "1", mesh_335_noaa),
    mesh_335_tar_dna = if_else(cruise == "EN657" & station == "L1" & cast == "1",
                               "1", mesh_335_tar_dna)
  )

## ------------------------------------------ ##
#            Write -----
## ------------------------------------------ ##
stamp <- format(Sys.Date(), "%Y%m%d")
write_csv(inv_all, here("data", "processed",
                        glue::glue("sample_inventory_combined-{stamp}.csv")))
