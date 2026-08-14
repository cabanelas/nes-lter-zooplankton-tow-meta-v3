################################################################################
## Script:  sample_inventory_combine.R
## Project: NES-LTER Zooplankton Inventory Data Package v3
## Author:  Alexandra C. Cabanelas
##
## Purpose: Read the per-cruise sample-inventory workbooks, clean names, stack
##          them into one long table, and write a combined CSV. Downstream this
##          feeds the size_fract_20 column in the tow-metadata objects.
##
## Inputs  (data/raw/sample_inventory/):
##   - <CRUISE>_SampleInventory.xlsx   (one per cruise)
## Output  (data/processed/):
##   - sample_inventory_combined-YYYYMMDD.csv
################################################################################

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
                        full.names = TRUE)
message("Found ", length(inv_files), " inventory files:\n  ",
        paste(basename(inv_files), collapse = "\n  "))

## read each, clean names, keep the source file for traceability
inv_list <- inv_files %>%
  set_names(str_remove(basename(.), "_SampleInventory\\.xlsx$")) %>%
  map(~ read_excel(.x) %>%
        clean_names() %>%
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

## ------------------------------------------ ##
#            Write -----
## ------------------------------------------ ##
stamp <- format(Sys.Date(), "%Y%m%d")
write_csv(inv_all, here("data", "processed",
                        glue::glue("sample_inventory_combined-{stamp}.csv")))
