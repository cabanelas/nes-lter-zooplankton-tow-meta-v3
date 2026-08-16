# NES-LTER Zooplankton Tow Metadata Data Package Scripts (v3)

This repository contains the R scripts that build the **NES-LTER zooplankton tow metadata** data package (v3) submitted to the Environmental Data Initiative (EDI). The package provides tow-level metadata for zooplankton samples collected with Bongo and ring nets along the Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER) Transect, ongoing since 2018.

Version 3 extends the published v2 package (which ended at cruise EN720) by adding the 2024–2026 cruises **AE2426, EN727, AR88, AR92, AR95, AR99, and HRS2601**, and adds the `depth_PX` sensor field (Kongsberg Simrad PX, starting AE2426) and separate ring-net tow logging (starting AR99).

## Repository structure

```
nes-lter-tow-meta-v3.Rproj
01_ship_speed_pull.R
02_ship_speed_eventlog_merge.R
03_bongo_logs_merge.R
04_tow_metadata_assemble.R
05_volume_review.R
06_tow_metadata_qaqc.R
sample_inventory_combine.R
data/
  raw/         # input logsheets, event log, TDR/PX, ship speed, winch, inventory workbooks
  processed/   # intermediate and published-ready CSVs written by the scripts
```

## Script workflow

Run the numbered scripts in order. `sample_inventory_combine.R` is a helper that can be run any time before `04`.

1. **`01_ship_speed_pull.R`**
   Pulls underway ship speed (STW / SOG) for the new cruises from the NES-LTER REST API and writes a raw ship-speed CSV. (v2 rows already carry ship speed from the published package.)

2. **`02_ship_speed_eventlog_merge.R`**
   Merges the underway ship speed onto the bongo/ring-net event log, producing `shipspeed_eventlog_v3.csv`.

3. **`03_bongo_logs_merge.R`**
   Assembles the combined tow table: stacks the published v2 rows, the new-cruise bongo logsheets, and the AR99 ring-net rows from the event log, aligns the schema, builds datetimes, applies manual logsheet fixes, and assigns `net_type`.

4. **`04_tow_metadata_assemble.R`**
   Fills the columns that came in as NA for the new cruises and produces the final combined table (2018–2026). This is where coordinates, depths (`depth_TDR`, `depth_PX`, cosine-law fallback, `net_max_depth`), ship speeds, volume filtered, haul factors, `size_fract_20`, and QARTOD data flags are derived. v2 rows are preserved by default; deliberate corrections are made only where documented. Writes the published-ready CSV `nes-lter-zooplankton-tow-metadata-v3-{date}.csv`.

5. **`05_volume_review.R`**
   Visual review of the volume-filtered values (elapsed time, 335 vs 150 µm volume, flowmeter vs speed×time comparison). Reads the `04` output; does not recompute or write anything. Use it to eyeball which volumes look off.

6. **`06_tow_metadata_qaqc.R`**
   Column-by-column structural QA/QC of the published-ready CSV: checks column names and types against an expected schema, required-field completeness, key uniqueness, controlled vocabularies (net type, Y/N fields, QARTOD flags), numeric ranges, and cross-field logic (counts↔volume, ring-net NAs, flag↔note consistency). Exports a data-dictionary CSV.

**Helper — `sample_inventory_combine.R`**
Reads the per-cruise sample-inventory workbooks (`data/raw/sample_inventory/`, including the `v2/` subfolder), cleans and stacks them into one long table, applies hand fixes, and writes `sample_inventory_combined-{date}.csv`. This feeds the `size_fract_20` column in `04`. Run any time before `04`.

## Requirements

```r
install.packages(c("here", "tidyverse", "readr", "dplyr", "tidyr", "purrr",
                   "lubridate", "readxl", "janitor", "glue",
                   "ggthemes", "cowplot", "RColorBrewer", "ggpubr",
                   "sf", "maps", "plotly", "listviewer"))
```

Core pipeline (`01`–`04`, `sample_inventory_combine`) uses: here, tidyverse, readr, dplyr, tidyr, purrr, lubridate, readxl, janitor, glue.
Review and QA (`05`, `06`) additionally use: ggthemes, cowplot (plotting); sf, maps, plotly (coordinate maps).

## Data inputs

Placed under `data/raw/`:

- `bongo_logs/` — new-cruise bongo net event log datasheets
- `sample_inventory/` — per-cruise sample-inventory workbooks (with `v2/` subfolder)
- `elog_zoop_tows_thruHRS2601_2026-08-10.csv` — event log (from the companion [nes-lter-api-pulls](https://github.com/cabanelas/nes-lter-api-pulls) project)
- `nes-lter-bongo-tdr-offsets.csv`, `max_depth_tdr_corrected.csv` — TDR/PX depths and offsets (from the companion `nes-lter-tdr-bongo` project)
- `winch/` — winch data (`max_wire_out`, `avg_angle`) used for the cosine-law depth fallback
- `speed_log/`, `shipspeed_eventlog_v2.csv` — ship speed inputs
- `nes-lter-zooplankton-tow-metadata-v2.csv` — published v2 rows carried forward

## Data package overview

**Title.** Zooplankton Tow Metadata for Northeast U.S. Shelf Long Term Ecological Research (NES-LTER) Transect Cruises, Ongoing Since 2018

**Version.** 3

**Summary.** Tow-level metadata for physical zooplankton samples collected with Bongo (150 µm and 335 µm mesh) and ring nets (20 µm and 150 µm mesh) along the NES-LTER Transect, located south of Martha's Vineyard, Massachusetts, at standard stations L1–L11 plus MVCO. Each record documents tow position and timing, net and instrument details, depths (target, bottom, TDR, PX, and derived net-maximum depth), wire and ship-speed data, flowmeter readings and volume filtered, haul factors, sample-type indicators, and QARTOD quality flags. Sample purposes include morphological identification, DNA metabarcoding, stable isotope analysis, and size fractionation. Some early cruises were conducted in partnership with the Ocean Observatories Initiative.

**Data flags.** Quality flags follow the QARTOD / IOC 54:V3 primary-level scheme: 1 = Good, 3 = Suspect/of high interest, 4 = Bad (failed critical), 9 = Missing. `primary_flag` is the worst case across all conditions on a row; `secondary_flag` gives the human-readable reason.

## Citation

Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER). (202X). Zooplankton tow metadata for Northeast U.S. Shelf Long Term Ecological Research (NES-LTER) Transect cruises, ongoing since 2018. v3. Environmental Data Initiative.

## Author

Alexandra C. Cabanelas
