# Solís Arce et al. (2021) — COVID-19 vaccine acceptance and hesitancy in LMICs

Folder-backed replication materials for [10.1038/s41591-021-01454-y](https://doi.org/10.1038/s41591-021-01454-y).

## Displayed replications

| id | Paper label |
|----|-------------|
| `tab_2` | Table 2 — Vaccination beliefs and coverage |
| `fig_1` | Fig. 1 — Acceptance rates by characteristics |
| `fig_2` | Fig. 2 — Reasons not to take the vaccine |
| `fig_3` | Fig. 3 — Most trusted sources |
| `ext_fig_1` | Extended Data Fig. 1 — Trusted sources by gender |
| `ext_fig_2` | Extended Data Fig. 2 — Leave-one/two-out acceptance |

## Data and DAG

Microdata root is `data/combined.csv`. Shared prep writes intermediates under `outputs/`:

- `prep_micro` → `outputs/df.rds`, `outputs/df2.rds`
- `prep_trust` → `outputs/df_trust.rds` (Fig. 3 / Ext. Fig. 1)

Table 2 uses small published aggregates `data/table_wgm.csv` and `data/vacc_cov.csv` (WGM / WHO), not respondent microdata.

## Layout

```
replication.yml
data/                 # combined.csv + table_wgm.csv + vacc_cov.csv
code/                 # prep_*, make_*/format_* per target
outputs/              # intermediates + display files
tests/
```

The [registry](https://github.com/replicate-anything/registry) holds a lightweight stub at `studies/10.1038_s41591-021-01454-y.yml` that points here.

## Build display artifacts

```r
library(replicateEverything)
options(
  replicateEverything.registry_root = "../registry",
  replicateEverything.use_sibling_packages = TRUE
)
replicateEverything::build_study_outputs(".", install_deps = TRUE)
```

## Sync to registry

```r
replicateEverything::prepare_study_for_registry(".", registry_root = "../registry")
replicateEverything::sync_study_to_registry(".", registry_root = "../registry")
```

## Local development (monorepo)

```r
options(
  replicateEverything.registry_root = "../registry",
  replicateEverything.use_sibling_packages = TRUE
)
replicateEverything::run_replication("10.1038/s41591-021-01454-y", "fig_1", given = "nothing", format = TRUE)
```
