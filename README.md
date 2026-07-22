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

## Layout

```
replication.yml
data/
code/
outputs/           # precomputed display files
tests/testthat/
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
replicateEverything::run_replication("10.1038/s41591-021-01454-y", "fig_1")
```
