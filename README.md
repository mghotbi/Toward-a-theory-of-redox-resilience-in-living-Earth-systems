
# Toward a theory of redox resilience in living Earth systems

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

This repository contains the reproducible figure workflow, processed datasets, and publication-ready outputs associated with the manuscript:

**Toward a theory of redox resilience in living Earth systems**

## Reproducing the figure

Run in R:

```r
source("figure_redox_resilience.r")


# Toward a theory of redox resilience in living Earth systems

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-reproducible%20workflow-276DC3.svg)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-reproducibility-39729E.svg)](https://quarto.org/)

This repository contains the reproducible figure workflow, processed datasets, and publication-ready outputs associated with the manuscript:

> **Toward a theory of redox resilience in living Earth systems**

The repository integrates biological, hydrological, microbial, root, freeze–thaw, oxygen-memory, and abiotic rewetting datasets to support a mechanistic redox-resilience framework for living Earth systems.

---

# Repository contents

| Folder / file | Description |
|---|---|
| `figure_redox_resilience.r` | Complete R workflow used to generate the publication figure. |
| `redox_resilience_figure.qmd` | Quarto reproducibility workflow. |
| `redox_resilience_figure.html` | Rendered reproducibility page. |
| `figures/` | Publication-ready PDF, TIFF, and PNG outputs. |
| `processed_data/` | Processed CSV and RDS datasets used in each panel. |
| `session_info.txt` | R session information for computational reproducibility. |

---

# Reproducing the figure

Run in R:

```r
source("figure_redox_resilience.r")
```

Or render the Quarto document:

```bash
quarto render redox_resilience_figure.qmd
```

---

# Processed datasets

Processed datasets are reference-prefixed for transparent provenance tracking:

- `lacroix_2022_*`
- `delwiche_2021_fluxnet_ch4_*`
- `kim_2012_rtsg_*`
- `angle_2017_*`
- `huo_2017_*`
- `liebmann_freeze_thaw_*`
- `sennett_2024_*`
- `liu_2025_*`

Each dataset is exported as both:

- `.csv`
- `.rds`

for maximum reproducibility and reuse.

---

# Figure outputs

The main publication figure is exported to:

```text
figures/
├── fig_redox_resilience_publish_nature_ready.pdf
├── fig_redox_resilience_publish_nature_ready.tiff
└── fig_redox_resilience_publish_nature_ready.png
```

---

# Reproducibility

The repository includes:

- fully reproducible R workflows
- Quarto-based figure rendering
- processed datasets
- publication-ready exports
- computational session information

Environment metadata is recorded in:

```text
session_info.txt
```

---

# Citation

Please cite the associated manuscript when using this repository:

> Ghotbi, M. *Toward a theory of redox resilience in living Earth systems.*

---

# License

MIT License.
