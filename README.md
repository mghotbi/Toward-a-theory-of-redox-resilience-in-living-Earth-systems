
# Toward a theory of redox resilience in living Earth systems

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

This repository contains the reproducible figure workflow, processed datasets, and publication-ready outputs associated with the manuscript:

**Toward a theory of redox resilience in living Earth systems**

## Reproducing the figure

Run in R:

```r
source("figure_redox_resilience.r")

```
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
 
# Citation

Please cite the associated manuscript when using this repository:

> Ghotbi, M. et al. *Toward a theory of redox resilience in living Earth systems.*

 
