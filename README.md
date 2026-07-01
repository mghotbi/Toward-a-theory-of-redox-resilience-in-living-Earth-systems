<div align="center">

# A Theory of Hydroclimatic Redox Resilience

**Code, datasets, and figure-generation scripts for a redox-resilience framework in living Earth systems**

Ghotbi, M. · Kolody, B. C. · Ghotbi, M. · Holtgrewe-Stukenbrock, E.

[![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![License: data](https://img.shields.io/badge/data-third--party%2C%20cited-lightgrey)](#license--reuse)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0001--9185--9993-A6CE39?logo=orcid&logoColor=white)](https://orcid.org/0000-0001-9185-9993)

</div>
---

This repository reproduces **Figure 5** (main text) from raw third-party datasets, through processed intermediates, to figure files.

> *Hydroclimatic redox resilience is the capacity of living Earth systems to recover distributed electron-transfer architecture following perturbation.*

<br>

## Contents

- [Quick start](#quick-start)
- [Repository structure](#repository-structure)
- [The six properties](#the-six-properties)
- [Datasets](#datasets)
- [Figure outputs](#figure-outputs)
- [Citation](#citation)

<br>

## Quick start

```bash
git clone https://github.com/mghotbi/Toward-a-theory-of-redox-resilience-in-living-Earth-systems
cd Toward-a-theory-of-redox-resilience-in-living-Earth-systems
```

Open `Redox_resilience_figure_script.R`, point `data_dir` at your local copy of `data/`, and run it top to bottom. It regenerates every processed dataset and both figures (`fig_MAIN_10panels_...`, `fig_EXTENDED_DATA_...`).

<br>

## Repository structure

```
.
├── Redox_resilience_figure_script.R     full panel + figure assembly
├── data/                                raw input files
├── github_ready_figure_exports/
│   ├── processed_data/                  one CSV + RDS per panel
│   └── figures/                         PDF · TIFF (1200 dpi) · PNG
└── README.md
```
<br>

## The six properties

Each panel in Figure 5 provides empirical evidence for one of six properties of redox resilience:

| | Property | Panels |
|:---:|:---|:---|
| 🟧 | **Capacity** | A · B · C |
| 🟦 | **Connectivity** | D · E |
| 🟨 | **Kinetics** | F · G |
| 🟪 | **Memory** | H · I |
| 🟩 | **Recovery trajectory** | J · K · L |

<br>

## Datasets

<details open>
<summary><b>Table 1 — Sources used in Fig 5 of this review</b> </summary>
<br>

| Panel | Property | Source | Ecosystem | Insight |
|:---:|:---|:---|:---|:---|
| A | Capacity | Lacroix et al. 2025 | California grassland | Soil carbon & structure set accessible electron-buffering capacity |
| B | Capacity | Li et al. 2025 | Rice rhizosphere | Rhythmic root O₂ loss creates reversible Fe electron buffers |
| C | Capacity | Patzner et al. 2020 | Permafrost thaw gradient | Thaw redistributes mineral electron reservoirs and carbon |
| D | Connectivity | Delwiche et al. 2021 (FLUXNET-CH4) | Global wetlands | Hydrological reconnection governs methane transport |
| E | Connectivity | Angle et al. 2017 | Freshwater wetland | Spatial separation permits methanogenesis under bulk oxygenation |
| F | Kinetics | Kim et al. 2012 | Global synthesis | Electron-accepting pathways recover on different timescales |
| G | Kinetics | Lacroix et al. 2025 | Rhizosphere soils | Rhizosphere activation rapidly shifts reaction rates & redox state |
| H | Memory | Sennett et al. 2024 | Agricultural soil | Prior O₂ exposure leaves a persistent functional legacy |
| I | Memory | Sennett et al. 2024 | Agricultural soil | O₂ history restructures denitrifier organization |
| J | Recovery trajectory | Liebmann et al. 2025 | Alaskan permafrost | Freeze–thaw transitions produce path-dependent trajectories |
| K | Recovery trajectory | Li et al. 2025 | Rice rhizosphere | Rhythmic O₂ release generates asynchronous physicochemical recovery |
| L | Recovery trajectory | Patzner et al. 2020 (synthesis) | Permafrost thaw | Recovery reconstructs electron-routing architecture, not a prior state |

**References**

<sup>
Lacroix, E. M. et al. Root exudation and fine texture interact to form anoxic microsites in rhizosphere soil. <i>Soil Biol. Biochem.</i> <b>211</b> (2025). doi:10.1016/j.soilbio.2025.109974<br>
Li, C. et al. Rhythmic radial oxygen loss enhances soil phosphorus bioavailability. <i>Nat. Commun.</i> <b>16</b> (2025). doi:10.1038/s41467-025-59637-x<br>
Patzner, M. S. et al. Iron mineral dissolution releases iron and associated organic carbon during permafrost thaw. <i>Nat. Commun.</i> <b>11</b> (2020). doi:10.1038/s41467-020-20102-6<br>
Delwiche, K. B. et al. FLUXNET-CH4: a global, multi-ecosystem dataset and analysis of methane seasonality from freshwater wetlands. <i>Earth Syst. Sci. Data</i> <b>13</b>, 3607–3689 (2021). doi:10.5194/essd-13-3607-2021<br>
Angle, J. C. et al. Methanogenesis in oxygenated soils is a substantial fraction of wetland methane emissions. <i>Nat. Commun.</i> <b>8</b> (2017). doi:10.1038/s41467-017-01753-4<br>
Kim, D. G., Vargas, R., Bond-Lamberty, B. & Turetsky, M. R. Effects of soil rewetting and thawing on soil gas fluxes. <i>Biogeosciences</i> <b>9</b>, 2459–2483 (2012). doi:10.5194/bg-9-2459-2012<br>
Sennett, L. B. et al. Determining how oxygen legacy affects trajectories of soil denitrifier community dynamics and N2O emissions. <i>Nat. Commun.</i> <b>15</b> (2024). doi:10.1038/s41467-024-51688-w<br>
Liebmann, P. et al. Perennial redox potential dynamics in Alaskan degraded and non-degraded permafrost soils. <i>Commun. Earth Environ.</i> <b>7</b>, 120 (2025). doi:10.1038/s43247-025-03143-x
</sup>



</details>

<details>
<summary><b>Table 2 — Raw files on disk</b></summary>
<br>

| File | Panel | Size | Notes |
|:---|:---:|:---:|:---|
| `fig4_panel_a_lacroix_capacity_axis.csv` | A | <1 MB | Anoxic-microsite, SOC, anaerobic-gene proxies |
| `lacroix_2025_rhizosphere_kinetics.csv` | G | <1 MB | Root exudation, O₂ depletion, anoxic-microsite formation |
| `global_rtsg_flux_v1.csv` | F | 1–5 MB | Global rewetting/thawing CO₂, CH₄, N₂O flux synthesis |
| `fluxnet_ch4_water_table.csv` | D | 5–10 MB | Daily wetland CH₄ flux & water-table depth (FLUXNET-CH4) |
| `angle_2017_methanogenesis.csv` | E | <1 MB | Methanogenic activity, oxygenated wetland soils |
| `luh.ID_6637_FTC_DATASET.csv` | J | 1–5 MB | Continuous Eh electrode record, freeze–thaw cycles |
| `p9.csv` | H | <1 MB | N₂ trajectory raw replicates by O₂ pre-treatment |
| `geneden.xlsx` | I | <1 MB | Denitrification gene / metatranscriptome counts |
| `Li 2025 Ncom.xlsx` | B, K | 1–5 MB | Fe plaque, O₂, P, redox-dynamics time series |
| `Main text_1) Porewater analysis.xlsx` | C | <1 MB | Porewater Fe²⁺ across thaw stages (Palsa → Bog → Fen) |
| `SI_6) Stock of reactive Fe and associated OC.xlsx` | L | <1 MB | Reactive Fe / Fe-OC stocks by soil horizon |

All raw files are third-party research data, included here only as the specific extracts used for figure panels. Cite the original papers above — not this repository — when reusing the underlying measurements.

</details>

<details>
<summary><b>Table 3 — Processed outputs</b></summary>
<br>

A matched ` .csv` + `.rds` pair to `figure_exports/processed_data/`, e.g. `lacroix_2025_panel_a_capacity_axis.csv`, `li_2025_panel_k_recovery_trajectory.csv`.

</details>

<br>

## Figure outputs

`figures/` contains, the main figures:

| Format | Use |
|:---|:---|
| `.pdf` | Vector, Cairo PDF device <br><br><img src="https://github.com/user-attachments/assets/e61d6dbb-b630-427f-ba38-3f6a6cadc108" width="250"> |
<br>

## Citation

If you use this code or these processed datasets, please cite the manuscript above and the original data sources in Table 1.

## License & reuse

Code in this repository is shared for reproducibility. Raw datasets remain the property of their original authors/publishers and are subject to their respective licenses — see Table 1 for citations and DOIs.

<br>

---

<div align="center">

**Mitra Ghotbi** · mitra.ghotbi@gmail.com · [ORCID 0000-0001-9185-9993](https://orcid.org/0000-0001-9185-9993)

</div>
