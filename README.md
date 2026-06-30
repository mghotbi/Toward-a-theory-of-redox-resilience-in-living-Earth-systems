# Toward a theory of redox resilience in living Earth systems

Code, processed datasets and figure-generation scripts supporting:

> Ghotbi, M., Kolody, B. C., Ghotbi, M. & Holtgrewe-Stukenbrock, E.
> *Hydroclimatic redox resilience is the capacity of living Earth systems to
> recover distributed electron-transfer architecture following perturbation.*

This repository accompanies Figure 5 (main text) and Supplementary Figure 1
(extended data), reproducing every panel from raw or lightly processed
public datasets.

## Repository structure

```
.
├── Redox_resilience_figure_script.R   # full panel + figure assembly script
├── data/                              # raw input files (see Table 2 below)
├── github_ready_figure_exports/
│   ├── processed_data/                # one CSV + RDS per panel, see Table 3
│   └── figures/                       # PDF / TIFF (1200 dpi) / PNG exports
└── README.md
```

Running `Redox_resilience_figure_script.R` end to end regenerates every
processed dataset and both figures (`fig_MAIN_10panels_...` /
`fig_EXTENDED_DATA_...`) from the raw files in `data/`. Edit the `data_dir`
path at the top of the script to point at a local copy of the `data/`
folder before running.

## Table 1 | Datasets used in this review

This mirrors Table S1 of the manuscript, with the original sources spelled
out.

| Property | Source | Ecosystem | Variables used in this review | Mechanistic insight | Figure panel |
|---|---|---|---|---|---|
| Capacity | Lacroix et al. 2025, *Soil Biol. Biochem.* 211, doi:10.1016/j.soilbio.2025.109974 | California grassland | Anoxic microsites, SOC, anaerobic genes | Soil carbon and structure determine accessible electron-buffering capacity | Fig. 5A |
| Capacity | Li et al. 2025, *Nat. Commun.* 16, doi:10.1038/s41467-025-59637-x | Rice rhizosphere | Fe plaque, O2, P, redox dynamics | Rhythmic root oxygen loss creates reversible Fe electron buffers | Fig. 5B, K |
| Capacity | Patzner et al. 2020, *Nat. Commun.* 11, doi:10.1038/s41467-020-20102-6 | Permafrost thaw gradient | Reactive Fe, Fe-bound OC, Fe reducers | Thaw redistributes mineral electron reservoirs and associated carbon | Fig. 5C, L |
| Connectivity | Delwiche et al. 2021, *Earth Syst. Sci. Data* 13, doi:10.5194/essd-13-3607-2021 (FLUXNET-CH4) | Global wetlands | Water table, CH4 flux | Hydrological reconnection governs ecosystem-scale methane transport | Fig. 5D |
| Connectivity | Angle et al. 2017, *Nat. Commun.* 8, doi:10.1038/s41467-017-01753-4 | Freshwater wetland | Methane production under oxic conditions | Spatial separation permits methanogenesis despite bulk oxygenation | Fig. 5E |
| Kinetics | Kim et al. 2012, *Biogeosciences* 9, doi:10.5194/bg-9-2459-2012 | Global synthesis | CO2, CH4, N2O responses after rewetting and thaw | Electron-accepting pathways recover on different timescales | Fig. 5F |
| Kinetics | Lacroix et al. 2025, *Soil Biol. Biochem.* 211, doi:10.1016/j.soilbio.2025.109974 | Rhizosphere soils | Root exudation, oxygen depletion, anoxic microsites | Rhizosphere activation rapidly changes reaction rates and redox conditions | Fig. 5G |
| Memory | Sennett et al. 2024, *Nat. Commun.* 15, doi:10.1038/s41467-024-51688-w | Agricultural soil | N2 production trajectories | Previous oxygen exposure leaves persistent functional legacy | Fig. 5H |
| Memory | Sennett et al. 2024, *Nat. Commun.* 15, doi:10.1038/s41467-024-51688-w | Agricultural soil | Metatranscriptomes, denitrification modules | Oxygen history restructures denitrifier organization | Fig. 5I |
| Recovery trajectory | Liebmann et al. 2025, *Commun. Earth Environ.* 7, 120, doi:10.1038/s43247-025-03143-x | Alaskan permafrost | Continuous Eh monitoring | Freeze-thaw transitions produce path-dependent seasonal redox trajectories | Fig. 5J |
| Recovery trajectory | Li et al. 2025, *Nat. Commun.* 16, doi:10.1038/s41467-025-59637-x | Rice rhizosphere | O2, Fe plaque, phosphorus mobilization | Rhythmic oxygen release generates asynchronous physicochemical recovery | Fig. 5K |
| Recovery trajectory | Conceptual synthesis derived from Patzner et al. 2020, *Nat. Commun.* 11, doi:10.1038/s41467-020-20102-6 | Permafrost thaw | Fe-OC interactions | Recovery involves reconstruction of electron-routing architecture rather than restoration of a previous state | Fig. 5L |

Lacroix et al. 2025 and Li et al. 2025 each appear twice because the same
underlying dataset supports two distinct properties: Lacroix supports
Capacity (Panel A) and Kinetics (Panel G); Li supports Capacity (Panel B)
and Recovery trajectory (Panel K). Sennett et al. 2024 likewise supports
both Memory panels (H, I) from a single study.

## Table 2 | Raw input files on disk

| File | Type | Approx. size | Maps to | Conditions / notes |
|---|---|---|---|---|
| `fig4_panel_a_lacroix_capacity_axis.csv` | CSV, tabular | <1 MB | Lacroix et al. 2025 (Panel A) | California grassland; anoxic-microsite, SOC and anaerobic-gene proxies underlying the capacity axis |
| `lacroix_2025_rhizosphere_kinetics.csv` | CSV, tabular | <1 MB | Lacroix et al. 2025 (Panel G) | Root exudation, oxygen depletion and anoxic-microsite formation in rhizosphere soils |
| `global_rtsg_flux_v1.csv` | CSV, tabular | ~1-5 MB | Kim et al. 2012 (Panel F) | Global rewetting/thawing soil gas flux synthesis; CO2, CH4, N2O response ratios |
| `fluxnet_ch4_water_table.csv` / `FLX_US-MAC_FLUXNET-CH4_DD_2013-2015_1-1.csv` | CSV, daily time series | ~5-10 MB | Delwiche et al. 2021 / FLUXNET-CH4 (Panel D) | Daily wetland CH4 flux and water-table depth; public FLUXNET-CH4 release, US-MAC site shown as fallback |
| `angle_2017_methanogenesis.csv` | CSV, tabular | <1 MB | Angle et al. 2017 (Panel E) | Methanogenic activity under oxygenated wetland soil conditions |
| `luh_ifbk.ID_6637_FTC_DATASET.csv` | CSV, electrode time series | ~1-5 MB | Liebmann et al. 2025 (Panel J) | Continuous redox-electrode (Eh) measurements through freeze-thaw cycles, Alaskan permafrost soils |
| `p9.csv` | CSV, raw plate/replicate data | <1 MB | Sennett et al. 2024 (Panel H) | N2 production trajectory raw replicates by oxygen pre-treatment |
| `geneden.xlsx` / `geneden.xls` / `geneden.csv` | Spreadsheet, gene-count table | <1 MB | Sennett et al. 2024 (Panel I) | Denitrification gene/metatranscriptome pathway counts by treatment and time |
| `Li 2025 Ncom.xlsx` / `li 2025 rythmic.xlsx` | Spreadsheet, multi-sheet workbook | 1-5 MB | Li et al. 2025 (Panels B, K) | Rice rhizosphere Fe plaque, O2, P and redox-dynamics time series |
| `Main text_1) Porewater analysis.xlsx` | Spreadsheet | <1 MB | Patzner et al. 2020 (Panel C) | Permafrost porewater Fe2+ concentrations across thaw-gradient stages (Palsa to Bog to Fen) |
| `SI_6) Stock of reactive Fe and associated OC.xlsx` | Spreadsheet | <1 MB | Patzner et al. 2020 (Panel L) | Reactive Fe and Fe-associated organic carbon stocks by soil horizon, basis for the conceptual recovery-trajectory synthesis |

All raw files are third-party research data, redistributed here only as the
specific extracts used to generate figure panels, under the terms of the
original publishers' data-sharing licenses. Please cite the original papers
in Table 1 (not this repository) when reusing the underlying measurements.

## Table 3 | Processed datasets generated by the script

Every panel writes a matched `<source>_<panel>.csv` and `<source>_<panel>.rds`
pair to `github_ready_figure_exports/processed_data/`:

| Output file | Panel | Content |
|---|---|---|
| `lacroix_2025_panel_a_capacity_axis.csv` | A | Long-format capacity-axis covariates, California grassland |
| `li_2025_panel_b_fe_plaque.csv` | B | Fe plaque / O2 / P / redox-dynamics by compartment |
| `patzner_2020_panel_c_porewater_fe2.csv` | C | Porewater Fe2+ by permafrost thaw stage |
| `delwiche_2021_panel_d_fluxnet_ch4.csv` | D | Cleaned daily CH4 flux / water-table series |
| `angle_2017_panel_e_methanogenesis.csv` | E | Methanogenic activity under oxic conditions |
| `kim_2012_panel_f_gas_kinetics.csv` | F | CO2/CH4/N2O response ratios after rewetting and thaw |
| `lacroix_2025_panel_g_rhizosphere_kinetics.csv` | G | Root exudation, O2 depletion, anoxic-microsite kinetics |
| `sennett_2024_panel_h_n2_trajectories.csv` | H | N2 production trajectories by oxygen pre-treatment |
| `sennett_2024_panel_i_denitrification_modules.csv` | I | Metatranscriptome / denitrification-module restructuring |
| `liebmann_2025_panel_j_redox_shift.csv` | J | Continuous Eh monitoring through freeze-thaw cycles |
| `li_2025_panel_k_recovery_trajectory.csv` | K | O2, Fe plaque, phosphorus-mobilization recovery series |
| `patzner_2020_panel_l_fe_oc_synthesis.csv` | L | Fe-OC interaction synthesis underlying the recovery-trajectory panel |

## Figure outputs

`github_ready_figure_exports/figures/` contains, for both the main figure
and the extended-data figure:

- `*.pdf` - vector, Cairo PDF device (publication-ready)
- `*.tiff` - 1200 dpi, LZW compression (journal submission)
- `*.png` - 300 dpi (preview / repository rendering)

If a PDF does not render in your browser or in GitHub's file preview, use
the PNG for a quick look, or download and open the PDF locally - large
multi-panel Cairo PDFs sometimes exceed GitHub's in-browser PDF viewer size
or rendering limits.

## Citation

If you use this code or these processed datasets, please cite the
manuscript above and the original data sources listed in Table 1.

## Contact

Mitra Ghotbi - mitra.ghotbi@gmail.com - ORCID: 0000-0001-9185-9993
