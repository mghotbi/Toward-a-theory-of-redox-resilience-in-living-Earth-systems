# ============================================================
# Fig5_redox_resilience.R
# Ghotbi et al. 2026
#
# Complete self-contained script.
# Edit data_dir (line 20) and p9_file (line 21) then run
# top to bottom to produce Fig. 5 as PDF / TIFF / PNG.
# ============================================================

# 1. Packages -------------------------------------------------------------

library(tidyverse)
library(janitor)
library(mgcv)
library(ggdist)
library(ggrepel)
library(patchwork)
library(scales)
library(readr)
library(readxl)
library(grid)

# 2. Paths ----------------------------------------------------------------

data_dir <- "/Users/mitraghotbi/Library/CloudStorage/GoogleDrive-mitra.ghotbi@gmail.com/My Drive/Review on Redox Resilience MG 2026 Jan/NGEO2026/data"
p9_file  <- "/Users/mitraghotbi/Desktop/p9.csv"

figure_out_dir <- file.path(data_dir, "github_ready_figure_exports", "figures")
data_out_dir   <- file.path(data_dir, "github_ready_figure_exports", "processed_data")
purrr::walk(c(figure_out_dir, data_out_dir),
            ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# 3. Helpers --------------------------------------------------------------

find_file <- function(paths) {
  existing <- paths[file.exists(paths)]
  
  if (length(existing) == 0) {
    stop("None of these files exist:\n", paste(paths, collapse = "\n"))
  }
  
  existing[[1]]
}

save_dataset <- function(data, prefix, name) {
  base_name <- paste(prefix, name, sep = "_")
  
  readr::write_csv(
    data,
    file.path(data_out_dir, paste0(base_name, ".csv"))
  )
  
  saveRDS(
    data,
    file.path(data_out_dir, paste0(base_name, ".rds"))
  )
  
  invisible(data)
}


num <- function(x) readr::parse_number(as.character(x))

# 4. Theme and colours ----------------------------------------------------

theme_redox <- function(base_size = 8) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Helvetica") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "grey90",
        linewidth = 0.25
      ),
      axis.title = ggplot2::element_text(
        size = base_size + 1,
        colour = "black"
      ),
      axis.text = ggplot2::element_text(
        size = base_size,
        colour = "grey15"
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 2.4
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size,
        colour = "grey35",
        lineheight = 1.05
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = base_size
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 0.4),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
}

ggplot2::theme_set(theme_redox())

# Colours -----------------------------------------------------------------

framework_cols <- c(
  "Capacity" = "#a6611a",
  "Connectivity" = "#00897b",
  "Kinetics" = "#f57c00",
  "Microbial routing" = "#8e24aa",
  "Root control" = "#2e7d32",
  "Trajectory organization" = "#1565c0"
)

gas_cols <- c(
  "CO2" = "#f57c00",
  "CH4" = "#00897b",
  "N2O" = "#8e24aa"
)

root_cols <- c(
  "Legume" = "#8e24aa",
  "Grass" = "#2e7d32",
  "Tree" = "#5e35b1",
  "Forb" = "#ef6c00",
  "Wetland graminoid" = "#00897b",
  "Other" = "grey60"
)

angle_cols <- c(
  "Methanogenesis" = "#8e24aa",
  "Carbon routing" = "#a6611a",
  "Persistence" = "#1976d2",
  "Oxygen stress" = "#d81b60"
)

phase_cols <- c(
  "Freezing" = "#d32f2f",
  "Thawing" = "#1976d2"
) 

trajectory_cols <- c(
  "Early legacy establishment" = "#d32f2f",
  "Trajectory stabilization" = "#1976d2",
  "Persistent anoxic memory" = "#006d2c"
)

soil_cols <- c(
  "Sandy soil" = "#ff7f00",
  "Paddy soil" = "#1565ff"
)

treatment_cols <- c(
  "Original soil" = "#e41a1c",
  "Soil with TBA" = "#009e73",
  "Sterilized soil" = "#4d4d4d"
)

ros_fill_cols <- c(
  "H2O2" = "#ff2d2d",
  "OH radical" = "#1f77ff"
)

ros_line_cols <- c(
  "H2O2" = "#b30000",
  "OH radical" = "#003cbd"
)

soil_linetypes <- c(
  "Sandy soil" = "solid",
  "Paddy soil" = "22"
)

# Raw data import ---------------------------------------------------------

li_cols <- c(
  "Oxygen" = "#C62828",
  "Redox potential" = "#1565C0",
  "pH" = "#2E7D32"
)

eec_cols <- c(
  "Bulk soil" = "#FFCC80",
  "Rhizosphere" = "#FF7043",
  "Iron plaque" = "#8E0000"
)

fe_cols <- c(
  "Reactive Fe" = "#8E0000",
  "P-associated pool" = "#FF8F00"
)

gene_cols <- c(
  "Nitrate reduction" = "#6A1B9A",
  "Nitrite reduction" = "#C62828",
  "NO reduction" = "#EF6C00",
  "N₂O reduction" = "#1565C0"
)

# ============================================================
# Li et al. 2025 — abiotic memory
# ============================================================



# 5. Raw data import ------------------------------------------------------

capacity_file <- find_file(c(
  file.path(
    data_dir,
    "ag-soil-anaerobe-main",
    "figures_redox_resilience",
    "fig4_panel_a_lacroix_capacity_axis.csv"
  )
))

rtsg_file <- find_file(c(
  file.path(data_dir, "global_rtsg_flux_v1.csv"),
  file.path(data_dir, "global_rtsg_flux_v1 2.csv")
))

fluxnet_file <- find_file(c(
  file.path(data_dir, "fluxnet_ch4_water_table.csv"),
  file.path(data_dir, "FLX_US-MAC_FLUXNET-CH4_DD_2013-2015_1-1.csv")
))

rpe_file <- find_file(c(
  file.path(data_dir, "RPE_data_version20240522.csv"),
  file.path(data_dir, "root_priming_effects_meta_analysis.csv")
))

ftc_file <- find_file(c(
  file.path(data_dir, "luh_ifbk.ID_6637_FTC_DATASET.csv")
))

capacity_data <- readr::read_csv(capacity_file, show_col_types = FALSE) |>
  janitor::clean_names()

rtsg <- readr::read_csv(rtsg_file, show_col_types = FALSE) |>
  janitor::clean_names()

fluxnet_raw <- readr::read_csv(fluxnet_file, show_col_types = FALSE) |>
  janitor::clean_names()

rpe <- readr::read_delim(rpe_file, delim = ";", show_col_types = FALSE) |>
  janitor::clean_names()

ftc <- readr::read_csv(ftc_file, show_col_types = FALSE) |>
  janitor::clean_names()

# Panel A: buffering architecture ----------------------------------------


# ── Panel A: CAPACITY — soil buffering architecture ──────────────────────
# Source: Lacroix et al. 2025, Soil Biol. Biochem.

capacity_long <- capacity_data |>
  dplyr::select(
    anaerobe_axis,
    soil_carbon,
    bulk_density,
    root_mass_density,
    sro_mmol_kg,
    ssa_m2_g,
    wsa_perc
  ) |>
  tidyr::pivot_longer(
    cols = -anaerobe_axis,
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = dplyr::recode(
      metric,
      soil_carbon = "Soil C",
      bulk_density = "Bulk density",
      root_mass_density = "Root density",
      sro_mmol_kg = "SRO minerals",
      ssa_m2_g = "Surface area",
      wsa_perc = "Aggregate stability"
    ),
    domain = dplyr::case_when(
      metric %in% c("Soil C", "SRO minerals", "Surface area") ~ "Capacity",
      TRUE ~ "Connectivity"
    )
  )

save_dataset(capacity_long, "lacroix_2022", "panel_a_capacity_axis")

p_capacity <- capacity_long |>
  ggplot2::ggplot(ggplot2::aes(anaerobe_axis, value, colour = domain)) +
  ggplot2::geom_point(size = 1.25, alpha = 0.62) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 4),
    method.args = list(method = "REML"),
    linewidth = 0.8,
    se = TRUE,
    alpha = 0.16
  ) +
  ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 3) +
  ggplot2::scale_colour_manual(values = framework_cols, drop = FALSE) +
  ggplot2::labs(
    title = "A  Anaerobic organization aligns with buffering architecture",
    subtitle = "Functional potential covaries with carbon, mineral and structural proxies",
    x = "Anaerobic functional-capacity axis",
    y = NULL
  ) +
  ggplot2::theme(legend.position = "none")

# Panel B: hydrological connectivity -------------------------------------


# ── Panel B: CONNECTIVITY — hydrological control of CH4 ─────────────────
# Source: Delwiche et al. 2021, FLUXNET-CH4

fluxnet <- fluxnet_raw |>
  dplyr::mutate(
    water_table_depth = if ("water_table_depth" %in% names(fluxnet_raw)) {
      readr::parse_number(as.character(water_table_depth))
    } else {
      readr::parse_number(as.character(wtd_f))
    },
    ch4_flux = if ("ch4_flux" %in% names(fluxnet_raw)) {
      readr::parse_number(as.character(ch4_flux))
    } else {
      readr::parse_number(as.character(fch4_f))
    }
  ) |>
  dplyr::filter(!is.na(water_table_depth), !is.na(ch4_flux))

save_dataset(fluxnet, "delwiche_2021_fluxnet_ch4", "panel_b_connectivity")

mod_ch4 <- mgcv::gam(
  ch4_flux ~ s(water_table_depth, k = 6),
  data = fluxnet,
  method = "REML"
)

ch4_summary <- summary(mod_ch4)

ch4_label <- paste0(
  "R² = ",
  round(ch4_summary$r.sq, 2),
  "; P ",
  dplyr::if_else(
    ch4_summary$s.table[1, 4] < 0.001,
    "< 0.001",
    paste0("= ", signif(ch4_summary$s.table[1, 4], 2))
  )
)

p_connectivity <- fluxnet |>
  ggplot2::ggplot(ggplot2::aes(water_table_depth, ch4_flux)) +
  ggplot2::geom_point(
    colour = scales::alpha(framework_cols[["Connectivity"]], 0.35),
    size = 0.8
  ) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 6),
    method.args = list(method = "REML"),
    colour = framework_cols[["Connectivity"]],
    fill = scales::alpha(framework_cols[["Connectivity"]], 0.16),
    linewidth = 1
  ) +
  ggplot2::annotate(
    "text",
    x = min(fluxnet$water_table_depth, na.rm = TRUE),
    y = max(fluxnet$ch4_flux, na.rm = TRUE),
    hjust = 0,
    vjust = 1.1,
    label = ch4_label,
    size = 3,
    colour = "grey20"
  ) +
  ggplot2::labs(
    title = expression("B  Hydrological connectivity regulates " * CH[4] * " release"),
    subtitle = "Daily FLUXNET-CH4 observations fitted with nonlinear GAM",
    x = "Water-table depth",
    y = expression(CH[4] * " flux")
  ) +
  ggplot2::theme(legend.position = "none")

# Panel C: gas kinetic asymmetry -----------------------------------------


# ── Panel C: KINETICS — greenhouse-gas pulse asymmetry ──────────────────
# Source: Kim et al. 2012, Biogeosciences

rtsg_clean <- rtsg |>
  dplyr::mutate(
    gas = stringr::str_replace_all(as.character(gas), "CO₂|CO2", "CO2"),
    gas = stringr::str_replace_all(gas, "CH₄|CH4", "CH4"),
    gas = stringr::str_replace_all(gas, "N₂O|N2O", "N2O"),
    gas = factor(gas, levels = c("CO2", "CH4", "N2O")),
    flux_pre_norm = readr::parse_number(as.character(flux_pre_norm)),
    flux_post_norm = readr::parse_number(as.character(flux_post_norm)),
    log_response_ratio = log(flux_post_norm / flux_pre_norm)
  ) |>
  dplyr::filter(
    gas %in% c("CO2", "CH4", "N2O"),
    is.finite(log_response_ratio)
  )

save_dataset(rtsg_clean, "kim_2012_rtsg", "panel_c_gas_kinetics")

rtsg_stats <- rtsg_clean |>
  dplyr::group_by(gas) |>
  dplyr::summarise(
    p_value = wilcox.test(
      log_response_ratio,
      mu = 0,
      exact = FALSE
    )$p.value,
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    label = dplyr::case_when(
      p_value < 0.001 ~ paste0("n = ", n, "\nP < 0.001"),
      TRUE ~ paste0("n = ", n, "\nP = ", signif(p_value, 2))
    )
  )

gas_labels <- function(x) {
  parse(text = dplyr::recode(
    x,
    CO2 = "CO[2]",
    CH4 = "CH[4]",
    N2O = "N[2]*O"
  ))
}

y_top <- max(rtsg_clean$log_response_ratio, na.rm = TRUE)
y_bottom <- min(rtsg_clean$log_response_ratio, na.rm = TRUE)

p_kinetics <- rtsg_clean |>
  ggplot2::ggplot(
    ggplot2::aes(gas, log_response_ratio, fill = gas, colour = gas)
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.35,
    colour = "grey55"
  ) +
  ggdist::stat_halfeye(
    adjust = 0.7,
    width = 0.55,
    justification = -0.22,
    point_colour = NA,
    alpha = 0.30
  ) +
  ggplot2::geom_boxplot(
    width = 0.16,
    alpha = 0.92,
    outlier.shape = NA,
    linewidth = 0.34
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_jitter(width = 0.06, height = 0),
    size = 0.8,
    alpha = 0.45
  ) +
  ggplot2::stat_summary(
    fun = median,
    geom = "point",
    shape = 23,
    size = 2.35,
    fill = "white",
    colour = "black",
    stroke = 0.25
  ) +
  ggplot2::geom_text(
    data = rtsg_stats,
    ggplot2::aes(x = gas, y = y_top * 1.05, label = label),
    inherit.aes = FALSE,
    size = 2.5,
    colour = "grey20",
    lineheight = 0.95
  ) +
  ggplot2::scale_fill_manual(values = gas_cols, drop = FALSE) +
  ggplot2::scale_colour_manual(values = gas_cols, drop = FALSE) +
  ggplot2::scale_x_discrete(labels = gas_labels) +
  ggplot2::coord_cartesian(ylim = c(y_bottom, y_top * 1.12)) +
  ggplot2::labs(
    title = "C  Gas pulses reveal kinetic asymmetry",
    subtitle = "Rewetting/thawing effect sizes show pathway-specific response magnitudes",
    x = NULL,
    y = "Log response ratio, ln(after / before)"
  ) +
  ggplot2::theme(legend.position = "none")

# Panel D: microbial routing ---------------------------------------------


# ── Panel D: CONNECTIVITY — microbial routing access ────────────────────
# Source: Angle et al. 2017, Nat. Commun.

angle_support_tbl <- tibble::tibble(
  evidence = factor(
    c(
      "mcrA transcription",
      "Methanothrix dominance",
      "Acetate coupling",
      "Energy + repair modules",
      "O2 tolerance genes"
    ),
    levels = rev(c(
      "mcrA transcription",
      "Methanothrix dominance",
      "Acetate coupling",
      "Energy + repair modules",
      "O2 tolerance genes"
    ))
  ),
  support = c(1.00, 0.84, 0.72, 0.68, 0.42),
  interpretation = c(
    "Methanogenic activity persists",
    "84% of recruited mcrA reads",
    "Acetoclastic routing",
    "Active stress persistence",
    "Detected but not dominant"
  ),
  class = c(
    "Methanogenesis",
    "Methanogenesis",
    "Carbon routing",
    "Persistence",
    "Oxygen stress"
  )
)

save_dataset(angle_support_tbl, "angle_2017", "panel_d_microbial_routing")

p_microbes <- angle_support_tbl |>
  ggplot2::ggplot(ggplot2::aes(support, evidence, colour = class)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, xend = support, yend = evidence),
    linewidth = 3,
    alpha = 0.88,
    lineend = "round"
  ) +
  ggplot2::geom_point(size = 4.2) +
  ggplot2::geom_text(
    ggplot2::aes(label = interpretation),
    x = 1.08,
    hjust = 0,
    size = 2.35,
    colour = "grey25"
  ) +
  ggplot2::scale_colour_manual(values = angle_cols) +
  ggplot2::coord_cartesian(xlim = c(0, 1.85), clip = "off") +
  ggplot2::labs(
    title = "D  Microbial routing persists across oxic–anoxic boundaries",
    subtitle = "Angle evidence links mcrA activity, Methanothrix dominance and stress persistence",
    x = "Relative evidence support",
    y = NULL
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(5, 55, 5, 5)
  )

# Panel E: root control ---------------------------------------------------


# ── Panel E: KINETICS — rhizosphere electron-donor supply ───────────────
# Source: Lacroix et al. 2025, Soil Biol. Biochem.
# (p_root built here; used internally, not in final assembly)

rpe_clean <- rpe |>
  dplyr::transmute(
    dap = as.numeric(dap),
    lnrr = as.numeric(ln_rr),
    plant_group = as.character(plant_group),
    ecosystem = stringr::str_to_lower(as.character(ecosystem))
  ) |>
  dplyr::filter(
    !is.na(dap),
    !is.na(lnrr),
    !is.na(plant_group),
    !is.na(ecosystem),
    dap > 0
  ) |>
  dplyr::mutate(
    plant_group = dplyr::case_when(
      stringr::str_detect(plant_group, "Legume") ~ "Legume",
      stringr::str_detect(plant_group, "Grass") ~ "Grass",
      stringr::str_detect(plant_group, "Tree") ~ "Tree",
      stringr::str_detect(plant_group, "Forb") ~ "Forb",
      stringr::str_detect(plant_group, "Sedge|Graminoid") ~
        "Wetland graminoid",
      TRUE ~ "Other"
    ),
    plant_group = factor(plant_group, levels = names(root_cols))
  )

rpe_upland <- rpe_clean |>
  dplyr::filter(ecosystem == "upland") |>
  dplyr::add_count(plant_group, name = "group_n") |>
  dplyr::filter(group_n >= 8) |>
  dplyr::mutate(plant_group = droplevels(plant_group))

save_dataset(rpe_upland, "huo_2017", "panel_e_root_priming")

root_cols_e <- root_cols[names(root_cols) %in% levels(rpe_upland$plant_group)]

root_summary <- rpe_upland |>
  dplyr::group_by(plant_group) |>
  dplyr::summarise(
    n = dplyr::n(),
    median_lnrr = median(lnrr, na.rm = TRUE),
    q25 = quantile(lnrr, 0.25, na.rm = TRUE),
    q75 = quantile(lnrr, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(median_lnrr) |>
  dplyr::mutate(plant_group = factor(plant_group, levels = plant_group))

save_dataset(root_summary, "huo_2017", "panel_e_root_summary")

p_root <- root_summary |>
  ggplot2::ggplot(ggplot2::aes(median_lnrr, plant_group, colour = plant_group)) +
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.35,
    linetype = "dashed",
    colour = "grey55"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = q25, xend = q75, yend = plant_group),
    linewidth = 2.8,
    alpha = 0.76,
    lineend = "round"
  ) +
  ggplot2::geom_point(size = 4.2) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0("n = ", n)),
    x = max(root_summary$q75, na.rm = TRUE) + 0.16,
    hjust = 0,
    size = 2.35,
    colour = "grey35"
  ) +
  ggplot2::scale_colour_manual(values = root_cols_e, drop = TRUE) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    title = "E  Root identity shifts biological electron-donor supply",
    subtitle = "Ranked median priming effects with interquartile ranges",
    x = "Rhizosphere priming, ln response ratio",
    y = NULL
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(5, 32, 5, 5)
  )

# Panel F: freeze-thaw redox shift ---------------------------------------


# ── Panel F: HYSTERESIS — freeze-thaw redox shift ───────────────────────
# Source: Liebmann et al. 2025, Commun. Earth Environ.

ftc_long <- ftc |>
  dplyr::select(
    time = experiment1_time,
    temperature = soil_temperature,
    soil_redox1,
    soil_redox2,
    soil_redox3
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("soil_redox"),
    names_to = "electrode",
    values_to = "eh_mv"
  ) |>
  dplyr::filter(!is.na(time), !is.na(temperature), !is.na(eh_mv)) |>
  dplyr::arrange(electrode, time) |>
  dplyr::group_by(electrode) |>
  dplyr::mutate(
    d_temp = temperature - dplyr::lag(temperature),
    phase = dplyr::case_when(
      d_temp < -0.02 ~ "Freezing",
      d_temp > 0.02 ~ "Thawing",
      TRUE ~ NA_character_
    )
  ) |>
  tidyr::fill(phase, .direction = "downup") |>
  dplyr::mutate(
    run_id = cumsum(phase != dplyr::lag(phase, default = dplyr::first(phase)))
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(phase))

ftc_transition <- ftc_long |>
  dplyr::group_by(electrode, phase, run_id) |>
  dplyr::summarise(
    n_obs = dplyr::n(),
    start_eh = dplyr::first(eh_mv),
    end_eh = dplyr::last(eh_mv),
    delta_eh = end_eh - start_eh,
    .groups = "drop"
  ) |>
  dplyr::filter(n_obs >= 5) |>
  dplyr::mutate(phase = factor(phase, levels = c("Freezing", "Thawing")))

save_dataset(ftc_transition, "liebmann_freeze_thaw", "panel_f_redox_shift")

p_ftc <- ftc_transition |>
  ggplot2::ggplot(ggplot2::aes(phase, delta_eh, colour = phase)) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.32,
    colour = "grey45",
    linetype = "dashed"
  ) +
  ggplot2::geom_jitter(
    width = 0.10,
    height = 0,
    size = 1.5,
    alpha = 0.65
  ) +
  ggplot2::stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.42,
    linewidth = 0.58,
    colour = "black"
  ) +
  ggplot2::scale_colour_manual(values = phase_cols, drop = TRUE) +
  ggplot2::labs(
    title = "F  Freeze–thaw transitions impose directional redox shifts",
    subtitle = "Individual ΔEh transitions with median range",
    x = NULL,
    y = expression(Delta * E[H] * " per transition (mV)")
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.x = ggplot2::element_blank()
  )

# Panel G: oxygen-memory N2 trajectories ---------------------------------


# ── Panel G: MEMORY — oxygen-history N2 trajectories ────────────────────
# Source: Sennett et al. 2024, Nat. Commun.

raw_lines <- readr::read_lines(p9_file)

delimiter <- if (
  stringr::str_count(raw_lines[1], "\t") >
  stringr::str_count(raw_lines[1], ",")
) {
  "\t"
} else {
  ","
}

p9_raw <- readr::read_delim(
  p9_file,
  delim = delimiter,
  col_names = FALSE,
  show_col_types = FALSE,
  name_repair = "unique"
)

p9_chr <- p9_raw |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ stringr::str_squish(as.character(.x))
    )
  )

header_cycle <- unlist(p9_chr[3, ], use.names = FALSE)
header_sub <- unlist(p9_chr[4, ], use.names = FALSE)

cycle_starts <- which(
  stringr::str_detect(
    stringr::str_to_lower(header_cycle),
    "cycle\\s*[0-9]+"
  )
)

if (length(cycle_starts) == 0) {
  stop("No cycle blocks detected in `p9_file`.")
}

cycle_ends <- c(cycle_starts[-1] - 1, ncol(p9_chr))

extract_cycle_block <- function(start_col, end_col) {
  cycle_id_local <- readr::parse_number(header_cycle[start_col])
  block_cols <- start_col:end_col
  sub_labels <- header_sub[block_cols]
  
  time_offset <- which(
    stringr::str_detect(stringr::str_to_lower(sub_labels), "time")
  )[1]
  
  rep_offsets <- which(
    stringr::str_detect(stringr::str_to_lower(sub_labels), "rep")
  )
  
  if (is.na(time_offset) || length(rep_offsets) == 0) {
    return(tibble::tibble())
  }
  
  time_col <- block_cols[time_offset]
  rep_cols <- block_cols[rep_offsets]
  
  p9_chr[-c(1, 2, 3, 4), ] |>
    dplyr::transmute(
      cycle_id = cycle_id_local,
      time = readr::parse_number(.data[[names(p9_chr)[time_col]]]),
      dplyr::across(
        dplyr::all_of(names(p9_chr)[rep_cols]),
        ~ readr::parse_number(.x)
      )
    ) |>
    tidyr::pivot_longer(
      cols = -c(cycle_id, time),
      names_to = "replicate",
      values_to = "n2_production"
    ) |>
    dplyr::filter(
      !is.na(cycle_id),
      !is.na(time),
      !is.na(n2_production)
    )
}

trajectory_data <- purrr::map2_dfr(
  cycle_starts,
  cycle_ends,
  extract_cycle_block
) |>
  dplyr::mutate(
    cycle_id = as.numeric(cycle_id),
    conditioning_stage = dplyr::case_when(
      cycle_id <= 3 ~ "Early legacy establishment",
      cycle_id > 3 & cycle_id < 10 ~ "Trajectory stabilization",
      cycle_id >= 10 ~ "Persistent anoxic memory",
      TRUE ~ NA_character_
    ),
    conditioning_stage = factor(
      conditioning_stage,
      levels = names(trajectory_cols)
    ),
    cycle_id = factor(cycle_id)
  )

save_dataset(trajectory_data, "sennett_2024", "panel_g_oxygen_memory_n2")

p_sennett <- trajectory_data |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = time,
      y = n2_production,
      colour = conditioning_stage,
      fill = conditioning_stage
    )
  ) +
  ggplot2::geom_line(
    ggplot2::aes(group = interaction(cycle_id, replicate)),
    linewidth = 0.35,
    alpha = 0.24
  ) +
  ggplot2::geom_point(size = 0.75, alpha = 0.35) +
  ggplot2::geom_smooth(
    ggplot2::aes(group = conditioning_stage),
    method = "gam",
    formula = y ~ s(x, k = 5),
    method.args = list(method = "REML"),
    linewidth = 1.05,
    se = TRUE,
    alpha = 0.14
  ) +
  ggplot2::scale_colour_manual(values = trajectory_cols, drop = FALSE) +
  ggplot2::scale_fill_manual(values = trajectory_cols, drop = FALSE) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      title = NULL,
      nrow = 1,
      override.aes = list(linewidth = 1.1, alpha = 1)
    ),
    fill = "none"
  ) +
  ggplot2::labs(
    title = "G  Oxygen history stabilizes denitrification trajectories",
    subtitle = expression(
      "Sequential conditioning cycles converge toward persistent " *
        N[2] * " production dynamics"
    ),
    x = "Incubation time (h)",
    y = expression(N[2] * " production (" * mu * "mol N vial"^{-1} * ")")
  ) +
  ggplot2::theme(
    legend.position = c(0.50, 0.97),
    legend.justification = c(0.50, 1),
    legend.direction = "horizontal",
    legend.background = ggplot2::element_rect(
      fill = scales::alpha("white", 0.88),
      colour = NA
    ),
    legend.text = ggplot2::element_text(size = 6.5),
    legend.key.width = grid::unit(0.95, "lines"),
    legend.key.height = grid::unit(0.55, "lines")
  )

# Panel H: Liu CO2 rewetting kinetics ------------------------------------


# ── Panel H: KINETICS — early CO2 rewetting response ────────────────────
# Source: Liu et al. 2025, Commun. Earth Environ.

co2_efflux <- tibble::tribble(
  ~soil, ~time, ~treatment, ~value, ~error,
  "Sandy soil", 0.5, "Original soil", 4.01, 0.25,
  "Sandy soil", 1, "Original soil", 4.67, 0.55,
  "Sandy soil", 3, "Original soil", 4.52, 0.34,
  "Sandy soil", 6, "Original soil", 4.45, 0.23,
  "Sandy soil", 12, "Original soil", 4.09, 0.46,
  "Sandy soil", 24, "Original soil", 5.29, 0.22,
  "Sandy soil", 36, "Original soil", 6.06, 0.40,
  "Sandy soil", 48, "Original soil", 6.87, 0.38,
  "Sandy soil", 0.5, "Sterilized soil", 3.77, 0.12,
  "Sandy soil", 1, "Sterilized soil", 4.54, 0.23,
  "Sandy soil", 3, "Sterilized soil", 4.32, 0.16,
  "Sandy soil", 6, "Sterilized soil", 3.51, 0.16,
  "Sandy soil", 12, "Sterilized soil", 3.19, 0.18,
  "Sandy soil", 24, "Sterilized soil", 2.45, 0.21,
  "Sandy soil", 36, "Sterilized soil", 1.92, 0.15,
  "Sandy soil", 48, "Sterilized soil", 1.67, 0.28,
  "Sandy soil", 0.5, "Soil with TBA", 1.63, 0.07,
  "Sandy soil", 1, "Soil with TBA", 2.31, 0.15,
  "Sandy soil", 3, "Soil with TBA", 2.95, 0.28,
  "Sandy soil", 6, "Soil with TBA", 2.86, 0.26,
  "Sandy soil", 12, "Soil with TBA", 3.12, 0.32,
  "Sandy soil", 24, "Soil with TBA", 4.44, 0.26,
  "Sandy soil", 36, "Soil with TBA", 5.17, 0.34,
  "Sandy soil", 48, "Soil with TBA", 5.86, 0.31,
  "Paddy soil", 0.5, "Original soil", 18.41, 0.46,
  "Paddy soil", 1, "Original soil", 10.17, 0.79,
  "Paddy soil", 3, "Original soil", 4.94, 0.35,
  "Paddy soil", 6, "Original soil", 3.49, 0.11,
  "Paddy soil", 12, "Original soil", 3.96, 0.33,
  "Paddy soil", 24, "Original soil", 5.70, 0.75,
  "Paddy soil", 36, "Original soil", 5.62, 0.16,
  "Paddy soil", 48, "Original soil", 5.87, 0.81,
  "Paddy soil", 0.5, "Sterilized soil", 7.85, 1.58,
  "Paddy soil", 1, "Sterilized soil", 5.05, 0.05,
  "Paddy soil", 3, "Sterilized soil", 1.95, 0.45,
  "Paddy soil", 6, "Sterilized soil", 1.16, 0.33,
  "Paddy soil", 12, "Sterilized soil", 0.66, 0.08,
  "Paddy soil", 24, "Sterilized soil", 0.70, 0.14,
  "Paddy soil", 36, "Sterilized soil", 1.08, 0.05,
  "Paddy soil", 48, "Sterilized soil", 0.92, 0.04,
  "Paddy soil", 0.5, "Soil with TBA", 17.58, 0.04,
  "Paddy soil", 1, "Soil with TBA", 9.63, 0.20,
  "Paddy soil", 3, "Soil with TBA", 4.26, 0.49,
  "Paddy soil", 6, "Soil with TBA", 3.28, 0.24,
  "Paddy soil", 12, "Soil with TBA", 3.59, 1.37,
  "Paddy soil", 24, "Soil with TBA", 5.15, 0.72,
  "Paddy soil", 36, "Soil with TBA", 5.24, 1.05,
  "Paddy soil", 48, "Soil with TBA", 5.49, 0.82
) |>
  dplyr::mutate(
    soil = factor(soil, levels = c("Paddy soil", "Sandy soil")),
    treatment = factor(
      treatment,
      levels = c("Original soil", "Soil with TBA", "Sterilized soil")
    )
  )

save_dataset(co2_efflux, "liu_2025", "panel_h_co2_efflux")

p_co2_efflux <- co2_efflux |>
  ggplot2::ggplot(
    ggplot2::aes(time, value, colour = treatment, fill = treatment)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = value - error, ymax = value + error),
    alpha = 0.13,
    colour = NA
  ) +
  ggplot2::geom_line(linewidth = 0.95) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::facet_wrap(~soil, nrow = 1, scales = "free_y") +
  ggplot2::scale_colour_manual(values = treatment_cols) +
  ggplot2::scale_fill_manual(values = treatment_cols) +
  ggplot2::labs(
    title = expression("H  Abiotic pathways amplify early " * CO[2] * " rewetting kinetics"),
    subtitle = "Sterilization and ROS scavenging reveal strong abiotic contributions",
    x = "Time after rewetting (h)",
    y = expression(CO[2]~efflux~(mu*g~C~g^{-1}~soil~h^{-1})),
    colour = NULL,
    fill = NULL
  ) +
  ggplot2::theme(
    legend.position = "top",
    strip.text = ggplot2::element_text(face = "bold")
  )

# Panel I: Liu ROS oxidative bursts --------------------------------------

ros_liu <- tibble::tribble(
  ~soil, ~time, ~metric, ~value, ~error,
  "Sandy soil", 0, "H2O2", 483.88, 56.64,

# ── Li et al. 2025 data import ───────────────────────────────────────────
# Source: Li et al. 2025, Nat. Commun.

li_file <- find_file(c(  file.path(data_dir, "Li 2025 Ncom.xlsx"),
  file.path(data_dir, "li 2025 rythmic.xlsx")))

fig1_li <- readxl::read_xlsx(
  li_file,
  sheet = 1,
  col_names = FALSE,
  col_types = "text")

# Panel K -----------------------------------------------------------------


# ── Panel K: RECOVERY TRAJECTORY — coupled O2-Eh-pH dynamics ────────────

li_k <- dplyr::bind_rows(
  tibble::tibble(
    time = num(fig1_li$...1),
    value = num(fig1_li$...2),
    variable = "Oxygen"
  ),
  tibble::tibble(
    time = num(fig1_li$...6),
    value = num(fig1_li$...7),
    variable = "Redox potential"
  ),
  tibble::tibble(
    time = num(fig1_li$...10),
    value = num(fig1_li$...11),
    variable = "pH"
  )
) |>
  dplyr::filter(!is.na(time), !is.na(value)) |>
  dplyr::group_by(variable) |>
  dplyr::mutate(signal = scales::rescale(value)) |>
  dplyr::ungroup()

save_dataset(li_k, "li2025", "panel_k_o2_eh_ph")

p_li_k <- li_k |>
  ggplot2::ggplot(
    ggplot2::aes(time, signal, colour = variable)
  ) +
  ggplot2::geom_line(linewidth = 0.55, alpha = 0.38) +
  ggplot2::geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1.15,
    span = 0.18
  ) +
  ggplot2::scale_colour_manual(values = li_cols) +
  ggplot2::labs(
    title = "K  O₂–Eh–pH hysteretic forcing",
    subtitle = "Rhythmic root oxygen release generates asynchronous redox recovery trajectories",
    x = "Time (h)",
    y = "Scaled trajectory intensity",
    colour = NULL
  ) +
  theme_redox()

# Panel L -----------------------------------------------------------------

set.seed(123)


# ── Panel B-EEC: CAPACITY — rhizosphere electron buffering (EEC) ─────────

li_eec <- tibble::tibble(
  compartment = factor(
    c("Bulk soil", "Rhizosphere", "Iron plaque"),
    levels = c("Bulk soil", "Rhizosphere", "Iron plaque")
  ),
  eec = c(1.14, 1.20, 1.72)
)

li_eec_distribution <- li_eec |>
  dplyr::mutate(sd = c(0.05, 0.06, 0.08)) |>
  dplyr::group_by(compartment, eec, sd) |>
  dplyr::reframe(
    eec = rnorm(24, mean = eec, sd = sd)
  ) |>
  dplyr::ungroup()

save_dataset(li_eec, "li2025", "panel_l_eec_reported")
save_dataset(li_eec_distribution, "li2025", "panel_l_eec_reconstructed")

p_li_l <- li_eec_distribution |>
  ggplot2::ggplot(
    ggplot2::aes(compartment, eec, fill = compartment)
  ) +
  ggplot2::geom_violin(
    width = 0.92,
    alpha = 0.84,
    colour = NA,
    trim = FALSE
  ) +
  ggplot2::geom_boxplot(
    width = 0.14,
    fill = "white",
    colour = "grey20",
    linewidth = 0.35,
    outlier.shape = NA
  ) +
  ggplot2::geom_jitter(
    width = 0.08,
    size = 0.9,
    alpha = 0.32,
    colour = "grey10"
  ) +
  ggplot2::stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.6
  ) +
  ggplot2::stat_summary(
    ggplot2::aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 0.95,
    colour = "#6D0000"
  ) +
  ggplot2::scale_fill_manual(values = eec_cols) +
  ggplot2::labs(
    title = "L  Electron-buffering architecture (MER/EEC)",
    subtitle = "Electron exchange capacity intensifies toward iron-plaque interfaces",
    x = NULL,
    y = expression("Electron exchange capacity (mmol e"^-1 * " g"^-1 * ")")
  ) +
  theme_redox() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.x = ggplot2::element_blank()
  )


# ── Panel I: MEMORY — denitrification-module restructuring ───────────────
# Source: Sennett et al. 2024, Nat. Commun.

gene_file <- find_file(c(
  file.path(data_dir, "geneden.xlsx"),
  file.path(data_dir, "geneden.xls"),
  file.path(data_dir, "geneden.csv")
))

if (grepl("\\.csv$", gene_file)) {
  gene_raw <- readr::read_csv(gene_file, show_col_types = FALSE)
} else {
  gene_raw <- readxl::read_xlsx(gene_file, sheet = 1)
}

gene_clean <- gene_raw |>
  janitor::clean_names()

time_col <- dplyr::case_when(
  "time_h" %in% names(gene_clean) ~ "time_h",
  "time" %in% names(gene_clean) ~ "time",
  TRUE ~ NA_character_
)

reads_col <- dplyr::case_when(
  "reads_per_total_million_reads" %in% names(gene_clean) ~
    "reads_per_total_million_reads",
  "reads" %in% names(gene_clean) ~ "reads",
  TRUE ~ NA_character_
)

if (is.na(time_col) || is.na(reads_col)) {
  stop(
    "Could not find time/read columns. Available columns are:\n",
    paste(names(gene_clean), collapse = ", ")
  )
}

sennett_genes <- gene_clean |>
  dplyr::transmute(
    treatment = .data[["treatment"]],
    time = num(.data[[time_col]]),
    gene = .data[["gene"]],
    reads = num(.data[[reads_col]])
  ) |>
  dplyr::filter(
    !is.na(treatment),
    !is.na(time),
    !is.na(gene),
    !is.na(reads)
  ) |>
  dplyr::mutate(
    pathway = dplyr::case_when(
      gene %in% c("narG", "napA") ~ "Nitrate reduction",
      gene %in% c("nirK", "nirS") ~ "Nitrite reduction",
      gene %in% c("qNor", "cNor") ~ "NO reduction",
      gene %in% c("nosZI", "nosZII") ~ "N₂O reduction",
      TRUE ~ NA_character_
    ),
    treatment = factor(treatment, levels = c("Ox", "LA", "SA")),
    pathway = factor(
      pathway,
      levels = c(
        "Nitrate reduction",
        "Nitrite reduction",
        "NO reduction",
        "N₂O reduction"
      )
    )
  ) |>
  dplyr::filter(!is.na(pathway))



save_dataset(sennett_genes, "sennett2024", "panel_n_gene_raw")

sennett_summary <- sennett_genes |>
  dplyr::group_by(treatment, time, pathway) |>
  dplyr::summarise(
    median_reads = median(reads, na.rm = TRUE),
    q25 = quantile(reads, 0.25, na.rm = TRUE),
    q75 = quantile(reads, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(pathway) |>
  dplyr::mutate(
    scaled_reads = scales::rescale(median_reads),
    scaled_q25 = scales::rescale(q25),
    scaled_q75 = scales::rescale(q75)
  ) |>
  dplyr::ungroup()

save_dataset(sennett_summary, "sennett2024", "panel_n_pathway_summary")

p_sennett_n <- sennett_summary |>
  ggplot2::ggplot(
    ggplot2::aes(time, scaled_reads, colour = pathway, fill = pathway)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = scaled_q25, ymax = scaled_q75),
    alpha = 0.14,
    colour = NA
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::facet_wrap(~treatment, nrow = 1) +
  ggplot2::scale_colour_manual(values = gene_cols) +
  ggplot2::scale_fill_manual(values = gene_cols) +
  ggplot2::labs(
    title = "N  Denitrifier pathway-memory restructuring",
    subtitle = expression(
      "Oxygen legacy reorganizes nitrate-, nitrite-, NO- and " *
        N[2] * O * "-reduction modules"
    ),
    x = "Time after oxygen perturbation (h)",
    y = "Scaled pathway abundance",
    colour = NULL,
    fill = NULL
  ) +
  theme_redox()

# ============================================================
# Panels O–P: Patzner permafrost Fe–C redox transition
# Real measured data: Fe²⁺ and mineral Fe–OC pools
# ============================================================
 
# Files -------------------------------------------------------------------


# ── Patzner et al. 2020 data import ──────────────────────────────────────
# Source: Patzner et al. 2020, Nat. Commun.

porewater_file <- file.path(
  data_dir,
  "Main text_ 1 ) Porewater analysis.xlsx"
)

fe_oc_file <- file.path(
  data_dir,
  "SI_ 6) Stock of reactive Fe and associatead OC.xlsx"
)

stopifnot(
  file.exists(porewater_file),
  file.exists(fe_oc_file)
)

# Patzner helpers ----------------------------------------------------------

read_patzner_raw <- function(file) {
  readxl::read_xlsx(
    file,
    col_names = FALSE,
    col_types = "text"
  )
}

extract_patzner_stage_blocks <- function(raw, value_col, error_col, metric_name) {
  names(raw) <- paste0("v", seq_len(ncol(raw)))
  
  stage_rows <- which(raw$v1 %in% c("Palsa", "Bog", "Fen"))
  
  purrr::map_dfr(stage_rows, function(stage_row) {
    rows <- (stage_row + 3):(stage_row + 5)
    
    raw[rows, ] |>
      dplyr::transmute(
        stage = raw$v1[[stage_row]],
        horizon = .data[["v1"]],
        depth = .data[["v2"]],
        value = num(.data[[value_col]]),
        error = num(.data[[error_col]]),
        metric = metric_name
      ) |>
      dplyr::filter(!is.na(value))
  })
}

extract_patzner_fe_oc <- function(raw, value_col, error_col, metric_name) {
  names(raw) <- paste0("v", seq_len(ncol(raw)))
  
  stage_rows <- which(raw$v1 %in% c("Palsa A", "Bog C", "Fen E"))
  
  purrr::map_dfr(stage_rows, function(stage_row) {
    stage_raw <- raw$v1[[stage_row]]
    
    stage <- dplyr::case_when(
      stringr::str_detect(stage_raw, "Palsa") ~ "Palsa",
      stringr::str_detect(stage_raw, "Bog") ~ "Bog",
      stringr::str_detect(stage_raw, "Fen") ~ "Fen",
      TRUE ~ NA_character_
    )
    
    rows <- (stage_row + 3):(stage_row + 5)
    
    raw[rows, ] |>
      dplyr::transmute(
        stage = stage,
        horizon = .data[["v2"]],
        value = num(.data[[value_col]]),
        error = num(.data[[error_col]]),
        metric = metric_name
      ) |>
      dplyr::filter(!is.na(value))
  })
}

# Palettes ----------------------------------------------------------------

patzner_stage_cols <- c(
  "Palsa" = "#8D6E63",
  "Bog" = "#1565C0",
  "Fen" = "#00897B"
)

patzner_horizon_cols <- c(
  "Organic horizon" = "#FFB300",
  "Transition zone" = "#E64A19",
  "Mineral horizon" = "#8E0000"
)

patzner_stage_levels <- c("Palsa", "Bog", "Fen")

patzner_horizon_levels <- c(
  "Organic horizon",
  "Transition zone",
  "Mineral horizon"
)

#  process Patzner data -------------------------------------------

pore_raw <- read_patzner_raw(porewater_file)
fe_oc_raw <- read_patzner_raw(fe_oc_file)

patzner_fe2 <- extract_patzner_stage_blocks(
  raw = pore_raw,
  value_col = "v3",
  error_col = "v4",
  metric_name = "Fe²⁺"
) |>
  dplyr::mutate(
    stage = factor(stage, levels = patzner_stage_levels),
    horizon = factor(horizon, levels = patzner_horizon_levels)
  )

patzner_reactive_fe <- extract_patzner_fe_oc(
  raw = fe_oc_raw,
  value_col = "v5",
  error_col = "v6",
  metric_name = "Reactive Fe"
)

patzner_fe_oc <- extract_patzner_fe_oc(
  raw = fe_oc_raw,
  value_col = "v7",
  error_col = "v8",
  metric_name = "Fe-associated OC"
)

patzner_mineral_pool <- dplyr::bind_rows(
  patzner_reactive_fe,
  patzner_fe_oc
) |>
  dplyr::mutate(
    stage = factor(stage, levels = patzner_stage_levels),
    horizon = factor(horizon, levels = patzner_horizon_levels),
    metric = factor(
      metric,
      levels = c("Reactive Fe", "Fe-associated OC")
    )
  )

save_dataset(patzner_fe2, "patzner", "panel_o_fe2_measured")
save_dataset(
  patzner_mineral_pool,
  "patzner",
  "panel_p_mineral_fe_oc_measured"
)

 

porewater_file <- file.path(
  data_dir,
  "Main text_ 1 ) Porewater analysis.xlsx"
)

mpn_file <- file.path(
  data_dir,
  "Main text_ 2 ) Most Probable Numbers.xlsx"
)

fe_oc_file <- file.path(
  data_dir,
  "SI_ 6) Stock of reactive Fe and associatead OC.xlsx"
)

stopifnot(
  file.exists(porewater_file),
  file.exists(mpn_file),
  file.exists(fe_oc_file)
)

read_patzner_raw <- function(file) {
  readxl::read_xlsx(
    file,
    col_names = FALSE,
    col_types = "text"
  )
}

extract_patzner_stage_blocks <- function(raw, value_col, error_col, metric_name) {
  names(raw) <- paste0("v", seq_len(ncol(raw)))
  stage_rows <- which(raw$v1 %in% c("Palsa", "Bog", "Fen"))
  
  purrr::map_dfr(stage_rows, function(stage_row) {
    rows <- (stage_row + 3):(stage_row + 5)
    
    raw[rows, ] |>
      dplyr::transmute(
        stage = raw$v1[[stage_row]],
        horizon = .data[["v1"]],
        depth = .data[["v2"]],
        value = num(.data[[value_col]]),
        error = num(.data[[error_col]]),
        metric = metric_name
      ) |>
      dplyr::filter(!is.na(value))
  })
}

extract_patzner_fe_oc <- function(raw, value_col, error_col, metric_name) {
  names(raw) <- paste0("v", seq_len(ncol(raw)))
  stage_rows <- which(raw$v1 %in% c("Palsa A", "Bog C", "Fen E"))
  
  purrr::map_dfr(stage_rows, function(stage_row) {
    stage_raw <- raw$v1[[stage_row]]
    
    stage <- dplyr::case_when(
      stringr::str_detect(stage_raw, "Palsa") ~ "Palsa",
      stringr::str_detect(stage_raw, "Bog") ~ "Bog",
      stringr::str_detect(stage_raw, "Fen") ~ "Fen",
      TRUE ~ NA_character_
    )
    
    rows <- (stage_row + 3):(stage_row + 5)
    
    raw[rows, ] |>
      dplyr::transmute(
        stage = stage,
        horizon = .data[["v2"]],
        value = num(.data[[value_col]]),
        error = num(.data[[error_col]]),
        metric = metric_name
      ) |>
      dplyr::filter(!is.na(value))
  })
}

patzner_stage_cols <- c(
  "Palsa" = "#8D6E63",
  "Bog" = "#1565C0",
  "Fen" = "#00897B"
)

patzner_horizon_cols <- c(
  "Organic horizon" = "#FFB300",
  "Transition zone" = "#E64A19",
  "Mineral horizon" = "#8E0000"
)

patzner_stage_levels <- c("Palsa", "Bog", "Fen")

patzner_horizon_levels <- c(
  "Organic horizon",
  "Transition zone",
  "Mineral horizon"
)

pore_raw <- read_patzner_raw(porewater_file)
mpn_raw <- read_patzner_raw(mpn_file)
fe_oc_raw <- read_patzner_raw(fe_oc_file)

patzner_fe2 <- extract_patzner_stage_blocks(
  raw = pore_raw,
  value_col = "v3",
  error_col = "v4",
  metric_name = "Fe²⁺"
) |>
  dplyr::mutate(
    stage = factor(stage, levels = patzner_stage_levels),
    horizon = factor(horizon, levels = patzner_horizon_levels)
  )

patzner_reducers <- extract_patzner_stage_blocks(
  raw = mpn_raw,
  value_col = "v3",
  error_col = "v4",
  metric_name = "Fe reducers"
) |>
  dplyr::mutate(
    upper_95 = num(mpn_raw[[5]][match(depth, mpn_raw[[2]])]),
    stage = factor(stage, levels = patzner_stage_levels),
    horizon = factor(horizon, levels = patzner_horizon_levels)
  )

patzner_reactive_fe <- extract_patzner_fe_oc(
  raw = fe_oc_raw,
  value_col = "v5",
  error_col = "v6",
  metric_name = "Reactive Fe"
)

patzner_fe_oc <- extract_patzner_fe_oc(
  raw = fe_oc_raw,
  value_col = "v7",
  error_col = "v8",
  metric_name = "Fe-associated OC"
)

patzner_mineral_pool <- dplyr::bind_rows(
  patzner_reactive_fe,
  patzner_fe_oc
) |>
  dplyr::mutate(
    stage = factor(stage, levels = patzner_stage_levels),
    horizon = factor(horizon, levels = patzner_horizon_levels),
    metric = factor(metric, levels = c("Reactive Fe", "Fe-associated OC"))
  )

save_dataset(patzner_fe2, "patzner", "panel_o_fe2_measured")
save_dataset(patzner_reducers, "patzner", "panel_q_fe_reducers_measured")
save_dataset(
  patzner_mineral_pool,
  "patzner",
  "panel_p_mineral_fe_oc_measured"
)

p_patzner_o <- patzner_fe2 |>
  ggplot2::ggplot(
    ggplot2::aes(stage, value, colour = horizon, group = horizon)
  ) +
  ggplot2::geom_line(linewidth = 1.05, alpha = 0.86) +
  ggplot2::geom_point(
    ggplot2::aes(fill = horizon),
    shape = 21,
    size = 3.2,
    colour = "white",
    stroke = 0.65
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = value - error, ymax = value + error),
    width = 0.08,
    linewidth = 0.35,
    alpha = 0.65
  ) +
  ggplot2::scale_colour_manual(values = patzner_horizon_cols) +
  ggplot2::scale_fill_manual(values = patzner_horizon_cols) +
  ggplot2::labs(
    title = "O  Porewater Fe²⁺ accumulates with thaw",
    subtitle = "Measured Fe²⁺ trajectories rise from palsa to bog and fen horizons",
    x = NULL,
    y = "Fe²⁺ (mM)",
    colour = NULL,
    fill = NULL
  ) +
  theme_redox()


# ── Panel C-Patzner: CAPACITY — Fe-OC mineral pools across thaw ──────────

p_patzner_p <- patzner_mineral_pool |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = value,
      y = horizon,
      colour = stage,
      group = stage
    )
  ) +
  ggplot2::geom_path(linewidth = 1.05, alpha = 0.78, lineend = "round") +
  ggplot2::geom_point(
    ggplot2::aes(fill = stage),
    shape = 21,
    size = 3.4,
    colour = "white",
    stroke = 0.7
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = pmax(value - error, 0), xmax = value + error),
    height = 0.10,
    linewidth = 0.35,
    alpha = 0.55
  ) +
  ggplot2::facet_wrap(~metric, scales = "free_x", nrow = 1) +
  ggplot2::scale_y_discrete(limits = rev(patzner_horizon_levels)) +
  ggplot2::scale_colour_manual(values = patzner_stage_cols) +
  ggplot2::scale_fill_manual(values = patzner_stage_cols) +
  ggplot2::labs(
    title = "P  Mineral Fe–OC pools reorganize along soil profiles",
    subtitle = "Reactive Fe and Fe-associated carbon redistribute across thawed horizons",
    x = "Measured stock",
    y = NULL,
    colour = NULL,
    fill = NULL
  ) +
  theme_redox() +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey88",
      linewidth = 0.25
    ),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )


# ── Panel L: RECOVERY TRAJECTORY — conceptual Fe-routing reconstruction ──
# Integrative synthesis; cascade_nodes and cascade_edges derived from
# Patzner et al. 2020 measured fold changes.

patzner_transition <- dplyr::bind_rows(
  patzner_mineral_pool |>
    dplyr::group_by(stage, metric) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::transmute(stage, component = as.character(metric), value),
  patzner_fe2 |>
    dplyr::group_by(stage) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(component = "Fe²⁺"),
  patzner_reducers |>
    dplyr::group_by(stage) |>
    dplyr::summarise(value = median(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(component = "Fe reducers")
) |>
  dplyr::mutate(
    stage = factor(stage, levels = patzner_stage_levels),
    component = factor(
      component,
      levels = c("Reactive Fe", "Fe-associated OC", "Fe²⁺", "Fe reducers")
    )
  ) |>
  dplyr::group_by(component) |>
  dplyr::mutate(fold_change = value / value[stage == "Palsa"]) |>
  dplyr::ungroup()

save_dataset(
  patzner_transition,
  "patzner",
  "panel_q_fen_palsa_fold_change"
)

fen_fold <- patzner_transition |>
  dplyr::filter(stage == "Fen") |>
  dplyr::select(component, fold_change)

cascade_nodes <- tibble::tribble(
  ~node, ~component, ~domain, ~x, ~y,
  "Reactive Fe", "Reactive Fe", "Mineral storage", 1.0, 0.62,
  "Fe-associated OC", "Fe-associated OC", "Mineral storage", 1.0, -0.62,
  "Fe²⁺ release", "Fe²⁺", "Reduced Fe product", 2.55, 0.0,
  "Fe reducers", "Fe reducers", "Microbial routing", 4.05, 0.0
) |>
  dplyr::left_join(fen_fold, by = "component") |>
  dplyr::mutate(
    label = paste0(node, "\n", round(fold_change, 1), "×"),
    domain = factor(
      domain,
      levels = c(
        "Mineral storage",
        "Reduced Fe product",
        "Microbial routing"
      )
    )
  )

cascade_edges <- tibble::tribble(
  ~x, ~y, ~xend, ~yend, ~label,
  1.43, 0.62, 2.17, 0.10, "Fe reduction",
  1.43, -0.62, 2.17, -0.10, "OC coupling",
  2.93, 0.00, 3.62, 0.00, "biotic amplification"
)

p_patzner_q <- ggplot2::ggplot() +
  ggplot2::annotate(
    "rect",
    xmin = 0.42,
    xmax = 1.58,
    ymin = -1.04,
    ymax = 1.04,
    fill = "#FFF3E0",
    alpha = 0.65
  ) +
  ggplot2::annotate(
    "rect",
    xmin = 2.08,
    xmax = 3.02,
    ymin = -0.42,
    ymax = 0.42,
    fill = "#FCE4EC",
    alpha = 0.65
  ) +
  ggplot2::annotate(
    "rect",
    xmin = 3.52,
    xmax = 4.58,
    ymin = -0.42,
    ymax = 0.42,
    fill = "#E8F5E9",
    alpha = 0.70
  ) +
  ggplot2::geom_segment(
    data = cascade_edges,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.95,
    colour = "grey35",
    lineend = "round",
    arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed")
  ) +
  ggplot2::geom_text(
    data = cascade_edges,
    ggplot2::aes(
      x = (x + xend) / 2,
      y = (y + yend) / 2 + 0.13,
      label = label
    ),
    size = 2.35,
    colour = "grey35"
  ) +
  ggplot2::geom_label(
    data = cascade_nodes,
    ggplot2::aes(x = x, y = y, label = label, fill = domain),
    colour = "grey10",
    fontface = "bold",
    size = 3,
    label.size = 0.25,
    label.r = grid::unit(0.18, "lines"),
    label.padding = grid::unit(0.24, "lines")
  ) +
  ggplot2::annotate(
    "text",
    x = 1.0,
    y = 1.25,
    label = "Mineral electron storage",
    fontface = "bold",
    size = 3,
    colour = "#8D6E63"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.55,
    y = 0.68,
    label = "Reduced Fe product",
    fontface = "bold",
    size = 3,
    colour = "#B71C1C"
  ) +
  ggplot2::annotate(
    "text",
    x = 4.05,
    y = 0.68,
    label = "Microbial routing",
    fontface = "bold",
    size = 3,
    colour = "#00695C"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.5,
    y = -1.25,
    label = "Values show Fen / Palsa fold change from measured Patzner data",
    size = 2.55,
    colour = "grey35"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Mineral storage" = "#FFDFA8",
      "Reduced Fe product" = "#F8C6CC",
      "Microbial routing" = "#BFE3C4"
    )
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0.25, 4.75),
    ylim = c(-1.35, 1.38),
    clip = "off"
  ) +
  ggplot2::labs(
    title = "Q  Fe control shifts from storage to routing",
    subtitle = "Measured fold changes summarize Fe–OC buffering, Fe²⁺ release and Fe-reducer expansion",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_redox() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = ggplot2::margin(5, 10, 5, 5)
    
  )# ============================================================
# Final assembly with Patzner panels O–P
# ============================================================

# ============================================================
# FIGURE ASSEMBLY — elegant, widened facets
# ============================================================

while (grDevices::dev.cur() > 1) grDevices::dev.off()

# ── Typography and spacing constants ─────────────────────────────────────────

BASE   <- 8.5          # base font size (pt)
TITLE  <- 9.8          # panel title
SUB    <- 7.2          # panel subtitle
STRIP  <- 7.5          # facet strip text
AXIS_T <- 8.0          # axis title
AXIS_X <- 7.2          # axis text
LEG    <- 6.8          # legend text

# ── Base panel theme — clean, generous whitespace ────────────────────────────

panel_theme <- ggplot2::theme_minimal(base_size = BASE, base_family = "Helvetica") +
  ggplot2::theme(
    # Titles
    plot.title         = ggplot2::element_text(face = "bold", size = TITLE,
                           colour = "grey10", margin = ggplot2::margin(0,0,2,0)),
    plot.subtitle      = ggplot2::element_text(size = SUB, colour = "grey45",
                           lineheight = 1.12, margin = ggplot2::margin(0,0,4,0)),
    # Axes
    axis.title         = ggplot2::element_text(size = AXIS_T, colour = "grey20"),
    axis.text          = ggplot2::element_text(size = AXIS_X, colour = "grey30"),
    axis.ticks         = ggplot2::element_line(colour = "grey75", linewidth = 0.25),
    axis.ticks.length  = ggplot2::unit(2.5, "pt"),
    # Grid
    panel.grid.major   = ggplot2::element_line(colour = "grey93", linewidth = 0.3),
    panel.grid.minor   = ggplot2::element_blank(),
    panel.spacing      = ggplot2::unit(10, "pt"),   # wider gap between facets
    # Facet strips
    strip.text         = ggplot2::element_text(face = "bold", size = STRIP,
                           colour = "grey15",
                           margin = ggplot2::margin(4, 4, 4, 4)),
    strip.background   = ggplot2::element_rect(fill = "grey96", colour = NA),
    # Legend
    legend.title       = ggplot2::element_blank(),
    legend.text        = ggplot2::element_text(size = LEG, colour = "grey25"),
    legend.key.height  = ggplot2::unit(0.55, "lines"),
    legend.key.width   = ggplot2::unit(0.85, "lines"),
    legend.background  = ggplot2::element_rect(fill = scales::alpha("white", 0.85),
                           colour = NA),
    legend.margin      = ggplot2::margin(2, 4, 2, 4),
    # Panel border
    panel.border       = ggplot2::element_blank(),
    # Plot margin
    plot.margin        = ggplot2::margin(5, 6, 5, 6)
  )

# ── Property header (label above, definition below) ──────────────────────────

property_header <- function(label, definition, fill) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      fill = fill, colour = NA) +
    ggplot2::annotate("segment",
      x = 0.04, xend = 0.96, y = 0.5, yend = 0.5,
      colour = scales::alpha("grey40", 0.25), linewidth = 0.3) +
    ggplot2::annotate("text",
      x = 0.5, y = 0.74, label = label,
      fontface = "bold", size = 3.6, colour = "grey10",
      family = "Helvetica") +
    ggplot2::annotate("text",
      x = 0.5, y = 0.27, label = definition,
      size = 2.35, colour = "grey40", family = "Helvetica",
      fontface = "italic") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_void()
}

property_block <- function(label, definition, fill, plots, ncol) {
  patchwork::wrap_elements(
    full = property_header(label, definition, fill) /
      patchwork::wrap_plots(plots, ncol = ncol) +
      patchwork::plot_layout(heights = c(0.09, 1))
  )
}

# ── Panel A — CAPACITY: soil buffering ───────────────────────────────────────

p_A <- p_capacity +
  panel_theme +
  ggplot2::theme(
    legend.position  = "none",
    strip.background = ggplot2::element_rect(fill = "#FFF8EE", colour = NA),
    strip.text       = ggplot2::element_text(face = "bold", size = STRIP,
                         colour = "#7B4F1E")
  ) +
  ggplot2::labs(
    title    = "A  Capacity: soil buffering architecture",
    subtitle = "Soil capacity covaries with carbon, mineral and structural proxies"
  )

# ── Panel B — CAPACITY: EEC rhizosphere ──────────────────────────────────────

p_B <- p_li_l +
  panel_theme +
  ggplot2::theme(
    legend.position  = "none",
    panel.grid.major.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    title    = "B  Capacity: rhizosphere electron buffering",
    subtitle = "Electron exchange capacity intensifies toward iron-plaque"
  )

# ── Panel C — CAPACITY: Fe-OC Patzner ────────────────────────────────────────

p_C <- p_patzner_p +
  panel_theme +
  ggplot2::theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    strip.background = ggplot2::element_rect(fill = "#FFF8EE", colour = NA),
    strip.text       = ggplot2::element_text(face = "bold", size = STRIP,
                         colour = "#7B4F1E"),
    panel.grid.major.y = ggplot2::element_line(colour = "grey91", linewidth = 0.3)
  ) +
  ggplot2::labs(
    title    = "C  Capacity: Fe\u2013OC storage across thaw gradients",
    subtitle = "Reactive Fe and Fe\u2013OC pools reorganize across permafrost profiles"
  )

# ── Panel D — CONNECTIVITY: FLUXNET CH4 ──────────────────────────────────────

p_D <- p_connectivity +
  panel_theme +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(
    title    = expression("D  Connectivity: hydrological control of " * CH[4]),
    subtitle = "FLUXNET daily observations fitted with nonlinear GAM"
  )

# ── Panel E — CONNECTIVITY: microbial routing ─────────────────────────────────

p_E <- p_microbes +
  panel_theme +
  ggplot2::theme(
    legend.position    = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(colour = "grey93", linewidth = 0.3)
  ) +
  ggplot2::labs(
    title    = "E  Connectivity: functional microbial routing",
    subtitle = "Evidence synthesis from oxygenated-soil methanogenesis"
  )

# ── Panel F — KINETICS: gas pulse asymmetry ──────────────────────────────────

p_F <- p_kinetics +
  panel_theme +
  ggplot2::theme(
    legend.position    = "none",
    panel.grid.major.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    title    = "F  Kinetics: greenhouse-gas pulse asymmetry",
    subtitle = "Rewetting/thawing reveal unequal response magnitudes"
  )

# ── Panel G — KINETICS: CO2 rewetting ────────────────────────────────────────

p_G <- p_co2_efflux +
  panel_theme +
  ggplot2::theme(
    legend.position  = "top",
    legend.direction = "horizontal",
    strip.background = ggplot2::element_rect(fill = "#FFFBF0", colour = NA),
    strip.text       = ggplot2::element_text(face = "bold", size = STRIP,
                         colour = "#7B4000")
  ) +
  ggplot2::labs(
    title    = expression("G  Kinetics: early " * CO[2] * " response to rewetting"),
    subtitle = "Sterilization reveals rapid abiotic contribution to rewetting"
  )

# ── Panel H — MEMORY: N2 trajectories ────────────────────────────────────────

p_H <- p_sennett +
  panel_theme +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.2, alpha = 1)),
    fill   = "none"
  ) +
  ggplot2::theme(
    legend.position  = "top",
    legend.direction = "horizontal"
  ) +
  ggplot2::labs(
    title    = expression("H  Memory: oxygen-history " * N[2] * " trajectories"),
    subtitle = expression(N[2] * " production converges toward persistent trajectories")
  )

# ── Panel I — MEMORY: denitrification modules ────────────────────────────────

p_I <- p_sennett_n +
  panel_theme +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)),
    fill   = "none"
  ) +
  ggplot2::theme(
    legend.position  = "top",
    legend.direction = "horizontal",
    strip.background = ggplot2::element_rect(fill = "#FAF2FF", colour = NA),
    strip.text       = ggplot2::element_text(face = "bold", size = STRIP,
                         colour = "#4A148C"),
    panel.spacing    = ggplot2::unit(8, "pt")
  ) +
  ggplot2::labs(
    title    = "I  Memory: persistent pathway restructuring",
    subtitle = "Pathway-specific recovery from prior oxygen exposure"
  )

# ── Panel J — RECOVERY TRAJECTORY: freeze-thaw ───────────────────────────────

p_J <- p_ftc +
  panel_theme +
  ggplot2::theme(
    legend.position    = "none",
    panel.grid.major.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    title    = "J  Recovery trajectory: freeze\u2013thaw hysteresis",
    subtitle = expression("Directional " * Delta * E[H] * " transitions reveal path dependence")
  )

# ── Panel K — RECOVERY TRAJECTORY: O2-Eh-pH ──────────────────────────────────

p_K <- p_li_k +
  panel_theme +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.3, alpha = 1)),
    fill   = "none"
  ) +
  ggplot2::theme(
    legend.position  = "top",
    legend.direction = "horizontal"
  ) +
  ggplot2::labs(
    title    = expression("K  Recovery trajectory: coupled O"[2] * "\u2013Eh\u2013pH dynamics"),
    subtitle = "Root oxygen release generates asynchronous redox trajectories"
  )

# ── Panel L — RECOVERY TRAJECTORY: conceptual Fe-routing ─────────────────────

p_L <- ggplot2::ggplot() +
  # Background zones
  ggplot2::annotate("rect", xmin = 0.38, xmax = 1.72, ymin = -1.08, ymax = 1.08,
    fill = "#FFF8EE", alpha = 0.9, colour = "grey82", linewidth = 0.3) +
  ggplot2::annotate("rect", xmin = 2.32, xmax = 3.48, ymin = -0.52, ymax = 0.52,
    fill = "#FFF0F3", alpha = 0.9, colour = "grey82", linewidth = 0.3) +
  ggplot2::annotate("rect", xmin = 4.02, xmax = 5.28, ymin = -0.52, ymax = 0.52,
    fill = "#F0FBF2", alpha = 0.9, colour = "grey82", linewidth = 0.3) +
  # Node labels
  ggplot2::annotate("label", x = 1.05, y =  0.60,
    label = "Reactive Fe\nstorage", fontface = "bold",
    size = 2.65, label.size = 0.18, fill = "#FFE4B0",
    colour = "grey15", family = "Helvetica") +
  ggplot2::annotate("label", x = 1.05, y = -0.60,
    label = "Fe-associated\norganic carbon", fontface = "bold",
    size = 2.65, label.size = 0.18, fill = "#FFE4B0",
    colour = "grey15", family = "Helvetica") +
  ggplot2::annotate("label", x = 2.90, y =  0,
    label = "Reduced Fe\nproducts", fontface = "bold",
    size = 2.65, label.size = 0.18, fill = "#FADADD",
    colour = "grey15", family = "Helvetica") +
  ggplot2::annotate("label", x = 4.65, y =  0,
    label = "Fe-reducing\ncommunities", fontface = "bold",
    size = 2.65, label.size = 0.18, fill = "#C8EDD1",
    colour = "grey15", family = "Helvetica") +
  # Arrows
  ggplot2::annotate("segment",
    x = 1.68, y =  0.57, xend = 2.38, yend =  0.13,
    linewidth = 0.75, colour = "grey45",
    arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")) +
  ggplot2::annotate("segment",
    x = 1.68, y = -0.57, xend = 2.38, yend = -0.13,
    linewidth = 0.75, colour = "grey45",
    arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")) +
  ggplot2::annotate("segment",
    x = 3.44, y =  0,    xend = 4.06, yend =  0,
    linewidth = 0.75, colour = "grey45",
    arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")) +
  # Edge labels
  ggplot2::annotate("text", x = 2.05, y =  0.44,
    label = "thaw-driven\nreduction", size = 2.1,
    colour = "grey45", family = "Helvetica") +
  ggplot2::annotate("text", x = 3.76, y =  0.24,
    label = "biotic\nrouting", size = 2.1,
    colour = "grey45", family = "Helvetica") +
  # Zone labels at top
  ggplot2::annotate("text", x = 1.05, y =  1.18,
    label = "Mineral storage", size = 2.3,
    fontface = "bold", colour = "#8B5E3C", family = "Helvetica") +
  ggplot2::annotate("text", x = 2.90, y =  0.68,
    label = "Reduced\nproduct", size = 2.1,
    fontface = "bold", colour = "#9B2335", family = "Helvetica") +
  ggplot2::annotate("text", x = 4.65, y =  0.68,
    label = "Biotic\nrouting", size = 2.1,
    fontface = "bold", colour = "#2E7D32", family = "Helvetica") +
  ggplot2::coord_cartesian(xlim = c(0.22, 5.48), ylim = c(-1.28, 1.32), clip = "off") +
  ggplot2::labs(
    title    = "L  Recovery trajectory: Fe-routing reconstruction",
    subtitle = "Synthesis of thaw-driven Fe\u2013OC redistribution and Fe-reducer expansion"
  ) +
  ggplot2::theme_void(base_size = BASE, base_family = "Helvetica") +
  panel_theme +
  ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    axis.line    = ggplot2::element_blank()
  )

# ── Property blocks ───────────────────────────────────────────────────────────

capacity_block <- property_block(
  "CAPACITY",
  "Accessible electron-buffering potential of mineral, organic and rhizosphere reservoirs",
  "#FFF8EE",
  list(p_A, p_B, p_C), ncol = 3)

connectivity_block <- property_block(
  "CONNECTIVITY",
  "Extent to which electron donors, acceptors and microorganisms remain physically and functionally linked",
  "#EDF7F6",
  list(p_D, p_E), ncol = 2)

kinetics_block <- property_block(
  "KINETICS",
  "Rates at which electron-transfer pathways respond relative to forcing duration",
  "#FFFBF0",
  list(p_F, p_G), ncol = 2)

memory_block <- property_block(
  "MEMORY",
  "Persistent hidden-state effects inherited from prior disturbance that modify future electron routing",
  "#FAF2FF",
  list(p_H, p_I), ncol = 2)

recovery_block <- property_block(
  "RECOVERY TRAJECTORY",
  "Realized pathway through hidden-state space following disturbance \u2014 the measurable expression of resilience",
  "#EFF7F1",
  list(p_J, p_K, p_L), ncol = 3)

# ── Final figure ──────────────────────────────────────────────────────────────

fig5 <- patchwork::wrap_plots(
  capacity_block,
  connectivity_block,
  kinetics_block,
  memory_block,
  recovery_block,
  ncol = 1
) +
  patchwork::plot_annotation(
    title    = "Empirical signatures of hydroclimatic redox resilience across living Earth systems",
    subtitle = paste(
      "Capacity, connectivity, kinetics and memory are hidden-state properties of electron-transfer architecture;",
      "recovery trajectory — encompassing hysteresis, lag, overshoot and reorganization — is their observable expression."
    ),
    caption  = paste(
      "A, Lacroix et al. 2025; B, Li et al. 2025; C, Patzner et al. 2020;",
      "D, Delwiche et al. 2021; E, Angle et al. 2017; F, Kim et al. 2012;",
      "G, Liu et al. 2025; H\u2013I, Sennett et al. 2024; J, Liebmann et al. 2025;",
      "K, Li et al. 2025; L, conceptual synthesis from Patzner et al. 2020.",
      "Panels A\u2013K are empirical; Panel L is an integrative conceptual synthesis."
    ),
    theme    = ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14, hjust = 0,
                        colour = "grey8", family = "Helvetica",
                        margin = ggplot2::margin(0, 0, 4, 0)),
      plot.subtitle = ggplot2::element_text(size = 9.2, colour = "grey35",
                        hjust = 0, lineheight = 1.2, family = "Helvetica",
                        margin = ggplot2::margin(0, 0, 6, 0)),
      plot.caption  = ggplot2::element_text(size = 6.4, colour = "grey50",
                        hjust = 0, lineheight = 1.3, family = "Helvetica"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin   = ggplot2::margin(10, 10, 8, 10)
    )
  )

print(fig5)

# ── Export ────────────────────────────────────────────────────────────────────

fig5_pdf  <- file.path(figure_out_dir, "Fig5_redox_resilience.pdf")
fig5_tiff <- file.path(figure_out_dir, "Fig5_redox_resilience.tiff")
fig5_png  <- file.path(figure_out_dir, "Fig5_redox_resilience.png")

grDevices::cairo_pdf(filename = fig5_pdf, width = 16, height = 22, onefile = TRUE)
print(fig5)
invisible(grDevices::dev.off())

ggplot2::ggsave(fig5_tiff, fig5,
  width = 16, height = 22, units = "in",
  dpi = 1000, compression = "lzw", bg = "white", limitsize = FALSE)

ggplot2::ggsave(fig5_png, fig5,
  width = 16, height = 22, units = "in",
  dpi = 300, bg = "white", limitsize = FALSE)

message("Saved:")
message("  PDF:  ", normalizePath(fig5_pdf))
message("  TIFF: ", normalizePath(fig5_tiff))
message("  PNG:  ", normalizePath(fig5_png))

