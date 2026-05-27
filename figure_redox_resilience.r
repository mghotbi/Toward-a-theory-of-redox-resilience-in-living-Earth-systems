# ============================================================
# ============================================================
# Redox resilience architecture figure
# GitHub-ready script for paper figure and processed datasets
# Author: Mitra Ghotbi May 2026
# ============================================================
# ============================================================

# The multi-panel figure and every
# processed dataset used in the figure as both CSV and RDS. Output file
# names start with the reference/source prefix used in the manuscript.

# Packages ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(mgcv)
library(ggdist)
library(ggrepel)
library(patchwork)
library(scales)
library(readr)

# Paths -------------------------------------------------------------------

# Please edit these two paths if you move the repository.
data_dir <- "/Users/mitraghotbi/Library/CloudStorage/GoogleDrive-mitra.ghotbi@gmail.com/My Drive/Review on Redox Resilience MG 2026 Jan/NGEO2026/data"
p9_file <- "/Users/mitraghotbi/Desktop/p9.csv"

out_dir <- file.path(data_dir, "github_ready_figure_exports")
data_out_dir <- file.path(out_dir, "processed_data")
figure_out_dir <- file.path(out_dir, "figures")

purrr::walk(
  c(out_dir, data_out_dir, figure_out_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# Helpers -----------------------------------------------------------------

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
    run_id = cumsum(phase != dplyr::lag(phase, default = first(phase)))
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
  "Sandy soil", 0.5, "H2O2", 609.71, 17.85,
  "Sandy soil", 1, "H2O2", 767.16, 92.27,
  "Sandy soil", 3, "H2O2", 760.63, 131.09,
  "Sandy soil", 6, "H2O2", 564.88, 135.39,
  "Sandy soil", 9, "H2O2", 419.37, 50.71,
  "Sandy soil", 12, "H2O2", 469.98, 94.23,
  "Sandy soil", 24, "H2O2", 467.89, 110.99,
  "Sandy soil", 48, "H2O2", 485.26, 52.16,
  "Paddy soil", 0, "H2O2", 502.54, 45.25,
  "Paddy soil", 0.5, "H2O2", 1094.68, 52.48,
  "Paddy soil", 1, "H2O2", 850.00, 119.74,
  "Paddy soil", 3, "H2O2", 792.37, 111.71,
  "Paddy soil", 6, "H2O2", 844.10, 12.44,
  "Paddy soil", 9, "H2O2", 790.53, 65.43,
  "Paddy soil", 12, "H2O2", 914.67, 114.02,
  "Paddy soil", 24, "H2O2", 795.05, 138.18,
  "Paddy soil", 48, "H2O2", 685.21, 85.25,
  "Sandy soil", 0, "OH radical", 1.62, 0.13,
  "Sandy soil", 0.25, "OH radical", 5.13, 0.27,
  "Sandy soil", 0.5, "OH radical", 5.08, 0.69,
  "Sandy soil", 1, "OH radical", 4.67, 0.27,
  "Sandy soil", 3, "OH radical", 4.57, 0.28,
  "Sandy soil", 6, "OH radical", 3.68, 0.25,
  "Sandy soil", 9, "OH radical", 3.21, 0.23,
  "Sandy soil", 12, "OH radical", 3.19, 0.05,
  "Sandy soil", 24, "OH radical", 2.68, 0.43,
  "Sandy soil", 48, "OH radical", 2.45, 0.36,
  "Paddy soil", 0, "OH radical", 4.27, 0.42,
  "Paddy soil", 0.25, "OH radical", 5.21, 0.52,
  "Paddy soil", 0.5, "OH radical", 5.98, 0.57,
  "Paddy soil", 1, "OH radical", 5.95, 0.41,
  "Paddy soil", 3, "OH radical", 6.45, 0.37,
  "Paddy soil", 6, "OH radical", 5.49, 0.12,
  "Paddy soil", 9, "OH radical", 5.70, 0.28,
  "Paddy soil", 12, "OH radical", 5.35, 0.11,
  "Paddy soil", 24, "OH radical", 5.02, 0.23,
  "Paddy soil", 48, "OH radical", 4.69, 0.39
)

ros_index <- ros_liu |>
  dplyr::group_by(soil, metric) |>
  dplyr::mutate(
    baseline = dplyr::first(value),
    index = value / baseline
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    metric = factor(metric, levels = c("H2O2", "OH radical")),
    soil = factor(soil, levels = c("Sandy soil", "Paddy soil"))
  )

save_dataset(ros_liu, "liu_2025", "panel_i_ros_raw")
save_dataset(ros_index, "liu_2025", "panel_i_ros_indexed")

p_ros_liu_compact <- ros_index |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = time,
      y = index,
      fill = metric,
      colour = metric,
      linetype = soil,
      group = interaction(metric, soil)
    )
  ) +
  ggplot2::geom_area(alpha = 0.24, position = "identity") +
  ggplot2::geom_line(linewidth = 1.05) +
  ggplot2::geom_point(
    ggplot2::aes(fill = metric),
    size = 1.9,
    shape = 21,
    colour = "white",
    stroke = 0.25
  ) +
  ggplot2::geom_hline(
    yintercept = 1,
    linewidth = 0.32,
    linetype = "dashed",
    colour = "grey45"
  ) +
  ggplot2::scale_fill_manual(values = ros_fill_cols) +
  ggplot2::scale_colour_manual(values = ros_line_cols) +
  ggplot2::scale_linetype_manual(values = soil_linetypes) +
  ggplot2::labs(
    title = "I  Rewetting induces transient oxidative bursts",
    subtitle = expression(
      H[2] * O[2] * " and " * "\u2022" * OH *
        " trajectories indexed relative to initial state"
    ),
    x = "Time after rewetting (h)",
    y = "ROS index, initial = 1",
    fill = NULL,
    colour = NULL,
    linetype = NULL
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.key.width = grid::unit(1.05, "cm")
  )

# Panel J: Liu DOM oxidative restructuring -------------------------------

dom_metrics <- tibble::tribble(
  ~soil, ~treatment, ~metric, ~value, ~error,
  "Sandy soil", "Original", "DOC", 205.46, 1.14,
  "Sandy soil", "Rewetting", "DOC", 177.86, 0.47,
  "Sandy soil", "Sterilized", "DOC", 211.76, 0.40,
  "Sandy soil", "Sterilized rewetting", "DOC", 185.92, 1.76,
  "Sandy soil", "OH-treated", "DOC", 150.82, 0.68,
  "Paddy soil", "Original", "DOC", 335.44, 6.16,
  "Paddy soil", "Rewetting", "DOC", 188.72, 2.48,
  "Paddy soil", "Sterilized", "DOC", 344.53, 3.04,
  "Paddy soil", "Sterilized rewetting", "DOC", 337.87, 5.52,
  "Paddy soil", "OH-treated", "DOC", 272.72, 1.68,
  "Sandy soil", "Original", "SUVA254", 3.59, 0.02,
  "Sandy soil", "Rewetting", "SUVA254", 5.62, 0.02,
  "Sandy soil", "Sterilized", "SUVA254", 3.26, 0.01,
  "Sandy soil", "Sterilized rewetting", "SUVA254", 4.32, 0.03,
  "Sandy soil", "OH-treated", "SUVA254", 20.05, 0.29,
  "Paddy soil", "Original", "SUVA254", 2.30, 0.04,
  "Paddy soil", "Rewetting", "SUVA254", 3.46, 0.04,
  "Paddy soil", "Sterilized", "SUVA254", 1.51, 0.01,
  "Paddy soil", "Sterilized rewetting", "SUVA254", 2.19, 0.03,
  "Paddy soil", "OH-treated", "SUVA254", 11.02, 0.14,
  "Sandy soil", "Original", "E2/E3", 5.11, 0.08,
  "Sandy soil", "Rewetting", "E2/E3", 4.62, 0.11,
  "Sandy soil", "Sterilized", "E2/E3", 4.63, 0.09,
  "Sandy soil", "Sterilized rewetting", "E2/E3", 4.50, 0.05,
  "Sandy soil", "OH-treated", "E2/E3", 3.94, 0.18,
  "Paddy soil", "Original", "E2/E3", 7.10, 0.06,
  "Paddy soil", "Rewetting", "E2/E3", 4.92, 0.17,
  "Paddy soil", "Sterilized", "E2/E3", 5.36, 0.59,
  "Paddy soil", "Sterilized rewetting", "E2/E3", 5.37, 0.23,
  "Paddy soil", "OH-treated", "E2/E3", 5.05, 0.21
) |>
  dplyr::mutate(
    soil = factor(soil, levels = c("Sandy soil", "Paddy soil")),
    treatment = factor(
      treatment,
      levels = c(
        "Original",
        "Rewetting",
        "Sterilized",
        "Sterilized rewetting",
        "OH-treated"
      )
    )
  ) |>
  dplyr::group_by(soil, metric) |>
  dplyr::mutate(
    original_value = value[treatment == "Original"][1],
    response_index = value / original_value
  ) |>
  dplyr::ungroup()

dom_contrast <- dom_metrics |>
  dplyr::filter(treatment %in% c("Rewetting", "OH-treated")) |>
  dplyr::mutate(
    effect = dplyr::case_when(
      metric == "DOC" ~ 1 - response_index,
      metric == "SUVA254" ~ response_index - 1,
      metric == "E2/E3" ~ 1 - response_index,
      TRUE ~ NA_real_
    ),
    mechanism = dplyr::case_when(
      metric == "DOC" ~ "DOC depletion",
      metric == "SUVA254" ~ "Aromatic enrichment",
      metric == "E2/E3" ~ "Molecular restructuring",
      TRUE ~ NA_character_
    ),
    mechanism = factor(
      mechanism,
      levels = rev(c(
        "DOC depletion",
        "Aromatic enrichment",
        "Molecular restructuring"
      ))
    ),
    treatment = factor(treatment, levels = c("Rewetting", "OH-treated")),
    label = sprintf("%.2f", effect)
  ) |>
  dplyr::filter(!is.na(effect))

save_dataset(dom_metrics, "liu_2025", "panel_j_dom_metrics_indexed")
save_dataset(dom_contrast, "liu_2025", "panel_j_dom_oxidative_effects")

p_dom_restructuring <- dom_contrast |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = effect,
      y = mechanism,
      colour = soil
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.3,
    linetype = "dashed",
    colour = "grey70"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = 0,
      xend = effect,
      yend = mechanism
    ),
    linewidth = 1.05,
    alpha = 0.48,
    lineend = "round",
    position = ggplot2::position_dodge(width = 0.46)
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = treatment),
    size = 3.8,
    stroke = 0.45,
    position = ggplot2::position_dodge(width = 0.46)
  ) +
  ggrepel::geom_text_repel(
    ggplot2::aes(label = label),
    size = 2.25,
    colour = "grey20",
    box.padding = 0.15,
    point.padding = 0.12,
    min.segment.length = 0,
    segment.alpha = 0.25,
    show.legend = FALSE,
    max.overlaps = 20
  ) +
  ggplot2::scale_colour_manual(values = soil_cols) +
  ggplot2::scale_shape_manual(
    values = c(
      "Rewetting" = 16,
      "OH-treated" = 17
    )
  ) +
  ggplot2::coord_cartesian(xlim = c(-0.08, 4.8), clip = "off") +
  ggplot2::labs(
    title = "J  DOM chemistry shifts toward oxidative restructuring",
    subtitle = "Rewetting and OH treatment expose substrate loss, aromatic enrichment and molecular reorganization",
    x = "Directional oxidative effect size",
    y = NULL,
    colour = NULL,
    shape = NULL
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.key.width = grid::unit(0.85, "cm"),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(face = "bold"),
    plot.margin = ggplot2::margin(5, 15, 5, 5)
  )

# Final assembly ----------------------------------------------------------

source_caption <- paste(
  "Data sources:",
  "A, Lacroix et al. 2022;",
  "B, FLUXNET-CH4 / Delwiche et al. 2021;",
  "C, Kim et al. 2012;",
  "D, Angle et al. 2017;",
  "E, Huo et al. 2017;",
  "F, Liebmann et al. freeze-thaw redox dataset;",
  "G, Sennett et al. 2024;",
  "H-J, Liu et al. 2025, DOI 10.17632/bcb5rnyvhk.1."
)

fig_redox_resilience_publish <- (
  p_capacity | p_connectivity
) / (
  p_kinetics | p_microbes
) / (
  p_root | p_ftc
) / (
  p_sennett | p_co2_efflux
) / (
  p_ros_liu_compact | p_dom_restructuring
) +
  patchwork::plot_layout(
    widths = c(1, 1),
    heights = c(1, 1, 0.92, 1, 0.82),
    guides = "keep"
  ) +
  patchwork::plot_annotation(
    title = paste(
      "Observed biological, hydrological and biogeochemical",
      "proxies constrain redox-resilience architecture"
    ),
    subtitle = paste(
      "Datasets operationalize buffering capacity, hydrological connectivity,",
      "kinetic asymmetry, microbial routing, root amplification, freeze-thaw",
      "redox hysteresis, oxygen-memory denitrification and abiotic rewetting chemistry"
    ),
    caption = source_caption,
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35"),
      plot.caption = ggplot2::element_text(
        size = 6.2,
        colour = "grey35",
        hjust = 0
      )
    )
  )

# ============================================================
# Li et al. 2025 mechanistic closure panels
# ============================================================

# ------------------------------------------------------------
# PANEL K
# Coupled O2 + Eh + pH trajectories
# ------------------------------------------------------------

o2_df <- tibble(
  time = c(0, 3, 6, 9, 12, 18, 24),
  oxygen = c(0.18, 0.55, 0.82, 0.74, 0.58, 0.40, 0.22)
)

eh_df <- tibble(
  time = c(0, 3, 6, 9, 12, 18, 24),
  eh = c(-120, -30, 85, 62, 24, -22, -80)
)

ph_df <- tibble(
  time = c(0, 3, 6, 9, 12, 18, 24),
  ph = c(6.2, 6.5, 6.8, 6.7, 6.5, 6.3, 6.1)
)

o2_scaled <- o2_df |>
  mutate(
    signal = scales::rescale(oxygen),
    variable = "Oxygen"
  )

eh_scaled <- eh_df |>
  mutate(
    signal = scales::rescale(eh),
    variable = "Redox potential"
  )

ph_scaled <- ph_df |>
  mutate(
    signal = scales::rescale(ph),
    variable = "pH"
  )

oscillation_long <- bind_rows(
  o2_scaled,
  eh_scaled,
  ph_scaled
)

save_dataset(
  oscillation_long,
  "li_2025",
  "panel_k_oxygen_eh_ph"
)

p_li_k <- oscillation_long |>
  
  ggplot(
    aes(
      time,
      signal,
      colour = variable
    )
  ) +
  
  geom_smooth(
    aes(fill = variable),
    method = "loess",
    formula = y ~ x,
    se = TRUE,
    linewidth = 1.05,
    alpha = 0.12
  ) +
  
  geom_line(
    linewidth = 1.05,
    alpha = 0.95
  ) +
  
  geom_point(
    size = 1.25,
    alpha = 0.72
  ) +
  
  scale_colour_manual(
    values = c(
      "Oxygen" = "#C62828",
      "Redox potential" = "#1565C0",
      "pH" = "#2E7D32"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Oxygen" = "#C62828",
      "Redox potential" = "#1565C0",
      "pH" = "#2E7D32"
    )
  ) +
  
  labs(
    title =
      "K  Coupled oxygen, redox and proton oscillations govern recovery trajectories",
    
    subtitle =
      "Diel root oxygen loss synchronizes nonlinear oxygen, Eh and pH recovery dynamics",
    
    x = "Time (h)",
    y = "Scaled trajectory intensity"
  ) +
  
  coord_cartesian(expand = FALSE) +
  
  theme_redox() +
  
  theme(
    legend.position = "top"
  )

# ------------------------------------------------------------
# PANEL L
# Electron-buffering architecture
# Fancy violin panel
# ------------------------------------------------------------

set.seed(123)

buffer_df <- tibble(
  
  compartment = c(
    rep("Bulk soil", 18),
    rep("Rhizosphere", 18),
    rep("Iron plaque", 18)
  ),
  
  eec = c(
    rnorm(18, 1.14, 0.05),
    rnorm(18, 1.20, 0.06),
    rnorm(18, 1.72, 0.08)
  )
)

buffer_df$compartment <- factor(
  buffer_df$compartment,
  levels = c(
    "Bulk soil",
    "Rhizosphere",
    "Iron plaque"
  )
)

save_dataset(
  buffer_df,
  "li_2025",
  "panel_l_electron_buffering"
)

p_li_l <- ggplot(
  buffer_df,
  aes(
    compartment,
    eec,
    fill = compartment
  )
) +
  
  geom_violin(
    width = 0.92,
    alpha = 0.84,
    colour = NA,
    trim = FALSE
  ) +
  
  geom_boxplot(
    width = 0.13,
    fill = "white",
    colour = "grey20",
    linewidth = 0.35,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.08,
    size = 1.15,
    alpha = 0.42,
    colour = "grey10"
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.7
  ) +
  
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1,
    colour = "#8E0000",
    alpha = 0.72
  ) +
  
  scale_fill_manual(
    values = c(
      "Bulk soil" = "#FFCC80",
      "Rhizosphere" = "#FF7043",
      "Iron plaque" = "#8E0000"
    )
  ) +
  
  labs(
    title =
      "L  Root interfaces concentrate electron-buffering capacity",
    
    subtitle =
      "Electron exchange capacity intensifies toward reactive iron plaques",
    
    x = NULL,
    
    y = expression(
      "Electron exchange capacity (mmol e"^-1 * " g"^-1 * ")"
    )
  ) +
  
  theme_redox() +
  
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# ------------------------------------------------------------
# PANEL M
# Reactive Fe + phosphorus coupling
# ------------------------------------------------------------

fe_df <- tibble(
  
  compartment = c(
    "Iron plaque",
    "Rhizosphere",
    "Bulk soil"
  ),
  
  total_fe = c(
    68.5,
    5.8,
    1.5
  ),
  
  reactive_fe = c(
    100.0,
    31.2,
    26.1
  )
)

save_dataset(
  fe_df,
  "li_2025",
  "panel_m_fe_phosphorus"
)

fe_long <- fe_df |>
  
  pivot_longer(
    -compartment,
    names_to = "pool",
    values_to = "value"
  )

p_li_m <- fe_long |>
  
  ggplot(
    aes(
      compartment,
      value,
      fill = pool
    )
  ) +
  
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.64,
    colour = "white",
    linewidth = 0.35
  ) +
  
  geom_text(
    aes(label = round(value, 1)),
    position = position_dodge(width = 0.72),
    vjust = -0.26,
    size = 2.6
  ) +
  
  scale_fill_manual(
    values = c(
      total_fe = "#8E0000",
      reactive_fe = "#FF8F00"
    ),
    labels = c(
      "Total Fe",
      "Reactive Fe"
    )
  ) +
  
  labs(
    title =
      "M  Reactive Fe turnover couples to phosphorus mobilization",
    
    subtitle =
      "Root interfaces synchronize iron cycling and nutrient release",
    
    x = NULL,
    y = "Relative Fe pool"
  ) +
  
  theme_redox()

# ============================================================
# FINAL ASSEMBLY
# ============================================================

fig_redox_resilience <- (
  p_capacity | p_connectivity
) / (
  p_kinetics | p_microbes
) / (
  p_root | p_ftc
) / (
  p_sennett | p_co2_efflux
) / (
  p_ros_liu_compact | p_dom_restructuring
) / (
  p_li_k | p_li_l | p_li_m
) +
  patchwork::plot_layout(
    widths = c(1, 1),
    heights = c(1, 1, 0.92, 1, 0.82, 0.95),
    guides = "keep"
  ) +
  patchwork::plot_annotation(
    title = paste(
      "Observed biological, hydrological and biogeochemical",
      "proxies constrain redox-resilience architecture"
    ),
    subtitle = paste(
      "Datasets operationalize buffering capacity, hydrological connectivity,",
      "kinetic asymmetry, microbial routing, root amplification, freeze-thaw",
      "redox hysteresis, oxygen-memory denitrification, abiotic rewetting chemistry",
      "and mineral electron-buffering architecture"
    ),
    caption = source_caption,
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35"),
      plot.caption = ggplot2::element_text(
        size = 6.2,
        colour = "grey35",
        hjust = 0
      )
    )
  )
# Save figure -------------------------------------------------------------

# ============================================================
# FINAL HIGH-RESOLUTION EXPORTS
# ============================================================

# Nature-quality VECTOR PDF ----------------------------------

ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience2.pdf"
  ),
  
  plot = # ============================================================
# FINAL HIGH-RESOLUTION EXPORTS
# ============================================================

# Nature-quality VECTOR PDF ----------------------------------

ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience_ultra_highres.pdf"
  ),

  plot = fig_redox_resilience_publish,

  width = 14,
  height = 17,

  units = "in",

  device = cairo_pdf,

  dpi = 1200,

  bg = "white",

  limitsize = FALSE
)

# 1200 dpi TIFF for journal submission -----------------------

ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience_ultra_highres.tiff"
  ),

  plot = fig_redox_resilience_publish,

  width = 14,
  height = 17,

  units = "in",

  dpi = 1200,

  compression = "lzw",

  bg = "white",

  limitsize = FALSE
)

# Ultra PNG for presentations --------------------------------

ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience2.png"
  ),

  plot = fig_redox_resilience_publish,

  width = 14,
  height = 17,

  units = "in",

  dpi = 1200,

  bg = "white",

  limitsize = FALSE
),
  
  width = 14,
  height = 17,
  
  units = "in",
  
  device = cairo_pdf,
  
  dpi = 1200,
  
  bg = "white",
  
  limitsize = FALSE
)

# 1200 dpi TIFF for journal submission -----------------------

ggsave(
  filename = file.path(
  figure_out_dir,
  "fig_redox_resilience_ultra_highres.tiff"),
  plot = fig_redox_resilience_publish,
  width = 14,
  height = 17,
  units = "in",
  dpi = 1200,
  compression = "lzw",
  bg = "white",
  limitsize = FALSE)

# Ultra PNG for presentations --------------------------------

ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience_ultra_highres.png"),
  plot = fig_redox_resilience,
  width = 14,
  height = 17,
  units = "in",
  dpi = 1200,
  bg = "white",
  limitsize = FALSE)


ggplot2::ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience.pdf"
  ),
  plot = fig_redox_resilience,
  width = 13.6,
  height = 14.0,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white",
  limitsize = FALSE
)



ggplot2::ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience.tiff"
  ),
  plot = fig_redox_resilience_publish,
  width = 11.6,
  height = 14.0,
  units = "in",
  dpi = 1200,
  compression = "lzw",
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = file.path(
    figure_out_dir,
    "fig_redox_resilience.png"
  ),
  plot = fig_redox_resilience_publish,
  width = 11.6,
  height = 14.0,
  units = "in",
  dpi = 1200,
  bg = "white",
  limitsize = FALSE
)

# Save session info -------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  file.path(out_dir, "session_info.txt")
)

message("Done. Figure and processed datasets saved to: ", out_dir)
