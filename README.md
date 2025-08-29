# Malaria Simulation Dashboard

## Overview

This Shiny dashboard presents malaria transmission simulation results for a fictional case study in Switzerland. It enables users—especially from National Malaria Control Programs (NMCPs)—to explore intervention impacts by comparing the Business-As-Usual (BAU) scenario to the National Strategic Plan (NSP) scenario. The dashboard is interactive and supports filtering by age group, year, and scenario.

## Features

- **Age Group Selector** – Filter data by population age group.
- **Time Series Visualization** – Interactive plots of incidence and prevalence rates over time.
- **Effectiveness Plot** – Reduction in prevalence and incidence compared to the BAU scenario.
- **Intervention Map** – Shows deployed intervention combinations per canton (only for NSP).
- **Data Explorer** – Interactive table with filtering, searching, and export functionality.

## Project Structure

- `app.R`: Main Shiny application file.
- `input_data.sqlite`: SQLite database containing simulation data.
- `ch_shapefiles/`: Folder containing Swiss canton shapefiles.
- `README.md`: This file.
- `Dev_ex.docx`: Original task description from the organizers.

## Assumptions & Design Decisions

- Only two scenarios (BAU and NSP) are used for plotting and comparison.
- Age group filtering defaults to "All Ages" if unspecified.
- Mapping is only enabled for the NSP scenario and shows interventions deployed per canton.
- Uses `data.table` for fast filtering and aggregation, assuming large datasets in production.
- Shapefile geometries are cleaned using `st_make_valid()` for robustness.
- Color palettes are chosen using `RColorBrewer` with fallbacks for high category counts.
- Max display size for the data table is 5000 rows to prevent UI lag.

## Libraries Used

- **Core Shiny Framework**: `shiny`, `shinydashboard`, `htmltools`
- **Data Manipulation**: `data.table`, `dplyr`
- **Visualization**: `plotly`, `DT`, `RColorBrewer`
- **Geospatial Mapping**: `sf`, `leaflet`
- **Database Connection**: `RSQLite`

## AI Usage

This code was developed with use of ChatGPT for:
- Code structuring and optimization ideas
- Text formatting and function naming
- Writing helper functions (e.g., `calculate_reduction()`, `get_safe_colors()`)


