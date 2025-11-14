#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

input_path <- "data_raw/hex_enriquecido.gpkg"
output_dir <- "outputs"

if (!file.exists(input_path)) {
  stop("No se encontró el archivo: ", input_path)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

message("Leyendo hexágonos enriquecidos...")
hex_enriched <- st_read(input_path, quiet = TRUE)

message("Calculando métricas derivadas...")
hex_area_m2 <- st_area(st_transform(hex_enriched, 32616))
hex_enriched$area_km2 <- as.numeric(hex_area_m2) / 1e6

hex_demographics <- hex_enriched |>
  select(
    H3HASH,
    area_km2,
    poblacion_total,
    total_hogares,
    nse_score,
    nse_percentil,
    nse_categoria_modal,
    escolaridad_promedio,
    pct_seguridad_social,
    pct_educacion_superior,
    pct_sin_escolaridad,
    pct_analfabetismo,
    geometry = geom
  ) |>
  mutate(
    pop_density_km2 = if_else(area_km2 > 0, poblacion_total / area_km2, NA_real_),
    households_density = if_else(area_km2 > 0, total_hogares / area_km2, NA_real_)
  )

hex_environment <- hex_enriched |>
  select(
    H3HASH,
    promedio_veg,
    min_veg,
    max_veg,
    prom_temp,
    ndvi_secas_mean,
    ndvi_nortes_mean,
    ndvi_lluvias_mean,
    geometry = geom
  ) |>
  mutate(
    ndvi_amplitude = ndvi_lluvias_mean - ndvi_secas_mean,
    vegetation_category = case_when(
      promedio_veg >= 0.6 ~ "Muy alta",
      promedio_veg >= 0.4 ~ "Alta",
      promedio_veg >= 0.2 ~ "Media",
      !is.na(promedio_veg) ~ "Baja",
      TRUE ~ "Sin datos"
    )
  )

hex_mobility <- hex_enriched |>
  select(
    H3HASH,
    num_paradas,
    num_rutas,
    rutas,
    rutas_frecuencias,
    promedio_veg,
    geometry = geom
  ) |>
  mutate(
    routes_per_stop = if_else(num_paradas > 0, num_rutas / num_paradas, NA_real_),
    transit_category = case_when(
      num_paradas >= 20 ~ "Alta",
      num_paradas >= 10 ~ "Media",
      num_paradas > 0 ~ "Baja",
      TRUE ~ "Sin datos"
    )
  )

write_geojson <- function(data, filename) {
  path <- file.path(output_dir, filename)
  st_write(data, path, delete_dsn = TRUE, quiet = TRUE)
  message("Archivo escrito: ", path)
}

message("Guardando resultados en ", output_dir, "/")
write_geojson(hex_demographics, "23_hex_demographics.geojson")
write_geojson(hex_environment, "24_hex_environment.geojson")
write_geojson(hex_mobility, "25_hex_mobility.geojson")

message("Procesamiento completado.")
