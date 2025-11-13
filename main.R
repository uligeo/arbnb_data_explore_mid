#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  library(fs)
  library(lubridate)
  library(sf)
})

geojson_path <- "merida_airbnb_nov2025.geojson"
stopifnot(file.exists(geojson_path))

sanitize_pythonish_json <- function(text) {
  text |>
    str_replace_all("\\\\xa0", " ") |>
    str_replace_all("None", "null") |>
    str_replace_all("False", "false") |>
    str_replace_all("True", "true")
}

convert_single_to_double_quotes <- function(text) {
  str_replace_all(text, "'([^']*)'", "\"\\1\"")
}

safe_pythonish_parse <- function(text) {
  if (length(text) == 0 || is.na(text) || identical(text, "")) {
    return(NULL)
  }
  cleaned <- text |>
    sanitize_pythonish_json() |>
    convert_single_to_double_quotes()
  tryCatch(fromJSON(cleaned, simplifyVector = FALSE), error = function(e) NULL)
}

normalize_messy_column <- function(column) {
  map(column, function(item) {
    if (is.null(item) || length(item) == 0) {
      return(NULL)
    }
    if (is.list(item) && !is.character(item)) {
      return(item)
    }
    safe_pythonish_parse(item)
  })
}

pluck_chr <- function(x, ..., default = NA_character_) {
  val <- purrr::pluck(x, ..., .default = default)
  if (length(val) == 0) {
    return(default)
  }
  as.character(val)
}

pluck_num <- function(x, ..., default = NA_real_) {
  val <- purrr::pluck(x, ..., .default = default)
  if (length(val) == 0) {
    return(default)
  }
  suppressWarnings(as.numeric(val))
}

extract_structured_messages <- function(structured) {
  if (is.null(structured) || length(structured) == 0) {
    return(tibble())
  }
  sections <- intersect(
    names(structured),
    c("primaryLine", "secondaryLine", "mapPrimaryLine", "mapSecondaryLine", "reviewSnippet", "explanation")
  )
  map_dfr(sections, function(section) {
    entries <- structured[[section]]
    if (is.null(entries) || length(entries) == 0) {
      return(tibble())
    }
    tibble(
      section = section,
      entry = list(entries)
    ) |>
      unnest_longer(entry) |>
      unnest_wider(entry, names_sep = "_") |>
      transmute(
        section,
        message_type = entry_type,
        message_body = entry_body,
        message_headline = entry_headline,
        message_body_label = entry_bodyA11yLabel,
        message_font_weight = entry_fontWeight
      )
  })
}

extract_images <- function(images) {
  if (is.null(images) || length(images) == 0) {
    return(tibble())
  }
  tibble::as_tibble(images) |>
    select(any_of(c("url", "caption"))) |>
    mutate(image_rank = row_number()) |>
    relocate(image_rank)
}

extract_passport <- function(passport) {
  if (is.null(passport) || length(passport) == 0) {
    return(tibble())
  }
  tibble(
    host_name = pluck_chr(passport, "name"),
    host_since_text = pluck_chr(passport, "titleText"),
    host_is_superhost = purrr::pluck(passport, "isSuperhost", .default = NA),
    host_is_verified = purrr::pluck(passport, "isVerified", .default = NA),
    host_rating_average = pluck_num(passport, "ratingAverage"),
    host_rating_count = pluck_num(passport, "ratingCount"),
    host_profile_picture = pluck_chr(passport, "profilePictureUrl"),
    host_thumbnail = pluck_chr(passport, "thumbnailUrl"),
    host_years_hosting = pluck_num(passport, "timeAsHost", "years"),
    host_months_hosting = pluck_num(passport, "timeAsHost", "months")
  )
}

join_coords_as_sf <- function(df, base_coords, crs = 4326) {
  if (nrow(df) == 0) {
    return(NULL)
  }
  df |>
    left_join(base_coords, by = "room_id", relationship = "many-to-many") |>
    filter(!is.na(geometry_lon), !is.na(geometry_lat)) |>
    st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = crs, remove = FALSE)
}

write_geojson <- function(sf_obj, filename) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    return()
  }
  st_write(sf_obj, filename, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
}

raw_geojson <- fromJSON(geojson_path)
features_tbl <- raw_geojson[["features"]]
properties <- as_tibble(features_tbl[["properties"]])
geometry_tbl <- features_tbl[["geometry"]][["coordinates"]] |>
  map_dfr(~tibble(geometry_lon = .x[1], geometry_lat = .x[2]))

listings <- bind_cols(properties, geometry_tbl) |>
  mutate(
    check_in = ymd(check_in),
    check_out = ymd(check_out),
    stay_nights = as.integer(check_out - check_in),
    num_reseñas = parse_integer(as.character(num_reseñas), na = c("", "NA")),
    calificacion = parse_double(as.character(calificacion)),
    precio_total = parse_double(as.character(precio_total))
  )

listings <- listings |>
  mutate(
    structured_parsed = normalize_messy_column(structuredContent),
    price_parsed = normalize_messy_column(price),
    price_clean_parsed = normalize_messy_column(price_clean),
    passport_parsed = normalize_messy_column(passportData),
    rating_value = parse_double(as.character(rating$value)),
    rating_count = parse_double(as.character(rating$reviewCount)),
    rating_clean_value = parse_double(as.character(rating_clean$value)),
    rating_clean_count = parse_double(as.character(rating_clean$reviewCount))
  )

listings_core <- listings |>
  transmute(
    room_id,
    property_category = na_if(category, ""),
    property_kind = na_if(kind, ""),
    listing_type = na_if(type, ""),
    name = na_if(name, ""),
    title = na_if(title, ""),
    cuadrante_busqueda,
    ventana_fecha,
    check_in,
    check_out,
    stay_nights,
    precio_total,
    price_per_night = if_else(!is.na(precio_total) & stay_nights > 0, round(precio_total / stay_nights, 2), NA_real_),
    calificacion,
    num_reseñas,
    rating_value,
    rating_count,
    rating_clean_value,
    rating_clean_count,
    latitude,
    longitude,
    geometry_lat,
    geometry_lon
  )

listings_sf <- st_as_sf(listings_core, coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)
base_coords <- st_drop_geometry(listings_sf) |>
  select(room_id, geometry_lon, geometry_lat)

center_lon <- median(base_coords$geometry_lon, na.rm = TRUE)
center_lat <- median(base_coords$geometry_lat, na.rm = TRUE)
center_point <- st_sfc(st_point(c(center_lon, center_lat)), crs = 4326)

price_details <- listings |>
  transmute(room_id, price_parsed) |>
  mutate(
    unit_amount = map_dbl(price_parsed, ~pluck_num(.x, "unit", "amount")),
    unit_currency = map_chr(price_parsed, ~pluck_chr(.x, "unit", "curency_symbol")),
    unit_qualifier = map_chr(price_parsed, ~pluck_chr(.x, "unit", "qualifier")),
    total_amount = map_dbl(price_parsed, ~pluck_num(.x, "total", "amount")),
    total_currency = map_chr(price_parsed, ~pluck_chr(.x, "total", "currency_symbol"))
  ) |>
  select(-price_parsed)

price_breakdown <- listings |>
  transmute(room_id, price_parsed) |>
  mutate(break_down = map(price_parsed, ~pluck(.x, "break_down", .default = NULL))) |>
  select(-price_parsed) |>
  unnest_longer(break_down, values_to = "entry", keep_empty = FALSE) |>
  unnest_wider(entry) |>
  rename(
    breakdown_description = description,
    breakdown_amount = amount,
    breakdown_currency = currency
  ) |>
  group_by(room_id) |>
  mutate(breakdown_order = row_number()) |>
  ungroup()

structured_messages <- listings |>
  transmute(room_id, structured_parsed) |>
  mutate(messages = map(structured_parsed, extract_structured_messages)) |>
  select(-structured_parsed) |>
  unnest(messages) |>
  group_by(room_id, section) |>
  mutate(message_order = row_number()) |>
  ungroup()

image_urls <- listings |>
  transmute(room_id, images) |>
  mutate(image_tbl = map(images, extract_images)) |>
  select(-images) |>
  unnest(image_tbl)

passport_profiles <- listings |>
  transmute(room_id, passport_parsed) |>
  mutate(passport_tbl = map(passport_parsed, extract_passport)) |>
  select(-passport_parsed) |>
  unnest(passport_tbl)

property_badges <- listings |>
  transmute(room_id, badges) |>
  unnest_longer(badges, values_to = "badge_type", keep_empty = FALSE) |>
  filter(!is.na(badge_type), badge_type != "") |>
  group_by(room_id) |>
  mutate(badge_rank = row_number()) |>
  ungroup()

payment_policies <- listings |>
  transmute(room_id, paymentMessages) |>
  unnest(paymentMessages, keep_empty = FALSE) |>
  rename(
    policy_typename = `__typename`,
    policy_text = text,
    policy_type = type
  ) |>
  filter(!is.na(policy_type))

long_stay_discounts_list <- listings$long_stay_discount
has_discount <- map_lgl(long_stay_discounts_list, ~!is.null(.x) && length(.x) > 0)
long_stay_discounts <- tibble(
  room_id = listings$room_id[has_discount],
  discount_info = "Tiene descuento por estancia larga"
)

airbnb_service_fees_list <- map(listings$fee, ~pluck(.x, "airbnb", .default = list()))
has_fee <- map_lgl(airbnb_service_fees_list, ~length(.x) > 0)
airbnb_service_fees <- tibble(
  room_id = listings$room_id[has_fee],
  fee_info = "Tiene tarifa de servicio Airbnb"
)

cleaning_fees_list <- map(listings$fee, ~pluck(.x, "cleaning", .default = list()))
has_cleaning <- map_lgl(cleaning_fees_list, ~length(.x) > 0)
cleaning_fees <- tibble(
  room_id = listings$room_id[has_cleaning],
  cleaning_fee_info = "Tiene tarifa de limpieza"
)

geographic_quadrants <- listings_core |>
  group_by(cuadrante_busqueda) |>
  summarise(
    total_properties = n(),
    avg_price = mean(precio_total, na.rm = TRUE),
    median_price = median(precio_total, na.rm = TRUE),
    avg_rating = mean(calificacion, na.rm = TRUE),
    price_range_min = min(precio_total, na.rm = TRUE),
    price_range_max = max(precio_total, na.rm = TRUE),
    properties_with_rating = sum(!is.na(calificacion) & calificacion > 0),
    avg_price_per_night = mean(price_per_night, na.rm = TRUE),
    center_lon = mean(geometry_lon, na.rm = TRUE),
    center_lat = mean(geometry_lat, na.rm = TRUE)
  ) |>
  ungroup() |>
  st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326, remove = FALSE)

property_locations_enhanced <- listings_core |>
  mutate(
    distance_to_center = st_distance(
      st_as_sf(listings_core, coords = c("geometry_lon", "geometry_lat"), crs = 4326),
      center_point
    )[,1],
    distance_to_center_km = as.numeric(distance_to_center) / 1000,
    zone_classification = case_when(
      distance_to_center_km < 5 ~ "Centro",
      distance_to_center_km < 10 ~ "Zona Media",
      distance_to_center_km < 20 ~ "Zona Periférica",
      TRUE ~ "Zona Remota"
    )
  ) |>
  select(room_id, geometry_lon, geometry_lat, cuadrante_busqueda,
         distance_to_center_km, zone_classification) |>
  st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)

booking_windows <- listings_core |>
  group_by(ventana_fecha, check_in, check_out) |>
  summarise(
    total_properties = n(),
    avg_price_per_window = mean(precio_total, na.rm = TRUE),
    median_price = median(precio_total, na.rm = TRUE),
    min_price = min(precio_total, na.rm = TRUE),
    max_price = max(precio_total, na.rm = TRUE),
    avg_stay_nights = mean(stay_nights, na.rm = TRUE),
    properties_with_rating = sum(!is.na(calificacion) & calificacion > 0),
    avg_rating = mean(calificacion, na.rm = TRUE),
    center_lon = mean(geometry_lon, na.rm = TRUE),
    center_lat = mean(geometry_lat, na.rm = TRUE),
    .groups = "drop"
  ) |>
  st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326, remove = FALSE)

seasonal_pricing <- listings_core |>
  mutate(
    month = month(check_in),
    season_type = case_when(
      month %in% c(12, 1, 2, 3, 7, 8) ~ "Temporada Alta",
      TRUE ~ "Temporada Baja"
    )
  ) |>
  select(room_id, ventana_fecha, check_in, precio_total, price_per_night,
         season_type, geometry_lon, geometry_lat) |>
  st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)

stay_duration_analysis <- listings_core |>
  filter(!is.na(stay_nights), stay_nights > 0) |>
  group_by(stay_nights) |>
  summarise(
    total_properties = n(),
    avg_price_total = mean(precio_total, na.rm = TRUE),
    avg_price_per_night = mean(price_per_night, na.rm = TRUE),
    price_efficiency_ratio = avg_price_total / (stay_nights * avg_price_per_night),
    median_price_per_night = median(price_per_night, na.rm = TRUE),
    center_lon = mean(geometry_lon, na.rm = TRUE),
    center_lat = mean(geometry_lat, na.rm = TRUE),
    .groups = "drop"
  ) |>
  st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326, remove = FALSE)

rating_segments <- listings_core |>
  mutate(
    rating_range = case_when(
      is.na(calificacion) | calificacion == 0 ~ "Sin calificación",
      calificacion >= 4.8 ~ "Excelente (4.8-5.0)",
      calificacion >= 4.5 ~ "Muy Bueno (4.5-4.7)",
      calificacion >= 4.0 ~ "Bueno (4.0-4.4)",
      TRUE ~ "Regular (<4.0)"
    )
  ) |>
  group_by(rating_range) |>
  summarise(
    total_properties = n(),
    avg_price = mean(precio_total, na.rm = TRUE),
    avg_reviews_count = mean(num_reseñas, na.rm = TRUE),
    median_price = median(precio_total, na.rm = TRUE),
    avg_price_per_night = mean(price_per_night, na.rm = TRUE),
    center_lon = mean(geometry_lon, na.rm = TRUE),
    center_lat = mean(geometry_lat, na.rm = TRUE),
    .groups = "drop"
  ) |>
  st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326, remove = FALSE)

review_analysis <- listings_core |>
  left_join(passport_profiles |> select(room_id, host_years_hosting, host_months_hosting), by = "room_id") |>
  mutate(
    total_months_hosting = (host_years_hosting * 12) + coalesce(host_months_hosting, 0),
    reviews_per_month = if_else(total_months_hosting > 0, num_reseñas / total_months_hosting, NA_real_),
    rating_category = case_when(
      is.na(calificacion) | calificacion == 0 ~ "Sin datos",
      calificacion >= 4.8 ~ "Excelente",
      calificacion >= 4.5 ~ "Muy bueno",
      calificacion >= 4.0 ~ "Bueno",
      TRUE ~ "Regular"
    )
  ) |>
  select(room_id, calificacion, num_reseñas, reviews_per_month, rating_category,
         geometry_lon, geometry_lat) |>
  st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)

superhost_analysis <- passport_profiles |>
  filter(!is.na(host_is_superhost)) |>
  left_join(base_coords, by = "room_id") |>
  filter(!is.na(geometry_lon), !is.na(geometry_lat)) |>
  st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)

property_types <- listings_core |>
  transmute(
    room_id,
    property_category,
    property_kind,
    listing_type,
    title,
    property_classification = case_when(
      !is.na(property_category) & property_category != "" ~ property_category,
      !is.na(property_kind) & property_kind != "" ~ property_kind,
      !is.na(listing_type) & listing_type != "" ~ listing_type,
      TRUE ~ "Sin clasificar"
    ),
    geometry_lon,
    geometry_lat
  ) |>
  st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)

extract_bed_info <- function(structured_parsed) {
  if(is.null(structured_parsed) || length(structured_parsed) == 0) {
    return(list(bedrooms = NA_real_, beds = NA_real_))
  }

  primary_line <- pluck(structured_parsed, "primaryLine", .default = list())
  bedrooms <- NA_real_
  beds <- NA_real_

  for(msg in primary_line) {
    body <- pluck(msg, "body", .default = "")
    if(str_detect(body, "dormitorio")) {
      bedrooms <- parse_number(body)
    }
    if(str_detect(body, "cama") && !str_detect(body, "dormitorio")) {
      beds <- parse_number(body)
    }
  }

  list(bedrooms = bedrooms, beds = beds)
}

property_capacity <- listings |>
  transmute(
    room_id,
    bed_info = map(structured_parsed, extract_bed_info)
  ) |>
  mutate(
    num_bedrooms = map_dbl(bed_info, ~pluck(.x, "bedrooms", .default = NA_real_)),
    num_beds = map_dbl(bed_info, ~pluck(.x, "beds", .default = NA_real_))
  ) |>
  left_join(listings_core |> select(room_id, precio_total, price_per_night, geometry_lon, geometry_lat), by = "room_id") |>
  mutate(
    capacity_efficiency_ratio = if_else(
      !is.na(num_beds) & num_beds > 0 & !is.na(price_per_night),
      price_per_night / num_beds,
      NA_real_
    )
  ) |>
  select(room_id, num_bedrooms, num_beds, capacity_efficiency_ratio, geometry_lon, geometry_lat) |>
  filter(!is.na(geometry_lon), !is.na(geometry_lat)) |>
  st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)

competitive_analysis <- listings_core |>
  mutate(
    price_percentile = percent_rank(precio_total),
    rating_percentile = percent_rank(calificacion),
    value_for_money_score = case_when(
      is.na(calificacion) | calificacion == 0 ~ NA_real_,
      TRUE ~ (rating_percentile / (price_percentile + 0.01)) * 100
    ),
    competitive_position = case_when(
      is.na(value_for_money_score) ~ "Sin datos",
      value_for_money_score >= 75 ~ "Alto valor",
      value_for_money_score >= 40 ~ "Valor medio",
      TRUE ~ "Valor bajo"
    )
  ) |>
  select(room_id, precio_total, price_per_night, calificacion,
         price_percentile, rating_percentile, value_for_money_score,
         competitive_position, geometry_lon, geometry_lat) |>
  st_as_sf(coords = c("geometry_lon", "geometry_lat"), crs = 4326, remove = FALSE)

fs::dir_create("outputs")

price_details_sf <- join_coords_as_sf(price_details, base_coords)
price_breakdown_sf <- join_coords_as_sf(price_breakdown, base_coords)
structured_messages_sf <- join_coords_as_sf(structured_messages, base_coords)
image_urls_sf <- join_coords_as_sf(image_urls, base_coords)
passport_profiles_sf <- join_coords_as_sf(passport_profiles, base_coords)
property_badges_sf <- join_coords_as_sf(property_badges, base_coords)
payment_policies_sf <- join_coords_as_sf(payment_policies, base_coords)

long_stay_discounts_sf <- join_coords_as_sf(long_stay_discounts, base_coords)
airbnb_service_fees_sf <- join_coords_as_sf(airbnb_service_fees, base_coords)
cleaning_fees_sf <- join_coords_as_sf(cleaning_fees, base_coords)

write_geojson(listings_sf, "outputs/01_listings_core.geojson")
write_geojson(price_details_sf, "outputs/02_price_details.geojson")
write_geojson(price_breakdown_sf, "outputs/03_price_breakdown.geojson")
write_geojson(structured_messages_sf, "outputs/04_structured_messages.geojson")
write_geojson(image_urls_sf, "outputs/05_image_urls.geojson")
write_geojson(passport_profiles_sf, "outputs/06_passport_profiles.geojson")
write_geojson(property_badges_sf, "outputs/07_property_badges.geojson")
write_geojson(payment_policies_sf, "outputs/08_payment_policies.geojson")
write_geojson(long_stay_discounts_sf, "outputs/09_long_stay_discounts.geojson")
write_geojson(airbnb_service_fees_sf, "outputs/10_airbnb_service_fees.geojson")
write_geojson(cleaning_fees_sf, "outputs/11_cleaning_fees.geojson")
write_geojson(geographic_quadrants, "outputs/12_geographic_quadrants.geojson")
write_geojson(property_locations_enhanced, "outputs/13_property_locations_enhanced.geojson")
write_geojson(booking_windows, "outputs/14_booking_windows.geojson")
write_geojson(seasonal_pricing, "outputs/15_seasonal_pricing.geojson")
write_geojson(stay_duration_analysis, "outputs/16_stay_duration_analysis.geojson")
write_geojson(rating_segments, "outputs/17_rating_segments.geojson")
write_geojson(review_analysis, "outputs/18_review_analysis.geojson")
write_geojson(superhost_analysis, "outputs/19_superhost_analysis.geojson")
write_geojson(property_types, "outputs/20_property_types.geojson")
write_geojson(property_capacity, "outputs/21_property_capacity.geojson")
write_geojson(competitive_analysis, "outputs/22_competitive_analysis.geojson")

cat("\n========================================\n")
cat("SUBPRODUCTOS GENERADOS EN FORMATO GEOJSON\n")
cat("========================================\n\n")

cat("SUBPRODUCTOS ORIGINALES:\n")
cat(" 01. listings_core.geojson - Datos principales de propiedades\n")
cat(" 02. price_details.geojson - Detalles de precios\n")
cat(" 03. price_breakdown.geojson - Desglose de costos\n")
cat(" 04. structured_messages.geojson - Mensajes estructurados\n")
cat(" 05. image_urls.geojson - URLs de imágenes\n")
cat(" 06. passport_profiles.geojson - Perfiles de anfitriones\n\n")

cat("NUEVOS SUBPRODUCTOS:\n")
cat(" 07. property_badges.geojson - Insignias de propiedades\n")
cat(" 08. payment_policies.geojson - Políticas de pago\n")
cat(" 09. long_stay_discounts.geojson - Descuentos por estancia larga\n")
cat(" 10. airbnb_service_fees.geojson - Tarifas de servicio Airbnb\n")
cat(" 11. cleaning_fees.geojson - Tarifas de limpieza\n")
cat(" 12. geographic_quadrants.geojson - Análisis por cuadrantes\n")
cat(" 13. property_locations_enhanced.geojson - Ubicaciones enriquecidas\n")
cat(" 14. booking_windows.geojson - Ventanas de reserva\n")
cat(" 15. seasonal_pricing.geojson - Precios estacionales\n")
cat(" 16. stay_duration_analysis.geojson - Análisis de duración\n")
cat(" 17. rating_segments.geojson - Segmentación por calificación\n")
cat(" 18. review_analysis.geojson - Análisis de reseñas\n")
cat(" 19. superhost_analysis.geojson - Análisis de superhosts\n")
cat(" 20. property_types.geojson - Tipos de propiedad\n")
cat(" 21. property_capacity.geojson - Análisis de capacidad\n")
cat(" 22. competitive_analysis.geojson - Análisis competitivo\n\n")

cat("Total: 22 archivos GeoJSON generados en outputs/\n")
cat("========================================\n")
