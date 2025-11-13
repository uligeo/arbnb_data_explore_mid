# Airbnb Mérida – Dashboard de Exploración

Este repositorio contiene el dashboard interactivo de Airbnb para Mérida (corte noviembre 2025) elaborado con Quarto. El archivo principal es `dashboard.qmd`, donde se combinan visualizaciones geoespaciales, tablas interactivas y análisis estadísticos para entender la oferta local.

## ¿Qué encontrarás?
- **Resumen ejecutivo**: tarjetas con totales de propiedades, precios y calificaciones promedio.
- **Mapa general**: distribución de propiedades con categorías de precio y ventanas emergentes con detalles clave.
- **Características de propiedades**: gráficos por tipo, tablas enriquecidas con imágenes y capacidad.
- **Precios y calidad**: histogramas, boxplots estacionales, segmentos de rating y conteo de superhosts.
- **Geografía detallada**: cuadrantes con mayor oferta, mapa de calor, zonas por distancia y mapas temáticos (precios, capacidad, reseñas).
- **Competitividad y tiempo**: matriz precio-calidad, distribución de segmentos competitivos, ventanas de reserva y duración de estadías.

## Datos de entrada
El dashboard consume los GeoJSON generados en la carpeta `outputs/`, entre ellos:

| Archivo | Contenido |
| --- | --- |
| `01_listings_core.geojson` | Información base de cada propiedad (precios, ratings, coordenadas). |
| `03_price_breakdown.geojson` | Desglose de precios. |
| `12_geographic_quadrants.geojson` | Cuadrantes geográficos y totales. |
| `13_property_locations_enhanced.geojson` | Clasificación por distancia al centro. |
| `14_booking_windows.geojson` | Ventanas temporales y precios. |
| `15_seasonal_pricing.geojson` | Variación de precios por temporada. |
| `16_stay_duration_analysis.geojson` | Relación duración-precio. |
| `17_rating_segments.geojson` | Segmentos de calificación. |
| `18_review_analysis.geojson` | Análisis de reseñas. |
| `19_superhost_analysis.geojson` | Información de anfitriones. |
| `20_property_types.geojson` | Clasificación de tipos de propiedad. |
| `21_property_capacity.geojson` | Capacidad (dormitorios, camas). |
| `22_competitive_analysis.geojson` | Posicionamiento competitivo. |
| `05_image_urls.geojson`, `07_property_badges.geojson`, `08_payment_policies.geojson` | Recursos adicionales (imágenes, badges, políticas). |

El script `main.R` documenta cómo se generaron estas salidas a partir del GeoJSON bruto `merida_airbnb_nov2025.geojson`.

## Requisitos
- [Quarto](https://quarto.org/) 1.4+.
- R 4.2+ con los paquetes: `sf`, `dplyr`, `mapgl`, `ggplot2`, `plotly`, `tidyr`, `DT`, `scales`, `viridis` (ver bloque `setup` en `dashboard.qmd`).

## Cómo ejecutar el dashboard
1. Instala Quarto y las dependencias de R necesarias.
2. Ubica el proyecto en tu máquina y asegúrate de contar con los archivos `outputs/*.geojson`.
3. Renderiza con:
   ```bash
   quarto render dashboard.qmd --to dashboard
   ```
4. Abre `dashboard.html` en tu navegador para explorar los resultados.

## Personalización
- Ajusta el tema, logo o layout desde el encabezado YAML de `dashboard.qmd`.
- Modifica los filtros o categorías (ej. `precio_cat`, clasificación de reseñas) directamente en los bloques correspondientes.
- Puedes sustituir las fuentes de datos en `outputs/` siempre que mantengan los campos esperados en cada sección.

Si necesitas ayuda para adaptar el dashboard u obtener métricas adicionales, abre un issue o continúa la conversación indicando los cambios deseados.
