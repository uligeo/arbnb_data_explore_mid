# RESUMEN DE  GENERADOS

## Estadísticas Generales
- **Total de archivos GeoJSON:** 22
- **Dataset origen:** merida_airbnb_nov2025.geojson
- **Total de propiedades:** 5,321

## Listado de Subproductos

1. **01_listings_core.geojson** (3.8 MB)
   - Datos principales de todas las propiedades
   - Campos: room_id, categorías, precios, calificaciones, coordenadas

2. **02_price_details.geojson** (4.9 MB)
   - Detalles de estructura de precios
   - Campos: montos unitarios, totales, calificadores, monedas

3. **03_price_breakdown.geojson** (6.6 MB)
   - Desglose completo de costos por concepto
   - Campos: descripción, monto, moneda, orden

4. **04_structured_messages.geojson** (21 MB)
   - Mensajes estructurados (dormitorios, camas)
   - Campos: tipo de mensaje, cuerpo, sección

5. **05_image_urls.geojson** (30 MB)
   - URLs de todas las imágenes de propiedades
   - Total: ~31,556 imágenes
   - Promedio: 5.9 imágenes por propiedad

6. **06_passport_profiles.geojson** (1.7 MB)
   - Perfiles completos de anfitriones
   - 811 registros con datos de host
   - Campos: nombre, verificación, superhost, años hosting

#### A. Análisis de Distinciones y Políticas

7. **07_property_badges.geojson** (1.4 MB)
   - Insignias de propiedades
   - Tipos: TOP_X_GUEST_FAVORITE, SUPERHOST, GUEST_FAVORITE

8. **08_payment_policies.geojson** (3.1 MB)
   - Políticas de pago y cancelación
   - 3,379 propiedades con políticas
   - Incluye: FREE_CANCELLATION_HIGHLIGHT

9. **09_long_stay_discounts.geojson**
   - Descuentos por estancia larga
   - (vacío en este dataset)

10. **10_airbnb_service_fees.geojson**
    - Tarifas de servicio de Airbnb
    - (vacío en este dataset)

11. **11_cleaning_fees.geojson**
    - Tarifas de limpieza
    - (vacío en este dataset)

#### B. Análisis Geográfico

12. **12_geographic_quadrants.geojson** (13 KB)
    - Análisis agregado por 29 cuadrantes
    - Métricas: propiedades, precios promedio, rangos, calificaciones
    - Incluye punto central de cada cuadrante

13. **13_property_locations_enhanced.geojson** (1.7 MB)
    - Ubicaciones enriquecidas con análisis espacial
    - Distancia al centro calculada
    - Clasificación por zonas: Centro, Media, Periférica, Remota

#### C. Análisis Temporal y Estacional

14. **14_booking_windows.geojson** (2.2 KB)
    - 4 ventanas de reserva
    - Análisis de precios por período
    - Métricas: promedio, mediana, min, max

15. **15_seasonal_pricing.geojson** (1.8 MB)
    - Precios por temporada
    - Clasificación: Temporada Alta vs Baja
    - Base: meses 12,1,2,3,7,8 = Alta

16. **16_stay_duration_analysis.geojson** (2.1 MB)
    - Análisis por duración de estadía
    - Ratio de eficiencia de precio
    - Agrupado por número de noches

#### D. Análisis de Calidad y Reputación

17. **17_rating_segments.geojson** (2.1 KB)
    - Segmentación por rangos de calificación
    - Categorías: Excelente (4.8-5.0), Muy Bueno (4.5-4.7), Bueno (4.0-4.4), Regular (<4.0)
    - Incluye promedios de precio por segmento

18. **18_review_analysis.geojson** (2.0 MB)
    - Análisis detallado de reseñas
    - Reseñas por mes (calculado con antigüedad)
    - Categorización de calidad

19. **19_superhost_analysis.geojson** (1.7 MB)
    - Análisis específico de superhosts
    - 811 registros con información de host
    - Filtrado por propiedades con estado de superhost

#### E. Análisis de Producto

20. **20_property_types.geojson** (1.8 MB)
    - Clasificación de tipos de propiedad
    - Basado en: categoría, tipo, título
    - Incluye clasificación consolidada

21. **21_property_capacity.geojson** (4.1 MB)
    - Análisis de capacidad de propiedades
    - Número de dormitorios y camas
    - Ratio de eficiencia por cama

#### F. Análisis Competitivo

22. **22_competitive_analysis.geojson** (2.1 MB)
    - Análisis de posición competitiva
    - Percentiles de precio y calificación
    - Score de valor por dinero
    - Clasificación: Alto valor, Valor medio, Valor bajo

## Características Técnicas

- **Formato:** GeoJSON estándar (RFC 7946)
- **CRS:** EPSG:4326 (WGS84)
- **Estructura:** FeatureCollection con geometrías Point
- **Coordenadas:** Todas las capas incluyen lat/lon
- **Compatibilidad:** QGIS, ArcGIS, Leaflet, Mapbox, etc.

## Tamaño Total
- **Espacio ocupado:** ~75 MB
- **Archivos no vacíos:** 19 de 22

## Uso Recomendado

Estos GeoJSON pueden ser utilizados para:
- Análisis espacial en SIG
- Visualización en mapas web interactivos
- Machine Learning y análisis predictivo
- Dashboards y reportes
- APIs geoespaciales
- Estudios de mercado inmobiliario

