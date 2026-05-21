
# ============================================================
# CONTROL DE CALIDAD DE DATOS METEOROLÓGICOS
# ============================================================

library(tidyverse)
library(lubridate)
library(openxlsx)

# Cargar datos (asumiendo que ya tienes datos_power_limpios)
# Si no, cárgalos desde el archivo guardado

datos <- read.xlsx("datos_limpios.xlsx")

# Ver estructura de los datos
head(datos)
summary(datos)

# ============================================================
# 1. VERIFICAR VALORES FALTANTES
# ============================================================

# Contar cuántos NA hay por variable
sum(is.na(datos$T_media))
sum(is.na(datos$T_max))
sum(is.na(datos$T_min))
sum(is.na(datos$Precipitacion))
sum(is.na(datos$HR))
sum(is.na(datos$Viento))
sum(is.na(datos$Radiacion))

# ============================================================
# 2. VERIFICAR RANGOS PLAUSIBLES  DE ZONAS TROPICALES)
# ============================================================

# Temperatura media (°C) - Rango tropical: 15 a 35
datos$T_media_fuera_rango <- datos$T_media < 15 | datos$T_media > 35
cat("T_media fuera de rango [15, 35]:", sum(datos$T_media_fuera_rango, na.rm = TRUE), "\n")

# Temperatura máxima (°C) - Rango tropical: 20 a 45
datos$T_max_fuera_rango <- datos$T_max < 20 | datos$T_max > 45
cat("T_max fuera de rango [20, 45]:", sum(datos$T_max_fuera_rango, na.rm = TRUE), "\n")

# Temperatura mínima (°C) - Rango tropical: 10 a 28
datos$T_min_fuera_rango <- datos$T_min < 10 | datos$T_min > 28
cat("T_min fuera de rango [10, 28]:", sum(datos$T_min_fuera_rango, na.rm = TRUE), "\n")

# Precipitación (mm/día) - Rango tropical: 0 a 300
datos$Precipitacion_fuera_rango <- datos$Precipitacion < 0 | datos$Precipitacion > 300
cat("Precipitación fuera de rango [0, 300]:", sum(datos$Precipitacion_fuera_rango, na.rm = TRUE), "\n")

# Humedad relativa (%) - Rango tropical: 50 a 100
datos$HR_fuera_rango <- datos$HR < 50 | datos$HR > 100
cat("HR fuera de rango [50, 100]:", sum(datos$HR_fuera_rango, na.rm = TRUE), "\n")

# Viento (m/s) - Rango tropical: 0 a 30
datos$Viento_fuera_rango <- datos$Viento < 0 | datos$Viento > 30
cat("Viento fuera de rango [0, 30]:", sum(datos$Viento_fuera_rango, na.rm = TRUE), "\n")

# Radiación (W/m²) - Rango tropical: 100 a 350
datos$Radiacion_fuera_rango <- datos$Radiacion < 100 | datos$Radiacion > 350
cat("Radiación fuera de rango [100, 350]:", sum(datos$Radiacion_fuera_rango, na.rm = TRUE), "\n")

# ============================================================
# 3. VERIFICAR COHERENCIA ENTRE VARIABLES
# ============================================================

# T_max debe ser >= T_min
datos$T_max_menor_T_min <- datos$T_max < datos$T_min
cat("Registros con T_max < T_min:", sum(datos$T_max_menor_T_min, na.rm = TRUE), "\n")

# Opcional: Verificar que T_min no sea mayor que T_media
datos$T_min_mayor_T_media <- datos$T_min > datos$T_media
cat("Registros con T_min > T_media:", sum(datos$T_min_mayor_T_media, na.rm = TRUE), "\n")

# Opcional: Verificar que T_max no sea menor que T_media
datos$T_max_menor_T_media <- datos$T_max < datos$T_media
cat("Registros con T_max < T_media:", sum(datos$T_max_menor_T_media, na.rm = TRUE), "\n")

# ============================================================
# 4. DETECTAR OUTLIERS CON IQR
# ============================================================

# Calcular IQR para T_media
Q1 <- quantile(datos$T_media, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$T_media, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
limite_inferior <- Q1 - 3 * IQR
limite_superior <- Q3 + 3 * IQR
datos$T_media_outlier <- datos$T_media < limite_inferior | datos$T_media > limite_superior
cat("T_media - Outliers:", sum(datos$T_media_outlier, na.rm = TRUE), "\n")

# Calcular IQR para HR
Q1 <- quantile(datos$HR, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$HR, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
limite_inferior <- Q1 - 3 * IQR
limite_superior <- Q3 + 3 * IQR
datos$HR_outlier <- datos$HR < limite_inferior | datos$HR > limite_superior
cat("HR - Outliers:", sum(datos$HR_outlier, na.rm = TRUE), "\n")

# Calcular IQR para Viento
Q1 <- quantile(datos$Viento, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$Viento, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
limite_inferior <- Q1 - 3 * IQR
limite_superior <- Q3 + 3 * IQR
datos$Viento_outlier <- datos$Viento < limite_inferior | datos$Viento > limite_superior
cat("Viento - Outliers:", sum(datos$Viento_outlier, na.rm = TRUE), "\n")

# Calcular IQR para Radiacion
Q1 <- quantile(datos$Radiacion, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$Radiacion, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
limite_inferior <- Q1 - 3 * IQR
limite_superior <- Q3 + 3 * IQR
datos$Radiacion_outlier <- datos$Radiacion < limite_inferior | datos$Radiacion > limite_superior
cat("Radiación - Outliers:", sum(datos$Radiacion_outlier, na.rm = TRUE), "\n")

# ============================================================
# 5. CREAR DATOS LIMPIOS (REEMPLAZAR OUTLIERS CON NA)
# ============================================================

# Copiar datos originales
datos_limpios <- datos

# Reemplazar outliers con NA
datos_limpios$T_media[datos_limpios$T_media_outlier == TRUE] <- NA
datos_limpios$HR[datos_limpios$HR_outlier == TRUE] <- NA
datos_limpios$Viento[datos_limpios$Viento_outlier == TRUE] <- NA
datos_limpios$Radiacion[datos_limpios$Radiacion_outlier == TRUE] <- NA

# Reemplazar valores fuera de rango con NA
datos_limpios$T_media[datos_limpios$T_media_fuera_rango == TRUE] <- NA
datos_limpios$T_max[datos_limpios$T_max_fuera_rango == TRUE] <- NA
datos_limpios$T_min[datos_limpios$T_min_fuera_rango == TRUE] <- NA
datos_limpios$Precipitacion[datos_limpios$Precipitacion_fuera_rango == TRUE] <- NA
datos_limpios$HR[datos_limpios$HR_fuera_rango == TRUE] <- NA
datos_limpios$Viento[datos_limpios$Viento_fuera_rango == TRUE] <- NA
datos_limpios$Radiacion[datos_limpios$Radiacion_fuera_rango == TRUE] <- NA

# Reemplazar incoherencias con NA
datos_limpios$T_max[datos_limpios$T_max_menor_T_min == TRUE] <- NA
datos_limpios$T_min[datos_limpios$T_max_menor_T_min == TRUE] <- NA

# Opcional: Reemplazar incoherencias adicionales
datos_limpios$T_min[datos_limpios$T_min_mayor_T_media == TRUE] <- NA
datos_limpios$T_media[datos_limpios$T_min_mayor_T_media == TRUE] <- NA

datos_limpios$T_max[datos_limpios$T_max_menor_T_media == TRUE] <- NA
datos_limpios$T_media[datos_limpios$T_max_menor_T_media == TRUE] <- NA

cat("Datos limpios generados\n")
cat("Registros totales:", nrow(datos_limpios), "\n")

# ============================================================
# 6. RESUMEN FINAL
# ============================================================

# Totales de valores eliminados
cat("\nValores eliminados (convertidos a NA):\n")
cat("- T_media:", sum(is.na(datos_limpios$T_media) & !is.na(datos$T_media)), "\n")
cat("- T_max:", sum(is.na(datos_limpios$T_max) & !is.na(datos$T_max)), "\n")
cat("- T_min:", sum(is.na(datos_limpios$T_min) & !is.na(datos$T_min)), "\n")
cat("- Precipitacion:", sum(is.na(datos_limpios$Precipitacion) & !is.na(datos$Precipitacion)), "\n")
cat("- HR:", sum(is.na(datos_limpios$HR) & !is.na(datos$HR)), "\n")
cat("- Viento:", sum(is.na(datos_limpios$Viento) & !is.na(datos$Viento)), "\n")
cat("- Radiacion:", sum(is.na(datos_limpios$Radiacion) & !is.na(datos$Radiacion)), "\n")

# Comparar resumen antes y después
cat("\n=== TEMPERATURA MEDIA ===\n")
cat("Antes - media:", mean(datos$T_media, na.rm = TRUE), "| sd:", sd(datos$T_media, na.rm = TRUE), "\n")
cat("Después - media:", mean(datos_limpios$T_media, na.rm = TRUE), "| sd:", sd(datos_limpios$T_media, na.rm = TRUE), "\n")

cat("\n=== TEMPERATURA MÁXIMA ===\n")
cat("Antes - media:", mean(datos$T_max, na.rm = TRUE), "| max:", max(datos$T_max, na.rm = TRUE), "\n")
cat("Después - media:", mean(datos_limpios$T_max, na.rm = TRUE), "| max:", max(datos_limpios$T_max, na.rm = TRUE), "\n")

cat("\n=== TEMPERATURA MÍNIMA ===\n")
cat("Antes - media:", mean(datos$T_min, na.rm = TRUE), "| min:", min(datos$T_min, na.rm = TRUE), "\n")
cat("Después - media:", mean(datos_limpios$T_min, na.rm = TRUE), "| min:", min(datos_limpios$T_min, na.rm = TRUE), "\n")

cat("\n=== PRECIPITACIÓN ===\n")
cat("Antes - media:", mean(datos$Precipitacion, na.rm = TRUE), "| max:", max(datos$Precipitacion, na.rm = TRUE), "\n")
cat("Después - media:", mean(datos_limpios$Precipitacion, na.rm = TRUE), "| max:", max(datos_limpios$Precipitacion, na.rm = TRUE), "\n")

cat("\n=== HUMEDAD RELATIVA ===\n")
cat("Antes - media:", mean(datos$HR, na.rm = TRUE), "| min:", min(datos$HR, na.rm = TRUE), "\n")
cat("Después - media:", mean(datos_limpios$HR, na.rm = TRUE), "| min:", min(datos_limpios$HR, na.rm = TRUE), "\n")

cat("\n=== VIENTO ===\n")
cat("Antes - media:", mean(datos$Viento, na.rm = TRUE), "| max:", max(datos$Viento, na.rm = TRUE), "\n")
cat("Después - media:", mean(datos_limpios$Viento, na.rm = TRUE), "| max:", max(datos_limpios$Viento, na.rm = TRUE), "\n")

cat("\n=== RADIACIÓN ===\n")
cat("Antes - media:", mean(datos$Radiacion, na.rm = TRUE), "| min:", min(datos$Radiacion, na.rm = TRUE), "| max:", max(datos$Radiacion, na.rm = TRUE), "\n")
cat("Después - media:", mean(datos_limpios$Radiacion, na.rm = TRUE), "| min:", min(datos_limpios$Radiacion, na.rm = TRUE), "| max:", max(datos_limpios$Radiacion, na.rm = TRUE), "\n")

# ============================================================
# 7. GUARDAR RESULTADOS
# ============================================================

# Guardar datos con flags de calidad
write.xlsx(datos, "datos_con_banderas_calidad.xlsx")

# Guardar datos limpios
write.xlsx(datos_limpios, "datos_limpios.xlsx")

