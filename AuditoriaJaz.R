# Descomposición clásica aditiva del PM2.5 mensual de Belisario.

library(fpp3)
library(readxl)
library(here)

# PASO 1: Leer
pm25 <- read_excel(
  here("Data", "PM2.5.xlsx"),
  sheet = "LIMPIO"
)


pm25 <- read_excel(
  "C:/Users/jazzd/OneDrive/Documentos/Data/PM2.5.xlsx",
  sheet = "LIMPIO"
)

glimpse(pm25)

# PASO 2: Ancho -> largo (solo Belisario)
pm25_largo <- pm25 %>%
  select(FECHA, BELISARIO) %>%
  pivot_longer(cols = -FECHA, names_to = "estacion", values_to = "pm25") %>%
  filter(estacion == "BELISARIO")

# PASO 3: Redondear a la hora y fijar zona horaria
pm25_largo_corregido <- pm25_largo %>%
  mutate(FECHA = force_tz(round_date(FECHA, "hour"), "America/Guayaquil"))

# PASO 4: Duplicados
duplicados <- pm25_largo_corregido %>%
  count(estacion, FECHA) %>%
  filter(n > 1)
stopifnot(nrow(duplicados) == 0)

# PASO 5: tsibble
pm25_ts <- pm25_largo_corregido %>%
  filter(!is.na(pm25)) %>%
  as_tsibble(key = estacion, index = FECHA) %>%
  fill_gaps()

has_gaps(pm25_ts)
pm25_ts %>% count_gaps() %>% arrange(desc(.n))

pm25_mes <- pm25_ts %>%
  group_by_key() %>%
  index_by(mes = yearmonth(FECHA)) %>%
  summarise(n = sum(!is.na(pm25)), pm25 = mean(pm25, na.rm = TRUE)) %>%
  mutate(cobertura = n / (days_in_month(as.Date(mes)) * 24),
         pm25 = if_else(cobertura >= 0.75, pm25, NA_real_)) %>%
  fill_gaps()

# Gráfico de la serie observada
pm25_mes |> autoplot(pm25, colour = "#176B87", na.rm = TRUE) +
  labs(title = "PM2.5 mensual observado en Belisario")

# Usar la misma serie interpolada en ambas descomposiciones.
pm25_completa <- pm25_mes |>
  mutate(pm25 = zoo::na.approx(pm25, na.rm = FALSE)) |>
  filter(!is.na(pm25))

# Paso 1. Estimar la tendencia-ciclo con una media móvil centrada 2 × 12.
descomposicion <- pm25_completa |>
  mutate(
    `12-MA` = slider::slide_dbl(
      pm25, mean, .before = 5, .after = 6, .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(
      `12-MA`, mean, .before = 1, .after = 0, .complete = TRUE
    )
  )

# Gráfico de la tendencia 
pm25_mes |>
  autoplot(pm25, colour = "grey65", na.rm = TRUE) +
  geom_line(
    data = descomposicion, aes(x = mes, y = `2x12-MA`),
    colour = "#D55E00", linewidth = 0.9, na.rm = TRUE
  ) +
  labs(
    title = "Tendencia del PM2.5 en Belisario",
    subtitle = "Serie observada en gris y media móvil de la serie interpolada en naranja"
  )

# Paso 2. Quitar la tendencia.
descomposicion <- descomposicion |>
  mutate(
    sin_tendencia = pm25 - `2x12-MA`,
    mes_del_anio = month(as.Date(mes))
  )

# Gráfico de la serie sin tendencia
descomposicion |> autoplot(sin_tendencia, colour = "#176B87", na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  labs(
    title = "Serie sin tendencia",
    subtitle = "PM2.5 de la serie interpolada menos la tendencia-ciclo"
  )

# Paso 3. Promediar la serie sin tendencia por mes del año.
# Restar la media de los 12 promedios para que los efectos sumen cero.
patron_estacional <- descomposicion |>
  as_tibble() |>
  filter(!is.na(sin_tendencia)) |>
  group_by(mes_del_anio) |>
  summarise(
    n_valores = n(),
    promedio_sin_tendencia = mean(sin_tendencia),
    .groups = "drop"
  )

if (nrow(patron_estacional) != 12L) {
  stop("Se necesita al menos un valor sin tendencia para cada mes del año.")
}
patron_estacional <- patron_estacional |>
  mutate(estacional = promedio_sin_tendencia - mean(promedio_sin_tendencia))

# Repetir los 12 efectos mensuales a lo largo de la serie.
descomposicion <- descomposicion |>
  left_join(select(patron_estacional, mes_del_anio, estacional), by = "mes_del_anio")

# Gráfico de la componente estacional
descomposicion |> autoplot(estacional, colour = "#176B87", na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  labs(
    title = "Componente estacional",
    subtitle = "Patrón anual con 12 efectos mensuales que suman cero"
  )

# Paso 4. Restar la estacionalidad a la serie sin tendencia.                                      #si me corrieron todos los graficos no tuve mayor problema solo
descomposicion <- descomposicion |>                                                                #tuve que instalar el paquete zoo y todo funciono bien
  mutate(restante = sin_tendencia - estacional)                                                   #me quedo como este codigo como base para mi proyecto jeje gracias mau 

descomposicion |> autoplot(restante, colour = "#176B87", na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  labs(
    title = "Componente restante",
    subtitle = "PM2.5 de la serie interpolada menos la tendencia y la estacionalidad"
  )


#Auditar#########
library(feasts)
library(fabletools)   # ya vienen con fpp3, no hace daño

# 1. Modelo de R sobre LA MISMA serie interpolada (pm25_completa)
modelo_comp <- pm25_completa |>
  model(classical_decomposition(pm25 ~ season(12), type = "additive")) |>
  components()

# Gráfica del modelo de R (4 paneles)
modelo_comp |>
  autoplot() +
  labs(title = "Descomposición clásica aditiva (fpp3) - PM2.5 Belisario")

# 2. Gráfica de la descomposición manual en 4 paneles
d_largo <- descomposicion |>
  as_tibble() |>
  mutate(fecha = as.Date(mes)) |>
  select(fecha,
         `yt (serie original)` = pm25,
         `Tt (tendencia)`      = `2x12-MA`,
         `St (estacional)`     = estacional,
         `Rt (residuo)`        = restante) |>
  pivot_longer(-fecha, names_to = "componente", values_to = "valor") |>
  mutate(componente = factor(componente,
                             levels = c("yt (serie original)", "Tt (tendencia)",
                                        "St (estacional)", "Rt (residuo)")))

ggplot(d_largo, aes(fecha, valor)) +
  geom_line(aes(colour = componente), show.legend = FALSE, na.rm = TRUE) +
  facet_wrap(~componente, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("gray40", "#D55E00", "#0072B2", "#009E73")) +
  labs(x = NULL, y = NULL,
       title = "Descomposición clásica aditiva MANUAL - PM2.5 Belisario",
       subtitle = "yt = Tt + St + Rt")

# 3. Comparación manual vs modelo (las diferencias deben ser ~0)
comparacion <- descomposicion |>
  as_tibble() |>
  left_join(
    modelo_comp |> as_tibble() |> select(estacion, mes, trend, seasonal, random),
    by = c("estacion", "mes")
  ) |>
  mutate(
    dif_Tt = `2x12-MA` - trend,
    dif_St = estacional - seasonal,
    dif_Rt = restante   - random
  )

# Máxima diferencia absoluta por componente
comparacion |>
  summarise(
    max_dif_Tt = max(abs(dif_Tt), na.rm = TRUE),
    max_dif_St = max(abs(dif_St), na.rm = TRUE),
    max_dif_Rt = max(abs(dif_Rt), na.rm = TRUE)
  ) |>
  print(width = Inf)

# TRUE si coinciden dentro de la tolerancia numérica
all(abs(c(comparacion$dif_Tt, comparacion$dif_St, comparacion$dif_Rt)) < 1e-10,
    na.rm = TRUE)

# Los NA deben estar en las mismas posiciones (12 en tendencia: 6 al inicio y 6 al final)
c(NA_manual = sum(is.na(comparacion$`2x12-MA`)),
  NA_modelo = sum(is.na(comparacion$trend)))

# 4. Verificaciones internas
# yt = Tt + St + Rt
descomposicion |>
  as_tibble() |>
  mutate(chequeo = `2x12-MA` + estacional + restante - pm25) |>
  summarise(max_error = max(abs(chequeo), na.rm = TRUE))

# Los 12 efectos estacionales deben sumar 0
sum(patron_estacional$estacional)

#Se auditó la descomposicion clasica aditiva manual de mau
#comparandola con classical_decomposition()
#Las diferencias maximas absolutas fueron de 7.11×10⁻¹⁵ en la tendencia, 9.99×10⁻¹
#en el componente estacional y 8.88×10⁻¹⁵ en el residuo
#se acerca mucho a cero ,no es cero por el punto flotante entonces coincide perfectamente 
#al inicio y al final 
#y la identidad yt = Tt + St + Rt se verifico con un error maximo de 1.78×10⁻¹⁵
#asi que todo bajo control 