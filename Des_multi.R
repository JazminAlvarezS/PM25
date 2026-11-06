library(fpp3)
library(slider)
library(here)
library(readxl)


pm25 <- read_excel(
  "C:/Users/jazzd/OneDrive/Documentos/Data/PM2.5.xlsx",
  sheet = "LIMPIO"
)

glimpse(pm25)
#utilizando el here 
#pm25 <- read_excel(
#  here("Data", "PM2.5.xlsx"),
#  sheet = "LIMPIO"
#)


# PASO 2: Ancho -> largo (solo Belisario)
pm25_largo <- pm25 %>%
  select(FECHA, BELISARIO) %>%                 # <- evita columnas extra (K, L)
  pivot_longer(cols = -FECHA, names_to = "estacion", values_to = "pm25") %>%
  filter(estacion == "BELISARIO")              # <- filtro de estación

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

d <- pm25_mes %>%
  as_tibble() %>%
  mutate(fecha = as.Date(mes),
         mes_n = month(fecha))

#Tendencia-ciclo con 2x12-MA (m = 12 es par)
w <- c(1/24, rep(1/12, 11), 1/24)

d <- d %>%
  mutate(Tt = slide_dbl(pm25, ~ sum(.x * w),
                        .before = 6, .after = 6, .complete = TRUE),
         
# Serie sin tendencia (división)
         yt_Tt = pm25 / Tt)

# indices estacionales (media por mes), ajustados para sumar m = 12
St_tab <- d %>%
  group_by(mes_n) %>%
  summarise(S = mean(yt_Tt, na.rm = TRUE), .groups = "drop") %>%
  mutate(St = S * 12 / sum(S)) %>%      # suman 12 (media = 1)
  select(mes_n, St)

d <- d %>% left_join(St_tab, by = "mes_n")

# PASO 4: Componente resto
d <- d %>% mutate(Rt = pm25 / (Tt * St))

# GRAFICAS 
# 1. Serie original
g1 <- ggplot(d, aes(fecha, pm25)) +
  geom_line(colour = "gray40") +
  labs(x = NULL, y = "PM2.5 (µg/m³)",
       title = "1. Serie original - PM2.5 mensual, Belisario")

# 2. Tendencia-ciclo (sobre la serie original)
g2 <- ggplot(d, aes(fecha)) +
  geom_line(aes(y = pm25), colour = "gray70") +
  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1) +
  labs(x = NULL, y = "PM2.5 (µg/m³)",
       title = "2. Tendencia-ciclo (2x12-MA)")

# 3. Componente estacional
g3 <- ggplot(d, aes(fecha, St)) +
  geom_line(colour = "#0072B2") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = NULL, y = "St (índice)",
       title = "3. Componente estacional multiplicativo")

# 4. Componente aleatorio
g4 <- ggplot(d, aes(fecha, Rt)) +
  geom_line(colour = "#009E73") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = NULL, y = "Rt",
       title = "4. Componente aleatorio (residuo)")

g1; g2; g3; g4

#Grafica del modelo multiplicativo 
#tomamos esos años ya que no hay muchos datos faltantes en comparacion con los demas años 
dcmp <- pm25_mes %>%
  filter_index("2010-03" ~ "2019-06") %>%
  model(classical_decomposition(pm25, type = "multiplicative")) %>%
  components()

g5 <- autoplot(dcmp) +
  labs(title = "Descomposición clásica multiplicativa - PM2.5 Belisario (2010-2019)",
       x = NULL)
print(g5)
