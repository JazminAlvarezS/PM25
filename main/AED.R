library(fpp3)
library(readxl)
library(here)

# PASO 1: Leer
pm25 <- read_excel(
  here("data", "PM2.5.xlsx"),
  sheet = "LIMPIO"
)

glimpse(pm25)

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

# GRÁFICOS
pm25_mes %>% gg_subseries(pm25)

pm25_ts %>% as_tibble() %>%
  mutate(hora = hour(FECHA)) %>%
  group_by(estacion, hora) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(hora, pm25, colour = estacion)) + geom_line()

pm25_mes %>%
  gg_season(pm25, period = "year") +
  labs(y = "PM2.5 (µg/m³)", x = NULL,
       title = "Estacionalidad anual del PM2.5 - Estación Belisario")








#####################################################################################################################################################################
library(fpp3)
library(readxl)
library(here)


# PASO 1: Leer
library(readxl)
library(readxl)

#pm25 <- read_excel(
 # "C:/Users/jazzd/OneDrive/Documentos/PM25/Data/PM2.5.xlsx",
  #sheet = "LIMPIO",
  #range = cell_cols("A:J"),
  #col_types = c("date", rep("numeric", 9))
#)
pm25 <- read_excel(
  here("data", "PM2.5.xlsx"),
  sheet = "LIMPIO"
)

glimpse(pm25)

# PASO 2: Ancho -> largo
pm25_largo <- pm25 %>%
  pivot_longer(cols = -FECHA, names_to = "estacion", values_to = "pm25")

# PASO 3: Redondear a la hora y fijar zona horaria
pm25_largo_corregido <- pm25_largo %>%
  mutate(FECHA = force_tz(round_date(FECHA, "hour"), "America/Guayaquil"))

# PASO 4: Duplicados
duplicados <- pm25_largo_corregido %>%
  count(estacion, FECHA) %>%
  filter(n > 1)
stopifnot(nrow(duplicados) == 0)

# PASO 5: tsibble (quitando NA previos al inicio de cada estación)
pm25_ts <- pm25_largo_corregido %>%
  filter(!is.na(pm25)) %>%
  as_tsibble(key = estacion, index = FECHA) %>%
  fill_gaps()          # huecos internos -> NA explícitos

has_gaps(pm25_ts)      # TRUE esperado (hay huecos reales)
pm25_ts %>% count_gaps() %>% arrange(desc(.n))   # los huecos más largos


pm25_mes <- pm25_ts %>%
  group_by_key() %>%
  index_by(mes = yearmonth(FECHA)) %>%
  summarise(n = sum(!is.na(pm25)), pm25 = mean(pm25, na.rm = TRUE)) %>%
  mutate(cobertura = n / (days_in_month(as.Date(mes)) * 24),
         pm25 = if_else(cobertura >= 0.75, pm25, NA_real_)) %>%
  fill_gaps()

#GRAFICOS
#Evolución y Estacionalidad Mensual del PM2.5 por Estación de Monitoreo
pm25_mes %>% gg_subseries(pm25)
#Este gráfico de subseries estacionales (gg_subseries)
#muestra el comportamiento histórico de la concentración media de material 
#particulado PM2.5 ($\mu g/m^3$) para cada mes del año (de enero a diciembre), desglosado por estación de monitoreo de la red de calidad del aire de Quito. 



#Ciclo Diario Promedio de Concentracion de PM2.5 por Estación de Monitoreo
pm25_ts %>% as_tibble() %>%
  mutate(hora = hour(FECHA)) %>%
  group_by(estacion, hora) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(hora, pm25, colour = estacion)) + geom_line()

#Este grafico de lineas muestra el perfil horario promedio 
#de las concentraciones de material particulado PM2.5 ($\mu g/m^3$) 
#a lo largo de las 24 horas del día para cada una de las estaciones de monitoreo de la red de calidad del aire.


#Estacionalidad Anual de la Concentracion de PM2.5 por Estacion de Monitoreo
pm25_mes %>%
  gg_season(pm25, period = "year") +
  facet_wrap(vars(estacion), ncol = 3, scales = "free_y") +
  labs(y = "PM2.5 (µg/m³)", x = NULL,
       title = "Estacionalidad anual del PM2.5 por estación") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Este grafico de estacionalidad superpuesta permite comparar
#el ciclo anual de las concentraciones de material particulado PM2.5 ($\mu g/m^3$)
#mes a mes, agrupando los datos por cada estacion de monitoreo de la red de calidad del aire.  
