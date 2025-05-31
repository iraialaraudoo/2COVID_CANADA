#EXAMEN 2022 MODELO A

#Crear df
set.seed(123)  # para reproducibilidad

# Provincias comunes de Canadá
provinces <- c("Ontario", "Quebec", "British Columbia", "Alberta", "Manitoba",
               "Saskatchewan", "Nova Scotia", "New Brunswick", "Newfoundland and Labrador", "Prince Edward Island")

n_rows <- 200

# Crear fechas secuenciales desde hace 200 días hacia hoy
dates <- seq.Date(Sys.Date() - n_rows + 1, Sys.Date(), by = "day")

# Repetir provincias para completar las filas (aproximadamente)
province_col <- rep(provinces, length.out = n_rows)

# Simular población fija por provincia (en miles)
populations <- c(14710, 8537, 5116, 4372, 1375, 1170, 979, 781, 520, 159) * 1000
pop_df <- data.frame(province = provinces, population = populations)

# Unir población a cada fila según provincia
population_col <- pop_df$population[match(province_col, pop_df$province)]

# Simular casos acumulados (incrementan día a día, por provincia)
cases_cum <- numeric(n_rows)
vaccines_cum <- numeric(n_rows)
deaths_cum <- numeric(n_rows)
tests_cum <- numeric(n_rows)

for (i in 1:n_rows) {
  base_case <- 10000 + i*20 + runif(1, 0, 1000)
  base_vaccine <- 50000 + i*50 + runif(1, 0, 2000)
  base_death <- 200 + i*1.5 + runif(1, 0, 50)
  base_tests <- 200000 + i*100 + runif(1, 0, 5000)
  
  cases_cum[i] <- base_case
  vaccines_cum[i] <- base_vaccine
  deaths_cum[i] <- base_death
  tests_cum[i] <- base_tests
}

# Crear el dataframe
df_sim <- data.frame(
  province = province_col,
  date = dates,
  cases = round(cases_cum),
  deaths = round(deaths_cum),
  population = population_col,
  vaccines_administered = round(vaccines_cum),
  tests = round(tests_cum)
)
head(df_sim,3)
datos<- df_sim

#####################################################################################3

colnames(datos)
min(datos$date)
max(datos$date)

datos <- datos %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= (Sys.Date() - months(6)))

# Ordenamos los datos por provincia y fecha
datos <- datos %>%
  arrange(province, date)

# Calculamos casos nuevos y vacunas nuevas por día
datos <- datos %>%
  group_by(province) %>%
  mutate(
    casos_nuevos = cases - lag(cases, default = 0),
    vacunas_nuevas = vaccines_administered - lag(vaccines_administered, default = 0)
  ) %>%
  ungroup()

# Calculamos tasas por 100,000 habitantes
datos <- datos %>%
  mutate(
    tasa_contagios = (casos_nuevos / population) * 100000,
    tasa_vacunacion = (vacunas_nuevas / population) * 100000
  )
colnames(datos)
provincia_contagios<- datos%>% group_by(province) %>% select(tasa_contagios) %>% arrange(desc(tasa_contagios))
#Prince Edward Island              
provincia_vacunacion<- datos%>% group_by(province) %>% select(tasa_vacunacion) %>% arrange(desc(tasa_vacunacion))
#Prince Edward Island


