#FUNCION
ejercicio3<- function(fecha){
  fechas<- as.Date(fecha)
  resultado1<- datos%>% filter(date == fecha) %>% arrange(desc(cases)) %>% head(1)
  resultado2<- datos%>% filter(date <= fecha) %>% arrange(desc(tasa_vacunacion)) %>% head(1)
  return(paste("La provincia con mas casos de covid en", fecha, "es", resultado1, "y la que tiene mayor tasa de vacunacion diaria es", resultado2))
}
fecha<- 2025-05-09
ejercicio3(fecha)



library(dplyr)
library(lubridate)

ejercicio3 <- function(fecha) {
  fecha <- as.Date(fecha)
  inicio <- fecha %m-% months(3)
  
  # Provincia con más casos el día exacto
  resultado1 <- datos %>%
    filter(date == fecha) %>% arrange(desc(cases)) %>% head(1)
  
  # Provincia con mayor tasa promedio de vacunación diaria en 3 meses anteriores
  resultado2 <- datos %>%
    filter(date > inicio & date <= fecha) %>%
    group_by(province) %>%
    summarise(tasa_vacunacion_prom = mean(tasa_vacunacion, na.rm = TRUE)) %>%
    arrange(desc(tasa_vacunacion_prom)) %>%
    head(1)
  
  paste0(
    "La provincia con más casos en ", fecha, " es ", resultado1$province, 
    " con ", resultado1$cases, " casos. ",
    "La provincia con mayor tasa de vacunación diaria promedio en los 3 meses anteriores es ", 
    resultado2$province, " con ", round(resultado2$tasa_vacunacion_prom, 2), " vacunas diarias."
  )
}

# Usar la función con una fecha:
fecha <- "2025-05-09"
ejercicio3(fecha)
