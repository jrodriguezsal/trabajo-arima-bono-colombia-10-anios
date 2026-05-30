# Limpieza de entorno
# Limpieza de entorno
rm(list = ls())

if (!is.null(dev.list())) {dev.off()}  #se condiciona tanto por si no hay graficas y par aqeu sno salga error.

#______________________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 0. Instalación de Paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
} #condicionamos el paquete de pacman en caso de que no este instalado

pacman::p_load(
  tseries,     # ADF, KPSS, Jarque-Bera
  urca,        # Pruebas alternativas de raíz unitaria / estacionariedad
  FinTS,       # Prueba ARCH
  TSA,         # Herramientas para series de tiempo
  tidyverse,   # dplyr, ggplot2, readr, etc.
  forecast,    # Pronósticos
  quantmod,    # Lectura de datos financieros / FRED
  nortsTest,   # Pruebas adicionales
  car,         # Apoyo gráfico
  stargazer,   # Tablas de modelos
  here,        # Rutas relativas 
  fs,          # Manejo de carpetas
  lubridate,   # Manejo de fechas
  zoo          # Manejo de índices temporales
)

# Creamos carpetas para tener un proyecto reproducible
fs::dir_create("datos")
fs::dir_create("outputs/figuras")
fs::dir_create("outputs/tablas")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 1. Series de tiempo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ruta local de la base descargada -Lectura en local
ruta_bono <- "datos/bono10_colombia.csv"

# Cargar base local
Base_bono10 <- readr::read_csv(ruta_bono, show_col_types = FALSE)

Base_bono10 <- setNames(
  Base_bono10,
  c("año-mes", "tasa en %")
)

names(Base_bono10)
head(Base_bono10, 20)
tail(Base_bono10, 20)


# Convertimos los datos en una serie de tiempo
Base_bono10 = ts(Base_bono10$`tasa en %`, start = c(2003), freq = 12)
head(Base_bono10,20)
tail(Base_bono10,5)


#~~Gráfica con estilo~~#
chartSeries(Base_bono10,
            theme = chartTheme("white", up.col = "royalblue"),
            show.grid = F,
            TA = NULL,
            name = "Tasa interés bono Colombia 10 años"
)   

#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2. Estacionariedad ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.1. Método Gráfico (FAC & FACP) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

par(mfrow = c(1,2))    #Para abrir una ventana gráfica con 1 fila y 2 columnas
acf(Base_bono10, main = "FAC del Bono a 10 años")   #ACF Auto_Correlation_Function
pacf(Base_bono10, main = "FACP del Bono a 10 años")   #PACF Partial_Auto_Correlation_Function
par(mfrow = c(1, 1))  # para volver aponer la ventana a una sola grafica.
### La caída lineal (lenta) de la FAC sugiere una serie no estacionaria.
### Aplicamos pruebas de estacionariedad para saberlo con certeza


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.2. Prueba de Raíz Unitaria - ADF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Entre las pruebas de RU usaremos Dickey-Fuller Aumentada
## H0= Tiene raíz unitaria y no es estacionaria.
## si p<0.05 se rechaza H0 -> Es estacionaria.
adf.test(Base_bono10)
TestADF = adf.test(Base_bono10)$p.value

#Si persisten las dudas de estacionariedad aplicamos la prueba KPSS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.3. Prueba KPSS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## H0= La serie es estacionaria.
## Si p<0.05 se rechaza H0 -> No estacionaria
kpss.test(Base_bono10)
TestKPSS = kpss.test(Base_bono10)$p.value

#Si no es concluyente hacemos uso del paquete URCA
TestURCA = ur.kpss(Base_bono10)
summary(TestURCA)
## Decisión: Si el estadístico de prueba es mayor que el valor crítico se rechaza H0
## Regla de decisión: Si el estadístico de prueba es mayor que el valor crítico,
## se rechaza la hipótesis nula de estacionariedad.


#Las dos pruebas arrojaron que la serie no es estacionaria,
# Para lograr una serie estacionaria, podemos transformar la serie.
# Apliquemos diferencia para conseguir estacionariedad.
## Como los valores son % no es necesario aplicar logaritmo

bono10dif = diff(Base_bono10)

# Gráficamente
chartSeries(bono10dif,
            theme = chartTheme("white", up.col = "royalblue"),
            show.grid = T,
            TA = NULL,
            name = "Diferencia tasa de interés Bono Colombia 10 años"
)

# Calculamos la FAC y la FACP de la serie transformada
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))

fac_bono <- acf(bono10dif,lag.max = 24, plot = FALSE)
plot( fac_bono,main = "FAC",ylim = c(-1, 1)) 
#la serie diferenciada deja de ser diferenciaporcentual y pasa a ser cambio en puntos porcentuales.

facp_bono <- pacf(bono10dif,lag.max = 24, plot = FALSE)
plot( facp_bono,main = "FACP",ylim = c(-1, 1)) 
#ylim  sirve para mantener los margenes verticales en le rango de 1 a -1

mtext("Serie diferenciada: cambio mensual en puntos porcentuales",outer = TRUE, cex = 1)

par(mfrow = c(1, 1))
# La serie diferenciada representa el cambio mensual en puntos porcentuales
# de la tasa del bono.

#verificacion de estacionareidad serie diferenciada
adf.test(bono10dif)
kpss.test(bono10dif)

#Proponemos VARIOS MODELOS y al final elegimos el que mejor ajuste
arima011 <- arima(Base_bono10, order = c(0,1,1), include.mean = FALSE, method = "ML")
arima110 <- arima(Base_bono10, order = c(1,1,0), include.mean = FALSE, method = "ML")
arima111 <- arima(Base_bono10, order = c(1,1,1), include.mean = FALSE, method = "ML")
arima211 <- arima(Base_bono10, order = c(2,1,1), include.mean = FALSE, method = "ML")
arima112 <- arima(Base_bono10, order = c(1,1,2), include.mean = FALSE, method = "ML")
arima212 <- arima(Base_bono10, order = c(2,1,2), include.mean = FALSE, method = "ML")

# Resultado del modelo
stargazer::stargazer(
  arima011,
  arima110,
  arima111,
  arima211,
  arima112,
  arima212,
  type = "text",
  title = "Modelos ARIMA estimados"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.3. Aplicación de AIC y BIC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Para mayor certeza, aplicamos estadísticos de prueba para
#seleccionar el modelo más adecuado entre los que calculamos

tabla_modelos <- data.frame(
  Modelo = c(
    "ARIMA(0,1,1)", "ARIMA(1,1,0)", "ARIMA(1,1,1)", "ARIMA(2,1,1)",
    "ARIMA(1,1,2)", "ARIMA(2,1,2)"),
  AIC = c(AIC(arima011),AIC(arima110),AIC(arima111),AIC(arima211),
          AIC(arima112),AIC(arima212) ),
  BIC = c(BIC(arima011),BIC(arima110),BIC(arima111),BIC(arima211),
    BIC(arima112),BIC(arima212)))

tabla_modelos <- tabla_modelos[order(tabla_modelos$BIC), ]
print(tabla_modelos)

## El modelo con el menor valor fue el -------- (0,1,1)
# El mismo que se propuso inicialmente

## Ahora hacemos la prueba de validacion de supuestos para verificar y 
## tambien si los residuos ditribuyen como ruido blanco


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 3. Comparación y validación de modelos ARIMA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ahora comparamos todos los modelos candidatos.
# No se elige únicamente por AIC/BIC; también se revisa si los residuos
# se comportan como ruido blanco.

# Lista de modelos estimados
modelos <- list("ARIMA(0,1,1)" = arima011,"ARIMA(1,1,0)" = arima110,
  "ARIMA(1,1,1)" = arima111,"ARIMA(2,1,1)" = arima211,
  "ARIMA(1,1,2)" = arima112,"ARIMA(2,1,2)" = arima212)

# Órdenes p y q de cada modelo.
# Esto sirve para ajustar los grados de libertad en Ljung-Box.
ordenes <- data.frame(
  Modelo = names(modelos),
  p = c(0, 1, 1, 2, 1, 2),
  q = c(1, 0, 1, 1, 2, 2)
)

# Verificación:
# Todos deben aparecer como modelos tipo "Arima" o "arima".
print(sapply(modelos, class))

# Tabla vacía donde se guardarán los resultados
tabla_diagnostico <- data.frame()

# Ciclo para evaluar cada modelo
for (nombre in names(modelos)) {
  
  # Seleccionamos el modelo actual
  modelo_actual <- modelos[[nombre]]
  
  # Extraemos residuos del modelo actual
  residuos_actuales <- residuals(modelo_actual)
  residuos_actuales <- na.omit(as.numeric(residuos_actuales))
  
  # Identificamos p y q del modelo actual
  p <- ordenes$p[ordenes$Modelo == nombre]
  q <- ordenes$q[ordenes$Modelo == nombre]
  
  #--------------------------------------------#
  # Prueba Ljung-Box
  #--------------------------------------------#
  # H0: no hay autocorrelación en los residuos.
  # Queremos p-value > 0.05.
  
  lb6 <- Box.test(
    residuos_actuales,
    lag = 6,
    type = "Ljung-Box",
    fitdf = p + q
  )
  
  lb12 <- Box.test(
    residuos_actuales,
    lag = 12,
    type = "Ljung-Box",
    fitdf = p + q
  )
  
  lb24 <- Box.test(
    residuos_actuales,
    lag = 24,
    type = "Ljung-Box",
    fitdf = p + q
  )
  
  #--------------------------------------------#
  # Prueba Jarque-Bera
  #--------------------------------------------#
  # H0: los residuos siguen una distribución normal.
  # Queremos p-value > 0.05.
  
  jb <- tseries::jarque.bera.test(residuos_actuales)
  
  #--------------------------------------------#
  # Prueba ARCH
  #--------------------------------------------#
  # H0: no hay efectos ARCH en los residuos.
  # Queremos p-value > 0.05.
  
  arch <- FinTS::ArchTest(
    residuos_actuales,
    lags = 12
  )
  
  # Guardamos los resultados del modelo actual
  fila <- data.frame(
    Modelo = nombre,
    AIC = as.numeric(AIC(modelo_actual)),
    BIC = as.numeric(BIC(modelo_actual)),
    LB_6_pvalue = as.numeric(lb6$p.value),
    LB_12_pvalue = as.numeric(lb12$p.value),
    LB_24_pvalue = as.numeric(lb24$p.value),
    JB_pvalue = as.numeric(jb$p.value),
    ARCH_pvalue = as.numeric(arch$p.value)
  )
  
  tabla_diagnostico <- rbind(tabla_diagnostico, fila)
}

# Ordenamos la tabla por BIC.
# BIC suele preferirse cuando se busca un modelo más parsimonioso.
tabla_diagnostico <- tabla_diagnostico[order(tabla_diagnostico$BIC), ]
rownames(tabla_diagnostico) <- NULL

# Mostramos la tabla completa
print(tabla_diagnostico)

modelo_elegido_nombre <- "ARIMA(0,1,1)"
modelo_elegido <- arima011

cat("Modelo elegido:", modelo_elegido_nombre, "\n")
print(modelo_elegido)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Validación gráfica del modelo elegido ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Residuos del modelo elegido
residuos <- residuals(modelo_elegido)
residuos <- na.omit(as.numeric(residuos))

# Rango simétrico para los gráficos de residuos
lim_res <- max(abs(residuos))

# Calculamos FAC y FACP sin graficarlas directamente
fac_res <- acf(residuos, lag.max = 24, plot = FALSE)
facp_res <- pacf(residuos, lag.max = 24, plot = FALSE)

# Grilla de gráficos
par(mfrow = c(2, 2))

# 1. Residuos en el tiempo
plot(
  residuos,
  main = "Residuos del ARIMA(0,1,1)",
  ylab = "Residuo",
  xlab = "Tiempo",
  ylim = c(-lim_res, lim_res))

abline(h = 0, col = "red", lty = 2)

# 2. FAC de residuos
plot(fac_res,main = "FAC de residuos",ylim = c(-1, 1))

# 3. FACP de residuos
plot(facp_res,main = "FACP de residuos",ylim = c(-1, 1))

# 4. Q-Q plot de residuos
qq_res <- qqnorm(residuos,main = "Q-Q plot de residuos",plot.it = FALSE)

plot(qq_res$x,qq_res$y,main = "Q-Q plot de residuos",
     xlab = "Cuantiles teóricos",ylab = "Cuantiles muestrales",
  ylim = c(-lim_res, lim_res))

qqline(residuos, col = "red")

par(mfrow = c(1, 1))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 5. PRONÓSTICO A 10 PERIODOS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Se reestima el modelo elegido con forecast::Arima()
# Esto permite usar forecast() correctamente para el pronóstico.

modelo_elegido <- forecast::Arima( Base_bono10,order = c(0, 1, 1),
  include.mean = FALSE,method = "ML")

# Generar pronóstico a 10 meses
pronostico <- forecast::forecast(modelo_elegido,h = 10,level = 95)

# Mostrar pronóstico
print(pronostico)

# Graficar pronóstico
plot(pronostico,
  main = "Pronóstico a 10 meses - ARIMA(0,1,1)",
  xlab = "Tiempo",
  ylab = "Tasa de interés (%)")

tabla_pronostico <- data.frame(
  pronostico = as.numeric(pronostico$mean),
  limite_inferior_95 = as.numeric(pronostico$lower[,1]),
  limite_superior_95 = as.numeric(pronostico$upper[,1]))

print(tabla_pronostico)
