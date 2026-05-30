# Modelación ARIMA de la tasa del bono colombiano a 10 años
Este proyecto aplica la metodología Box-Jenkins a la tasa mensual del bono soberano colombiano a 10 años.

## Serie utilizada
- Fuente: FRED 
- Código de la serie: COLIRLTLT01STM
- Frecuencia: mensual
- Periodo: enero de 2003 a abril de 2026
- Variable: tasa de interés en porcentaje

## Metodología
1. Análisis gráfico de la serie.
2. Pruebas de estacionariedad: ADF, KPSS y KPSS con `urca`.
3. Aplicación de primera diferencia.
4. Estimación de modelos ARIMA(p,1,q).
5. Selección mediante AIC, BIC y validación de residuos.
6. Pronóstico a 10 meses.

## Modelo seleccionado
El modelo seleccionado fue ARIMA(0,1,1), debido a que presentó el menor AIC/BIC entre los modelos estimados y residuos sin autocorrelación significativa según Ljung-Box.

## Cómo correr el código
Abrir RStudio o VSCode desde la carpeta principal del proyecto y ejecutar:

```r
source("codigo/serie_poster_econometria2_v3_bono_colombia_10_anios.R")


