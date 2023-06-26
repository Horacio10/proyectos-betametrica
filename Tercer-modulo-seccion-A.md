Tercer modulo Seccion A
================
Teddy Alvarez Zarate
2023-06-26

## 1.- Calculo de la tasa de crecimiento de las exportaciones en Bolivia, desde el año 1992-2023

``` r
setwd("~/Experto en ciencia de datos/Modulo 3/Trabajo 3er modulo Seccion A")
datos <- read_excel("exportaciones.xlsx")
attach(datos)
datos1 <- ts(datos, start = c(1992))
plot(datos1[,"Exportaciones"])
```

![](Tercer-modulo-seccion-A_files/figure-gfm/cars-1.png)<!-- -->

``` r
tasa_export <- tslm(log(Exportaciones)~trend, data = datos1)
summary(tasa_export)
```

    ## 
    ## Call:
    ## tslm(formula = log(Exportaciones) ~ trend, data = datos1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7383 -0.1711 -0.0216  0.2764  0.7877 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 6.725608   0.173988  38.656  < 2e-16 ***
    ## trend       0.089457   0.009202   9.721  8.8e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4806 on 30 degrees of freedom
    ## Multiple R-squared:  0.7591, Adjusted R-squared:  0.751 
    ## F-statistic: 94.51 on 1 and 30 DF,  p-value: 8.796e-11

``` r
(exp(0.089457)-1)*100
```

    ## [1] 9.358031

## Interpretacion del modelo de la tasa de crecimiento de Exportaciones

### De acuerdo al resultado obtenido, se puede decir que la tasa de crecimiento de las exportaciones en Bolivia, desde al año 1992-2003 es de 9,35% anual

## 2.- Calculo de la tasa de crecimiento de la masa monetaria en Bolivia, desde el año 1992-2019

``` r
setwd("~/Experto en ciencia de datos/Modulo 3/Trabajo 3er modulo Seccion A")
oferta <- read_excel("oferta.xlsx")
attach(oferta)
oferta1 <- ts(oferta, start = c(1992))
plot(oferta1[,"Masa monetaria"])
```

![](Tercer-modulo-seccion-A_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
tasa <- tslm(log(`Masa monetaria`)~trend, data = oferta1)
(exp(0.030552)-1)*100
```

    ## [1] 3.10235

## Interpretacion del modelo de la tasa de crecimiento de la masa monetaria

### De acuerdo al resultado obtenido, se puede decir que la tasa de crecimiento de la masa monetaria en Bolivia, desde al año 1992-2019 es de 3,10% anual
