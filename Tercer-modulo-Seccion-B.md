Tercer modulo seccion B
================
Teddy Alvarez Zarate
2023-06-26

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(dynlm)
```

    ## Warning: package 'dynlm' was built under R version 4.2.3

    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(nlme)
```

    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

``` r
library(sandwich)
library(car)
```

    ## Warning: package 'car' was built under R version 4.2.3

    ## Loading required package: carData

    ## Warning: package 'carData' was built under R version 4.2.3

    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(ggplot2)
library(lmtest)
library(modelr)
library(lmtest)
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(moments)
library(strucchange)
```

    ## Warning: package 'strucchange' was built under R version 4.2.3

    ## 
    ## Attaching package: 'strucchange'
    ## 
    ## The following object is masked from 'package:stringr':
    ## 
    ##     boundary

``` r
library(nortest)
setwd("~/Experto en ciencia de datos/Modulo 3/Trabajo 3er modulo Seccion B")
datos <- read_excel("mocochinchi.xlsx")
modelo <- lm(Produccion_qq~Metodo_tunel+Rendimiento_arbol+ Numero_arboles, data=datos)
plot(modelo$residuals, type = "l")
```

![](Seccion-B_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
summary(modelo)
```

    ## 
    ## Call:
    ## lm(formula = Produccion_qq ~ Metodo_tunel + Rendimiento_arbol + 
    ##     Numero_arboles, data = datos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.8442 -2.4607  0.2051  2.1684 16.0246 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -20.692376   1.536120  -13.47   <2e-16 ***
    ## Metodo_tunel       10.238788   0.653002   15.68   <2e-16 ***
    ## Rendimiento_arbol   0.473906   0.033406   14.19   <2e-16 ***
    ## Numero_arboles      0.130656   0.003419   38.21   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.633 on 158 degrees of freedom
    ## Multiple R-squared:  0.9513, Adjusted R-squared:  0.9503 
    ## F-statistic:  1028 on 3 and 158 DF,  p-value: < 2.2e-16

## Intepretando los coeficientes del modelo

Puesto que la producción de mocochinchi esta medido en quintales, asi
como el rendimiento de los arboles a lo largo de su vida útil, de
acuerdo a los coeficientes obtenidos, tenemos que: Metodo túnel: 10,23
Lo que significa que si se utiliza el método túnel para el dehidratado
de durazno, se obtiene 10,24 quintales adicionales de mocochinchi
Rendimiento árbol: 0,47 Por cada incremento del redimiento de un árbol
en un quintal, se obtiene 0,47 quintales de mocochinchi Numero arboles:
0,13 Por cada árbol que se incrementa en la producción de durazno, se
tiene 0,13 quintales de mocochinchi

## Contraste Durbin Watsson

Ho: No existe autocorrelación H1: Existe autocorrelacion

``` r
dwtest(modelo)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  modelo
    ## DW = 1.6372, p-value = 0.009549
    ## alternative hypothesis: true autocorrelation is greater than 0

Puesto que p-valor (0.0095) \< que 0.05, se rechaza la Ho y se acepta la
H1 que existe un problema de autocorrelación

## Contraste Breusch Godfrey

Ho: No existe autocorrelación H1: Existe autocorrelacion

``` r
bgtest(modelo, order = 1)
```

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 1
    ## 
    ## data:  modelo
    ## LM test = 4.7586, df = 1, p-value = 0.02915

Puesto que p-valor (0,029) es menor que 0,05, se rechaza la hipótesis
nula y se acepta la hip alternativa que dice que Existe correlación.

## Atenuacion del problema

\#Metodo de error estándar libres de autocorrelación (metodo no
invasivo)

``` r
coeftest(modelo, vcov. = NeweyWest(modelo))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                      Estimate  Std. Error t value  Pr(>|t|)    
    ## (Intercept)       -20.6923762   1.7379828 -11.906 < 2.2e-16 ***
    ## Metodo_tunel       10.2387879   0.9772604  10.477 < 2.2e-16 ***
    ## Rendimiento_arbol   0.4739056   0.0379319  12.494 < 2.2e-16 ***
    ## Numero_arboles      0.1306562   0.0043355  30.136 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Con este metodo, se atenua el problema de autocorrelacion, sin la
necesidad de cambiar los valores de los coeficientes

## Heterocedasticidad

\#Metodo grafico para detectar heterosedasticidad

``` r
qplot(x=modelo$fitted.values, y=(modelo$residuals))+
geom_point()
```

    ## Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Seccion-B_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

En el grafico se puede ver cierta tendencia donde primero existe un
descenso para luego volver a subir \##Métodos de contraste \##Métodos
Brush-Pagan Ho: Existe homocedasticidad H1: No existe homocedasticidad

``` r
bptest(modelo, data = datos)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  modelo
    ## BP = 19.958, df = 3, p-value = 0.0001732

Ya que p-valor (0.0001732) es menor que 0,05, se rechaza la Ho y se
acepta H1 que dice que NO existe homocedasticidad (problema), ósea, que
existe heterocedasticidad

\##Atenuando el problema de heterocedasticidad \##Métodos HAC (no
invasivo)

``` r
modelo2<- coeftest(modelo, vcov = vcovHC(modelo))
modelo2
```

    ## 
    ## t test of coefficients:
    ## 
    ##                     Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)       -20.692376   1.438358 -14.386 < 2.2e-16 ***
    ## Metodo_tunel       10.238788   0.789036  12.976 < 2.2e-16 ***
    ## Rendimiento_arbol   0.473906   0.032114  14.757 < 2.2e-16 ***
    ## Numero_arboles      0.130656   0.003789  34.483 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Con este metodo, no invasivo, se atenua el problema de
heterocedasticidad

\##Multicolinealidad \##Identificando el problema \##Coeficiente de
correlacion

``` r
cor(datos[2:4], use = "complete.obs")
```

    ##                   Metodo_tunel Rendimiento_arbol Numero_arboles
    ## Metodo_tunel         1.0000000         0.1752337      0.3502798
    ## Rendimiento_arbol    0.1752337         1.0000000      0.2340648
    ## Numero_arboles       0.3502798         0.2340648      1.0000000

El coeficiente mas alto se presente entre las variables Numero arboles y
Metodo tunel que solo llega al 35%

``` r
ggpairs(datos[2:4])
```

![](Seccion-B_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
\##Identificando el problema de multicolinealidad mediante el factor de
inflacion de varianza (vif)

``` r
vif(modelo)
```

    ##      Metodo_tunel Rendimiento_arbol    Numero_arboles 
    ##          1.151934          1.069172          1.181280

En todas las variables los valores no se acercan a 10, por lo que se
puede decir que no existe un problema de multicolinealidad.

\##Prueba de normalidad de residuos

\##Identificando el problema de normalidad

``` r
datos <- datos%>%
     add_residuals(modelo)

hist(datos$resid)
```

![](Seccion-B_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Como se puede ver, el grafico no presenta una distribucion normal

\##Contraste Jarque Test

Ho: Los residuos tienen una distribución normal H1: Los residuos no
tienen una distribución normal

``` r
jarque.test(as.vector(modelo$residuals))
```

    ## 
    ##  Jarque-Bera Normality Test
    ## 
    ## data:  as.vector(modelo$residuals)
    ## JB = 19.219, p-value = 6.708e-05
    ## alternative hypothesis: greater

Ya que p-valor =6.708e-05, es menor que 0,05, se rechaza la Ho y se
acepta la H1 que dice que los residuos no tienen una distribución
normal.

\##Atenuando el problema de normalidad

``` r
datos <- datos%>%
    mutate(ln_Produccion=log(Produccion_qq))

modelo1 <- lm(ln_Produccion~Metodo_tunel+ Rendimiento_arbol+ Numero_arboles, data=datos)

jarque.test(as.vector(modelo1$residuals))
```

    ## 
    ##  Jarque-Bera Normality Test
    ## 
    ## data:  as.vector(modelo1$residuals)
    ## JB = 2.044, p-value = 0.3599
    ## alternative hypothesis: greater

Ya que p-valor =0.3599, es mayor que 0,05, se acepta la Ho y se rechaza
la H1 por lo que los residuos tienen una distribución normal.

\##Contraste de estabilidad

``` r
estable <- efp(modelo1, data = datos, type = "OLS-CUSUM")
plot(estable)
```

![](Seccion-B_files/figure-gfm/unnamed-chunk-14-1.png)<!-- --> De
acuerdo al grafico se puede decir que el modelo es estable
