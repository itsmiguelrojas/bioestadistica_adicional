## Cargar librerías ----
library(tidyverse)
library(readxl)

## Cargar datos ----
datos <- "salida_campo.xlsx"
salida.campo <- read_xlsx(datos)

## Cambiar nombre de columnas ----
colnames(salida.campo) <- c('individuo','lugar','muerto','cap','dap','ab','altura','clase.dap')

## Cambiar strings por factores

salida.campo$lugar <- factor(salida.campo$lugar)
salida.campo$muerto <- factor(salida.campo$muerto, levels = c('Sí','No'))
salida.campo$clase.dap <- factor(salida.campo$clase.dap, ordered = TRUE)

## Ver resumen de los datos ----
summary(salida.campo)
by(salida.campo, salida.campo$lugar, summary)

salida.campo %>%
  filter(!is.na(altura)) %>%
  ggplot(aes(x = lugar, y = altura, color = lugar)) +
  geom_jitter(size = 3)

## Muestreo ----

set.seed(2376)

salida.muestra <- salida.campo %>%
  filter(!is.na(altura)) %>%
  arrange(lugar) %>%
  slice_sample(n = 10, by = lugar)

salida.muestra %>%
  ggplot(aes(x = lugar, y = altura, color = lugar)) +
  geom_jitter(size = 3)

## Bootstrapping ----

### Número de remuestreos ----

B <- 9999

### Separar datos por lugar ----

CV.altura <- salida.muestra %>%
  filter(!is.na(altura), lugar == 'CV') %>%
  pull(altura)

JB.altura <- salida.muestra %>%
  filter(!is.na(altura), lugar == 'JB') %>%
  pull(altura)

### Calcular diferencia de medias ----

dif.media <- mean(CV.altura) - mean(JB.altura)
t.stat <- t.test(CV.altura, JB.altura)$statistic

CV.centro <- CV.altura - mean(CV.altura)
JB.centro <- JB.altura - mean(JB.altura)

### Agrupar muestras ----

pool <- c(CV.altura, JB.altura)
t.boot <- vector('numeric',B)

### Bucle for ----

set.seed(2376)

for(i in 1:B){
  boot.A <- sample(CV.centro, size = length(CV.centro), replace = TRUE)
  boot.B <- sample(JB.centro, size = length(JB.centro), replace = TRUE)
  
  t.boot[i] <- t.test(boot.A, boot.B)$statistic
}

### p-valor ----

p.value.upr <- mean(t.boot >= t.stat)
p.value.lwr <- mean(t.boot <= -t.stat)

2*max(p.value.upr, p.value.lwr)
p.value.lwr + p.value.upr
2*min(p.value.upr, p.value.lwr)
2*min(p.value.lwr, 1-p.value.lwr)

t.boot %>%
  as_tibble() %>%
  ggplot(aes(x = t.boot)) +
  geom_histogram(bins = 14, color = 'black', alpha = 0.3, fill = 'blue') +
  theme_minimal()