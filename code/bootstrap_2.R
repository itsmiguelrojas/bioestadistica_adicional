# Cargar librerías ----
library(tidyverse)
library(readxl)

# Cargar datos ----
archivo <- "datos/salida_campo.xlsx"
salida.campo <- read_xlsx(archivo)

colnames(salida.campo) <- c(
  'individuo',
  'lugar',
  'muerto',
  'cap',
  'dap',
  'area.basal',
  'altura',
  'clase.dap'
)

salida.campo$lugar <- factor(salida.campo$lugar)

# Ver distribución de altura por lugar ----
salida.campo %>%
  filter(muerto == 'No') %>%
  ggplot(aes(x = lugar, y = altura, fill = lugar)) +
  geom_violin(alpha = 0.3) +
  stat_boxplot(geom = 'errorbar', width = 0.2, linewidth = 1) +
  geom_boxplot(width = 0.1)

# Separar datos de altura ----

alt.cv <- salida.campo %>%
  filter(lugar == 'CV', muerto == 'No') %>%
  pull(altura)

alt.jb <- salida.campo %>%
  filter(lugar == 'JB', muerto == 'No') %>%
  pull(altura)

set.seed(123)

alt.cv.sample <- alt.cv %>%
  as_tibble() %>%
  slice_sample(n = 30) %>%
  pull()

set.seed(123)

alt.jb.sample <- alt.jb %>%
  as_tibble() %>%
  slice_sample(n = 30) %>%
  pull()

# Centrar en cero ----
alt.cv.ho <- alt.cv.sample - mean(alt.cv.sample)
alt.jb.ho <- alt.jb.sample - mean(alt.jb.sample)

# Diferencia de medias ----
diff <- mean(alt.cv.sample) - mean(alt.jb.sample)

# Bootstrapping ----
B <- 9999
diff.means <- vector('numeric', B)

set.seed(123)

for(i in 1:B){
  sample.1 <- sample(alt.cv.ho, size = length(alt.cv.ho), replace = T)
  sample.2 <- sample(alt.jb.ho, size = length(alt.jb.ho), replace = T)
  
  diff.means[i] <- mean(sample.1) - mean(sample.2)
}

# Ver distribución de diferencias de media bajo H0 ----

diff.means %>%
  as_tibble() %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 14, fill = color.p[1], color = 'black') +
  geom_vline(xintercept = c(-diff,diff), linetype = 'dashed') +
  annotate(geom = 'label', x = diff, y = 1500, label = paste0('X = ', diff)) +
  annotate(geom = 'label', x = -diff, y = 1500, label = paste0('X = ', -diff)) +
  annotate(geom = 'label', x = -2, y = 2000, label = paste0('p = ', round(mean(diff.means >= diff) + mean(diff.means <= -diff),4))) +
  labs(
    x = 'Diferencia de promedios',
    y = 'Conteo',
    title = 'Distribución de diferencia de promedios de las alturas de individuos vegetales',
    subtitle = 'Cerro El Volcán (CV) y Jardín Botánico de Caracas (JB)'
  ) +
  theme_minimal()

# Cálculo del p-valor

mean(diff.means >= diff) + mean(diff.means <= -diff)