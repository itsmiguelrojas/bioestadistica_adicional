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
  filter(lugar == 'CV', !is.na(altura)) %>%
  pull(altura)

alt.jb <- salida.campo %>%
  filter(lugar == 'JB', !is.na(altura)) %>%
  pull(altura)

# Centrar en cero ----
alt.cv.ho <- alt.cv - mean(alt.cv)
alt.jb.ho <- alt.jb - mean(alt.jb)

# Diferencia de medias ----
diff <- mean(alt.cv) - mean(alt.jb)

# Bootstrapping ----
B <- 19999
diff.means <- vector('numeric', B)

for(i in 1:B){
  #sample.1 <- sample(alt.cv.ho, size = length(alt.cv.ho), replace = T)
  #sample.2 <- sample(alt.jb.ho, size = length(alt.jb.ho), replace = T)
  
  sample.1 <- sample(alt.cv.ho, size = length(alt.cv.ho), replace = T)
  sample.2 <- sample(alt.jb.ho, size = length(alt.jb.ho), replace = T)
  
  diff.means[i] <- mean(sample.1) - mean(sample.2)
}

# Ver distribución de diferencias de media bajo H0 ----

diff.means %>%
  as_tibble() %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 14, color = 'black')

# Cálculo del p-valor

mean(diff.means >= diff)